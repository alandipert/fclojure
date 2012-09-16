(ns fclojure
  (:refer-clojure :exclude [eval resolve destructure])
  (require [fclojure.color :as color]
           [fclojure.intro :as intro]
           [clojure.java.io :as io]
           [clojure.set :as set])
  (use [clojure.pprint :only [pprint]]
       [fclojure.destructure :only [destructure]]
       clojure.repl)
  (:gen-class))

;;; Debugging

(defn error
  "msg can be a string or an exception."
  [msg]
  (color/with-color :red
    (if (instance? Exception msg)
      (.printStackTrace msg)
      (println msg))))

;;; Environment and Scopes

(def required-functions
  "These are functions from Clojure that must be referred."
  '#{nth nthnext})

(def vars-to-import
  "Vars from clojure.core to refer into the global fclojure environment."
  (set/union '#{* + - < = > assoc conj cons first list list* println rest second seq vector}
             required-functions))

(defn init-globals
  "Returns an atom containing a few choice Vars referred from
  clojure.core."
  []
  (->> vars-to-import
       (map (juxt identity (partial ns-resolve (the-ns 'clojure.core))))
       (into {})
       atom))

(def ^:dynamic *globals* (init-globals))

(defn find-nearest-fn
  "Starting with env, walks up the scope chain until it finds the
  lexically-nearest function."
  [env]
  (if env
    (if-let [nearest (:f env)]
      nearest
      (recur (:parent env)))))

(defn mkenv
  "Returns a new environment, which is a map of:
   :f - The function object that provides this scope.
   :parent - The parent scope, if any.
   :bindings - Map of symbols to values representing the lexical environment."
  ([] (mkenv nil nil {}))
  ([f parent bindings]
     {:f f
      :parent parent
      :bindings bindings}))

(defn resolve-lexical
  "Looks for name in the local lexical environment, and returns
  not-found if it is not found."
  [env name not-found]
  (if env
    (let [val (get-in env [:bindings name] not-found)]
      (if (= not-found val)
        (recur (:parent env) name not-found)
        val))
    not-found))

(defn resolve
  "Looks for name in both the lexical and global environments and
  returns nil if it's not found."
  [env name]
  (let [not-found ::not-found]
    (let [lexical (resolve-lexical env name not-found)]
      (if (= lexical ::not-found)
        (let [global (get @*globals* name not-found)]
          (if (= global ::not-found)
            (error (format "symbol '%s' is undefined." name))
            global))
        lexical))))

;;; Evaluation

(declare eval)

(defn build-bindings
  "Returns a map of bindings after destructuring."
  [env params args]
  (let [destructures (partition 2 (destructure [(vec params) (list 'quote (vec args))]))
        build-bindings (fn [new-bindings [name val]]
                         (assoc new-bindings name
                                (eval (update-in env [:bindings] merge new-bindings) val)))]
    (reduce build-bindings {} destructures)))

(defn mkfn
  "Returns a function object. Does not evaluate its arguments; apply*
  might have already."
  [env [params & body]]
  (fn self [& args]
    (let [bindings (build-bindings env params args)
          locals (mkenv self env bindings)]
      (peek (mapv (partial eval locals) body)))))

(defn mklet
  "Generates a function-based equivalent of the specified let binding
  and evaluates it."
  [env [bindings & body]]
  (let [rbindings (reverse (partition 2 bindings))
        nest (fn [expr [left right]]
               `((~'fn [~left] ~expr) ~right))
        innermost (let [[left right] (first rbindings)]
                    `((~'fn [~left] ~@body) ~right))]
    (eval env (reduce nest innermost (rest rbindings)))))

(defn apply*
  "Applies op to args in env, forgoing argument evaluation if op
  has ::fexpr metadata."
  [op env args]
  (let [operator (eval env op)]
    (apply operator (if (or (:ns (meta operator))
                            (not (::fexpr (meta operator))))
                      (mapv (partial eval env) args)
                      args))))

(defn operation-form? [x]
  (boolean (or (list? x)
               (instance? clojure.lang.Cons x)
               (instance? clojure.lang.ChunkedCons x))))

(defn eval
  "Evaluates form, optionally inside env."
  ([form] (eval nil form))
  ([env form]
     (cond

      (operation-form? form)
      (let [[op & args] form]
        (case op
          def     (do (swap! *globals* assoc (first args) (eval env (second args)))
                      (first args))
          quote   (first args)
          ffn     (with-meta (mkfn env args) {::fexpr true})
          fn      (mkfn env args)
          let     (mklet env args)
          eval    (eval env (eval env (first args)))
          if      (let [[pred then else] args]
                    (eval env (if (eval env pred) then else)))
          recur   (if-let [f (find-nearest-fn env)]
                     (apply f (map (partial eval env) args))
                     (throw (Exception. (format "No binding above %s found." (pr-str form)))))
          (apply* op env args)))

      (vector? form)
      (mapv (partial eval env) form)

      (map? form)
      (into {} (map (fn [[k v]] [k (eval env k)]) form))

      (symbol? form)
      (resolve env form)

      :else form)))

(defn repl
  "fclojure Read-Eval-Print loop."
  []
  (binding [*globals* (init-globals)]
    (println "Type :q to quit.")
    (loop []
      (print "> ")
      (flush)
      (when-let [form (read)]
        (when (not= form :q)
          (try
            (color/with-color :green
              (println (-> `'~form clojure.core/eval eval)))
            (catch Exception e (error e)))
          (recur))))))

(defmacro interpret
  "Interprets body as FClojure and returns the value of the last expression."
  [& body]
  `(binding [*globals* (init-globals)]
     (peek (mapv eval '~body))))

(defn read-file
  "rdr must be a java.io.PushbackReader. Returns a lazy sequence of
  Clojure forms in file f."
  [rdr]
  (lazy-seq
   (when-let [form (read rdr false ::eof)]
     (if (not= form ::eof)
       (cons form (read-file rdr))))))

(defn eval-file
  "Evaluates the FClojure file, string or URI f."
  [f]
  (with-open [rdr (java.io.PushbackReader. (io/reader f))]
    (binding [*globals* (init-globals)]
      (doseq [expr (read-file rdr)]
        (eval expr)))))

(defn -main [& args]
  (if (seq args)
    (case (first args)
      "-f" (eval-file (second args)))
    (do (intro/intro) (repl))))

(comment
  (interpret

   (def defmacro
     (ffn [name params & body]
          (eval (list 'def name
                      (list 'eval
                            (list 'ffn params
                                  (cons 'eval body)))))))

   (defmacro defn [name params body]
     (list 'def name
           (list 'fn params body)))

   (defn inc [n] (+ n 1))

   (def do*
     (ffn [res [expr & more]]
          (if expr
            (recur (eval expr) more)
            res)))

   (defmacro do [& body]
     (list 'do* nil body))

   (def if-let
     (ffn [[name test] then else]
          (let [res (eval test)]
            (eval (if res
                    (list 'let (vector name res) then)
                    else)))))

   (defn map [f [x & xs]]
     (if x (cons (f x) (map f xs))))

   (def quasiquote*
     (ffn [x]
          (if (seq x)
            (if (= (first x) 'unquote)
              (eval (second x))
              (recur (rest x)))
            x)))
   )
  )
