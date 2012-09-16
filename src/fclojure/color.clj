(ns fclojure.color "Utilities for colorizing output.")

(def colors
  (zipmap [:black :red :green :yellow :blue :magenta :cyan :white]
          (map (partial + 30) (range))))

(def codes
  {:prefix "\u001b["
   :suffix "m"
   :end "\u001b[m"})

(defn colorize [color s]
  (str (:prefix codes) (colors color) (:suffix codes) s (:end codes)))

(defn random-bright-color []
  (rand-nth (keys (dissoc colors :black :blue))))

(defn color-writer [color]
  (proxy [java.io.Writer] []
    (write [s]
      (.print System/out (if (string? s) (colorize color s) \space)))
    (flush [] (.flush System/out))))

(def rainbow-writer
  (proxy [java.io.Writer] []
    (write [s]
      (doseq [c s]
        (.print System/out (if (string? s) (colorize (random-bright-color) c) \space))))
    (flush [] (.flush System/out))))

(defmacro with-awesomeness [& exprs]
  `(binding [*out* rainbow-writer]
     ~@exprs))

(defmacro with-color [color & exprs]
  `(binding [*out* (color-writer ~color)]
     ~@exprs))