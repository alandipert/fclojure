(ns fclojure.intro
  (require [clojure.java.io :as io]
           [fclojure.color :as color]))

(defn intro
  "Prints out an awesome-looking colorized version of the logo."
  []
  (color/with-awesomeness
    (-> (io/resource "logo.txt") slurp println)))