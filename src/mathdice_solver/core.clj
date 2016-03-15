(ns mathdice-solver.core
  (:gen-class)
  (:require [clojure.math.numeric-tower :as math]))

;; List of legal ways to combine two numbers in MathDice.
(def math-functions [+, -, *, /, math/expt])

(defn generate-random-problem []
  "Generates a random MathDice problem with a target and three key numbers."
  (cons (apply * (take 2 (repeatedly #(inc (rand-int 12)))))
        (map inc (take 3 (repeatedly #(inc (rand-int 6)))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println (str "Here, have a mathdice problem: " (generate-random-problem))))
