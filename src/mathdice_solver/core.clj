(ns mathdice-solver.core
  (:gen-class)
  (:require [clojure.math.numeric-tower :as math]))

;; List of legal ways to combine two numbers in MathDice.
(def math-functions [+, -, *, /, math/expt])

(defn roll-d
  "Produces a lazy sequence of random integers from 1 to n."
  [n]
  (repeatedly #(inc (rand-int n))))

(defn generate-random-problem
  "Generates a random MathDice problem in the format (target die1 die2 die3)."
  []
  (cons (apply * (take 2 (roll-d 12)))
        (take 3 (roll-d 6))))

(defn all-orderings
  "Creates an set of all unique ways to re-order the inputs."
  [& args]
  (if (= (count args) 1)
      #{[args]}))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println (str "Here, have a mathdice problem: " (generate-random-problem))))
