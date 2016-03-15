(ns mathdice-solver.core
  (:gen-class)
  (:require [clojure.math.numeric-tower :as math
             :refer [expt]]))

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
      
(defn drop-nth
  "Removes the nth element from a collection."
  [sqn n]
  (concat (take n sqn) (drop (inc n) sqn)))

(defn all-orderings
  "Creates a set of all ways to re-order the input list."
  [sqn]
  (if (= 1 (count sqn))
      [sqn]
      (map (fn [n] (map #(cons (nth sqn n) %) (all-orderings (drop-nth sqn n))))
           (range (count sqn)))))

(defn cons-to-all
  "Exactly what it says on the tin."
  [x, sqns]
  (map #(cons x %) sqns))

(defn better-all-orderings
  "Creates a set of all ways to re-order the input list."
  [sqn]
  (if (empty? sqn)
      #{}))
      

(defn combine-two
  "Creates a list of possible ways to combine two numbers, given a list of functions."
  [functions n1 n2]
  (map #(% n1 n2)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println (str "Here, have a mathdice problem: " (generate-random-problem))))
