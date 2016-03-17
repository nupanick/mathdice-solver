(ns mathdice-solver.core
  (:gen-class)
  (:require [clojure.math.numeric-tower :as math
             :refer [expt]]))

;;;
;;; This section deals with producing mathdice problems.
;;;

(defn roll-d
  "Produces a lazy sequence of random integers from 1 to n."
  [n]
  (repeatedly #(inc (rand-int n))))

(defn generate-random-problem
  "Generates a random MathDice problem in the format (target die1 die2 die3)."
  []
  (cons (apply * (take 2 (roll-d 12)))
        (take 3 (roll-d 6))))

;;;
;;; This section contains functions used to generate potential solutions.
;;;

(defn drop-nth
  "Accepts a sequence, and returns that sequence with the nth element removed."
  [sqn n]
  (concat (take n sqn) (drop (inc n) sqn)))

(defn drop-each
  "Accepts a sequence and returns a list of all sequences created by removing one element from it."
  [sqn]
  (map #(drop-nth sqn %)
       (range (count sqn))))

(defn cons-to-all
  "Accepts a value and a collection of sequences. Prepends the value to each sequence."
  [x, sqns]
  (map #(cons x %) sqns))

(defn permute-n
	"Produces a collection of all possible unique sequences of n-many elements of the input sequence."
	[sqn n]
	(if (= 1 n)
	    (set (map #(list %) sqn))
	    (reduce into #{}
	            ;; Try each possible starting element, followed by all possible
	            ;; sequences of length n-1.
	            (map #(cons-to-all %1 (permute-n %2 (- n 1)))
	                 sqn
	                 (drop-each sqn)))))

(defn pick-two
	"Lists ways to pick two elements from the set. Special case of permute-n."
	[sqn]
	(permute-n sqn 2))

(defn permute
	"Lists ways to rearrange the set. Special case of permute-n."
	[sqn]
	(permute-n sqn (count sqn)))

;;;
;;; Construct all the possible expressions.
;;;

;; List of legal ways to combine two numbers in MathDice.
(def math-functions ['+ '- '* '/ 'expt])

(defn generate-expressions [functions [x y]]
	(into #{} (map #(list % x y) functions)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println (str "Here, have a mathdice problem: " (generate-random-problem))))
