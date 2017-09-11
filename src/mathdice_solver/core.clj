(ns mathdice-solver.core
  (:gen-class)
  (:require [clojure.math.numeric-tower :as math
             :refer [expt]]))

;;;
;;; This section deals with producing mathdice problems.
;;;

;; List of legal ways to combine two numbers in MathDice.
(def math-functions ['+ '- '* '/ 'expt])

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

;;;
;;; Construct all the possible expressions.
;;;

(defn pick-two
  "Produces all ways to pick two elements from a sequence, as well as the remainder after each one."
  [sqn]
  (let [n (count sqn)]
       (set (for [i (range n)
                  :let [x (nth sqn i)
                        sqn-i (drop-nth sqn i)]
                  j (range (dec n))
                  :let [y (nth sqn-i j)
                        sqn-i-j (drop-nth sqn-i j)]]
                 (list x y sqn-i-j)))))

(defn generate-expressions
  "Generate all possible expressions using the given two-operation operators on the given operands."
  [operators operands]
  (if (-> operands count (< 2))
      (set operands)
      (reduce into #{}
              (for [[x y more] (pick-two operands)
                    f operators]
                  (generate-expressions
                    operators
                    (conj more (list f x y)))))))

(defn safe-eval
  "Return nil on divide by zero error."
  [expression]
  (try (eval expression)
       (catch ArithmeticException e
              nil)))

(defn unique-outputs
  "Create a table of output values and one expression that evaluates to each one."
  [expressions]
  (loop [output-map {}
         [test-exp & more-exps :as expressions] (seq expressions)]
       (if (empty? expressions)
           output-map
           (let [v (safe-eval test-exp)]
                ;; Remember, v will be nil if test-exp includes division by zero.
                (if v
                    (recur
                      (assoc output-map v test-exp)
                      more-exps)
                    (recur
                      output-map
                      more-exps))))))

(defn distance
  "Returns absolute distance between the two numbers."
  [x y]
  (if (< x y)
      (- y x)
      (- x y)))

(defn best-guess
  "Returns the element of guesses with the smallest distance to the target."
  [guesses target]
  (let [distance-list (map #(distance % target) guesses)
        score-table (zipmap distance-list guesses)
        best-score (apply min distance-list)]
      (get score-table best-score)))

(defn find-solution
  "Given a mathdice problem, finds the best possible solution. Outputs both the number reached and the expression that produced it."
  [[target & dice]]
  (let [solution-list (unique-outputs (generate-expressions math-functions dice))
        answer (best-guess (keys solution-list) target)]
      (list answer (get solution-list answer))))

;;;
;;; The remainder of the code will be i/o oriented! Let's talk to the user.
;;;

(defn infixify
  "Converts an nested prefix expression into an infix one, for readability."
  [expression]
  (if (coll? expression)
      ;; swap the function into the middle, then recurse on the operands.
      (list
        (infixify (second expression))
        (first expression)
        (infixify (nth expression 2)))
      ;; non-nested expressions should just be a flat number
      expression))

(defn solution-str
  "Converts a solution object to a display string."
  [[result process]]
  (str result
       " = "
       (infixify process)))

(defn solve
  "Simple solution finder for interactive mode."
  [& args]
  (-> args
      find-solution
      solution-str))

(defn -main
  "I don't do a whole lot ... yet."
  ([] (-main "interactive"))
  ([arg & others]
    (if (= arg "solve") nil)
    ;solvey code
    (if (= arg "generate")
        (generate-random-problem))
    (if (= arg "interactive") nil)
    ; interactey code
  ))

