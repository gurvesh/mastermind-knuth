(ns mastermind-knuth.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Welcome. Please start a repl session, and call (init) to start."))

;; Most of the logic here is similar to the "brute-force" program. We init the game, but now we score each remaining guess. A min-max optimisation is involved.
;; Be warned it takes much longer than the other program. The first guess can take about a minute to run. Next ones will be quicker.

(def color-list
  '[red, green, orange, yellow, blue, purple])

(def current-guess
  (atom '()))

(def possibles
  (atom '()))

;; As per the algorithm, now we need to keep track of the current possibilities, and all possibilities, which will both change. So we use a bit of different init this time.

(def all-possibles
  (atom '()))

(defn init []
  (do 
    (reset! possibles (for [a color-list
                            b color-list
                            c color-list
                            d color-list]
                        (vector a b c d)))
    (reset! all-possibles (deref possibles))
    (println "game on!")
    (println "Please provide the hint in the form - (guess '(x y)), where x is the number of exact matches, and y the number of non-exact matches")
    (reset! current-guess '[red red green green])))

(defn compare-exact [code test]
  (count (filter true? (map = code test))))

(defn compare-approx [code test]
  (apply + (vals (merge-with min 
                           (select-keys (frequencies code) test)
                           (select-keys (frequencies test) code)))))

(defn compare-code [code test]
  (list (compare-exact code test)
        (- (compare-approx code test) (compare-exact code test))))

(defn filter-color [possibles test result]
  (filter #(= result
              (compare-code % test))
          possibles))

;; From Wikipedia - http://en.wikipedia.org/wiki/Mastermind_%28board_game%29. For each possible guess, that is, any unused code of the 1296 not just those in S, calculate how many possibilities in S would be eliminated for each possible colored/white peg score. The score of a guess is the minimum number of possibilities it might eliminate from S. From the set of guesses with the maximum score select one as the next guess.

(defn subscore [possibles test result]
  (- (count possibles) 
     (count (filter-color possibles test result))))

(defn score [possibles test]
  (reduce min (for [x (range 4)
                    y (range 4)
                    :when (and (not= x 4)
                               (<= (+ x y) 4))]
                (subscore possibles test (list x y)))))

(defn score-each-and-sort [all-possibles possibles]
  (sort-by val > (zipmap all-possibles
                         (for [test all-possibles]
                           (score possibles test)))))

;; The guess function will now pick the first one as per highest score. 

(defn guess [feedback]
  (if (= feedback '(4 0)) "Thanks for playing"
      (do 
        (reset! possibles (filter-color (deref possibles)
                                        (deref current-guess)
                                        feedback))
        (cond (= (count (deref possibles)) 1) (println "I have  read your mind. Final guess" (deref possibles))
              (= (count (deref possibles)) 0) "Looks like YOU made a mistake - puny human! Therefore you lose! Mistakes will not be tolerated. Just kidding - start again with (init). Intermediate states are not stored, so everything resets to 0 - sorry."
              :else
              (do
                (reset! all-possibles (filter #(not= (deref current-guess) %) (deref all-possibles)))
                (reset! current-guess (key (first (score-each-and-sort (deref all-possibles) (deref possibles))))))))))
