(ns looping-is-recursion)

;; Write the function (power n k) that computes the
;; mathematical expression n^k
;; (power 2 2)  ;=> 4
;; (power 5 3)  ;=> 125
;; (power 7 0)  ;=> 1
;; (power 0 10) ;=> 0
(defn power [base exp]
  (loop [acc 1
         n base
         k exp]
    (if (zero? k)
      acc
      (recur (* acc n) n (dec k)))))

;; Compute the last element of a sequence.
;; (last-element [])      ;=> nil
;; (last-element [1 2 3]) ;=> 3
;; (last-element [2 5])   ;=> 5
(defn last-element [a-seq]
  (loop [last-elem nil
         a-seq a-seq]
    (if (seq a-seq)
      (recur (first a-seq) (rest a-seq))
      last-elem)))

;; Write the function (seq= a-seq b-seq) that compares two sequences for equality.
;; (seq= [1 2 4] '(1 2 4))  ;=> true
;; (seq= [1 2 3] [1 2 3 4]) ;=> false
;; (seq= [1 3 5] [])        ;=> false
;; (seq= [1 2 nil] [1 2])   ;=> false
(defn seq= [seq1 seq2]
  (loop [seq1 seq1
         seq2 seq2]
    (if (and (empty? seq1) (empty? seq2))
      true
      (if (and (seq seq1) (seq seq2) (= (first seq1) (first seq2)))
        (recur (rest seq1) (rest seq2))
        false))))

;; Implement the function (find-first-index [predicate seq]) that returns the
;; first index in seq for which predicate returns true, or nil if no such index exists.
;; (find-first-index zero? [1 1 1 0 3 7 0 2])                    ;=> 3
;; (find-first-index zero? [1 1 3 7 2])                          ;=> nil
;; (find-first-index (fn [n] (= n 6)) [:cat :dog :six :blorg 6]) ;=> 4
;; (find-first-index nil? [])                                    ;=> nil
(defn find-first-index [pred a-seq]
  (loop [pred pred
         a-seq a-seq
         index 0]
    (if (empty? a-seq)
      nil
      (if (pred (first a-seq))
        index
        (recur pred (rest a-seq) (inc index))))))

;; Implement the function (avg a-seq) that computes the average of a sequence.
;; (avg [1 2 3])   ;=> 2
;; (avg [0 0 0 4]) ;=> 1
;; (avg [1 0 0 1]) ;=> 1/2 ;; or 0.5
;; (avg ())
(defn avg [a-seq]
  (loop [a-seq a-seq
         acc 0
         cnt 0]
    (if (empty? a-seq)
      (if (pos? cnt)
        (/ acc cnt)
        nil)
      (recur (rest a-seq) (+ acc (first a-seq)) (inc cnt)))))

;; Write the function (parity a-seq) that takes in a sequence and returns a set
;; of those elements that occur an odd number of times in the sequence.
;; (parity [:a :b :c])           ;=> #{:a :b :c}
;; (parity [:a :b :c :a])        ;=> #{:b :c}
;; (parity [1 1 2 1 2 3 1 2 3 4]) ;=> #{2 4}

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [result #{}
         a-seq a-seq]
    (if (empty? a-seq)
      result
      (recur
        (toggle result (first a-seq))
        (rest a-seq)))))

;; Write the function (fast-fibo n) that computes the nth fibonacci number using
;; loop and recur. Do not use recursion.
;; Hint: to avoid recursion, you need to keep track of Fn−1 and Fn in the loop.
;; F0 = 0
;; F1 = 1
;; Fn = Fn-1 + Fn-2
;; (fast-fibo 0) ;=> 0
;; (fast-fibo 1) ;=> 1
;; (fast-fibo 2) ;=> 1
;; (fast-fibo 3) ;=> 2
;; (fast-fibo 4) ;=> 3
;; (fast-fibo 5) ;=> 5
;; (fast-fibo 6) ;=> 8
;; (fast-fibo 7)
(defn fast-fibo [n]
  (cond (neg? n) nil
        (= n 0) 0
        (= n 1) 1
        :else (loop [n-current 2
                     fn-1 1
                     fn-2 0]
                (if (> n-current n)
                  fn-1
                  (recur
                    (inc n-current)
                    (+ fn-1 fn-2)
                    fn-1)))))

;; Write the function (cut-at-repetition a-seq) that takes in a sequence and
;; returns elements from the sequence up to the first repetition.
;; (cut-at-repetition [1 1 1 1 1])
;=> [1] doesn't have to be a vector, a sequence is fine too
;; (cut-at-repetition [:cat :dog :house :milk 1 :cat :dog])
;=> [:cat :dog :house :milk 1]
;; (cut-at-repetition [0 1 2 3 4 5])
;=> [0 1 2 3 4 5]
(defn cut-at-repetition [a-seq]
  (loop [acc []
         uniques #{}
         a-seq a-seq]
    (if (empty? a-seq)
      acc
      (if (contains? uniques (first a-seq))
        acc
        (recur (conj acc (first a-seq))
               (conj uniques (first a-seq))
               (rest a-seq))))))
