(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [ans base n]
                 (if (zero? n)
                   ans
                   (recur (* ans base) base (dec n))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (loop [s a-seq
         prev nil]
    (if (empty? s) prev (recur (rest s) (first s)))))

(defn seq= [seq1 seq2]
  (let [helper (fn [seq1 seq2 n]
                 (cond
                  (and (empty? seq1) (empty? seq2)) true
                  (or (empty? seq1) (empty? seq2)) false
                  (= (first seq1) (first seq2)) (recur (rest seq1) (rest seq2) n)
                  :else false))]
(helper (into [] seq1) (into [] seq2) (count seq1))))


(defn find-first-index [pred a-seq]
  (loop [i 0
         s a-seq]
    (cond
     (empty? s) nil
     (pred (first s)) i
     :else (recur (inc i) (rest s)))))


(defn avg [a-seq]
  (if (empty? a-seq)
    ""
    (loop [count 0
         s a-seq
         sum 0]
    (cond
     (empty? s) (/ sum count)
     :else (recur (inc count) (rest s) (+ sum (first s)))))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem))
  )


(defn parity [a-seq]
  (loop [set #{}
         s a-seq]
    (if (empty? s)
      set
      (recur (toggle set (first s)) (rest s)))))

(defn fast-fibo [n]
  (cond
   (= n 0) 0
   (= n 1) 1
   :else (loop [n1 1
           n2 0
           i 2]
      (if (= i n)
        (+ n1 n2)
        (recur (+ n1 n2) n1 (inc i))))))

(defn cut-at-repetition [a-seq]
  (loop [set #{}
         s a-seq
         dmp []]
    (let [elem (first s)]
      (cond
     (empty? s) dmp
     (contains? set elem) dmp
     :else (recur (conj set elem) (rest s) (conj dmp elem))))))


