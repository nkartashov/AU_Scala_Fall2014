(defn call-twice
  [f x]
  (f x)
  (f x))

(defn fun2
  [path]
  (let [text (slurp path)]
    (println text)
    text)
  )

(def cube-anonymous (fn [x] (* x x x)))

(defn fun4
  [seq1 seq2]
  (concat (reverse seq1) (reverse seq2)))

(defn fun5
  [elem input_seq]
  (some (fn [x] (== x elem)) input_seq))

(defn fun6
  [seq1 seq2]
  (let [zipped (map vector seq1 seq2)]
    (remove (fn [x] (apply == x)) zipped)))

(defn fun7
  [elem n]
  (map (fn [_] elem) (range n)))