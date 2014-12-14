(ns fact)

(defn dispatcher
  [num]
  (cond
    (== num 0) 0
    (> num 0) num
    :else 0))

(defmulti factorial dispatcher)

(defmethod factorial 0
  [_]
  1)

(defmethod factorial :default
  [num]
  (* num (factorial (dec num))))