(ns macros)

(defmacro my-or
  ([] nil)
  ([x] x)
  ([x & xs]
    `(if ~x
       true
       (my-or ~@xs))))

(defmacro my-fn
  [vars & body]
  `(fn ~vars ~@body))

(defmacro my-let
  [bindings & body]
  `((my-fn ~(vec (take-nth 2 bindings)) ~@body) ~@(take-nth 2 (rest bindings))))

(my-let [a 2] (println a))
(my-let [c 3 a 2 b 3] (println (+ a b c)) (println (* b c a)))
