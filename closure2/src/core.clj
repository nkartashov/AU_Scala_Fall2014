(ns core)

(defmacro my-or
  ([] nil)
  ([x] x)
  ([x & xs]
    `(if ~x
       true
       (my-or ~@xs))
    )
  )

(defn split-names-values
  [bindings]
  (cons (take-nth 2 bindings)
        (cons (take-nth 2 (rest bindings)) nil))
  )

(defmacro split-names-values-macro
  [input]
  (split-names-values input))

(defmacro my-fn
  [vars & body]
  `(fn ~vars ~@body))


(println (macroexpand '(my-fn [a b] (println a) (println b))))

(def x (my-fn [a] (println a)))
(x 10)

;((my-fn [a] ((println a) (println 100))) 10)
;(println (my-fn [a] ((println a) (println 100))))



;(defmacro my-let
;  [bindings & body]
;  `((fn
;      (vec (first (split-names-values-macro ~bindings)))
;      (do ~@body)) (second (split-names-values-macro ~bindings)))
;  )