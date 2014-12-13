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


