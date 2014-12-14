(ns transact)

(def first-account (ref 0))
(def second-account (ref 100))

(defn print-accounts
  []
  (print "First account: " @first-account "\n")
  (print "Second account: " @second-account "\n"))

(print-accounts)

(defn transfer [amount from to]
  (dosync
    (alter from - amount)
    (alter to + amount)))

(transfer 100 second-account first-account)

(print-accounts)