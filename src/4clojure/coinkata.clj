(ns 4clojure.coinkata)

(def change {2000 "$20"
             1000 "$10"
              500  "$5"
              100  "$1"
               25  "25c"
               10  "10c"
                5   "5c"
                1   "1c"})

(defn coin-kata [cents]
  "Returns number of coins to fulfill an integer amount.
   Thus, expects $10.98 as 1098"
  (loop [amt cents
         coins (sort > (keys change))
         acc []]
    (if (zero? amt)
      acc
      (let [coin (first coins)
            numcoins (int (/ amt coin))]
        (if (zero? numcoins)
          (recur amt (rest coins) acc)
          (recur (- amt (* coin numcoins))
                 (rest coins)
                 (conj acc coin numcoins)))))))

(defn dollar-kata [dollars]
  "Returns a map where keys are coins and values are numbers of coins"
  (->> (* (long 100) dollars)
       coin-kata
       (apply sorted-map-by >)))

(defn pretty-print-kata [kata]
  (doseq [[k v] kata]
    (printf "%3s - %d\n" (change k) v)))

;; ----------- Test ------------
(pretty-print-kata (dollar-kata 100829.87))

(pretty-print-kata (dollar-kata 0))




