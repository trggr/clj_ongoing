(ns 4clojure.core)


;;
;; Solutions to 4clojure.com riddles
;;
(fn [coll n]
  "Find nth element of a sequence"
  (loop [i 0 c coll]
    (if (= i n)
      (first c)
      (recur (inc i) (rest c)))))



(fn [coll]
   "Returns the total number of elements in a sequence."
   (loop [n 0 c coll]
     (if-not c
       n
       (recur (inc n) (next c)))))


(fn [coll]
  "Reverses a sequence"
  (loop [acc '() c coll]
    (if-not c
      acc
      (recur (conj acc (first c))
         (next c)))))

(fn [coll]
  "Returns the sum of a sequence of numbers"
  (reduce + coll))


(fn [coll]
  "Returns only the odd numbers from a sequence"
  (filter odd? coll))

(fn [n]
  "Returns the first Fibonacci numbers"
  (loop [i 0 fib []]
    (if (= i n)
      fib
      (recur (inc i)
	     (cond (= i 0) (conj fib 1)
		   (= i 1) (conj fib 1)
		   :else (conj fib (+ (fib (- i 1))
				      (fib (- i 2)))))))))

(fn [coll]
  "Returns true if coll is palindrome"
  (let [s (seq coll)]
    (= (reverse s) s)))


(fn [coll]
  "Flattens a sequence"
  (letfn [(f [acc coll]
	     (println (format "acc=%s, coll=%s" acc coll))
	     (let [e (first coll)]
	       (cond (nil? e)  acc
		     (coll? e) (f (f acc e) (rest coll))
		     :else (f (conj acc e) (rest coll)))))]
    (reverse (f '() coll))))


(fn [x]
  "Takes a string and returns a new string containing only the capital letters"
  (apply str (filter #(Character/isUpperCase %) (seq x))))


(fn [coll]
  "Removes consecutive duplicates from a sequence"
  (loop [prev nil s coll acc []]
    (let [cur (first s)]
      (cond (nil? cur) acc
        (= cur prev) (recur prev (rest s) acc)
	:else (recur cur (rest s) (conj acc cur))))))

(fn [coll]
  "Packs consecutive duplicates into sub-lists"
  (partition-by identity coll))

(fn [coll]
  "Duplicates each element of a sequence"
  (loop [acc [] c coll]
    (let [p (first c)]
      (if (nil? p)
	acc
	(recur (conj acc p p) (rest c))))))

(defn dup-n [coll n]
  "Duplicates each element of a sequence n times"
  (loop [acc [] c coll]
    (let [p (first c)]
      (if (nil? p)
	acc
	(recur (apply conj acc (for [i (range 0 n)] p)) (rest c))))))

(fn [coll n]
  "Duplicates each element of a sequence n times"
  (loop [acc [] c coll]
    (let [p (first c)]
      (if (nil? p)
	acc
	(recur (apply conj acc (for [i (range 0 n)] p)) (rest c))))))

(fn [n m]
  "Creates a list of all integers in a given range"
  (loop [i n acc []]
    (if (= i m)
      acc
      (recur (inc i) (conj acc i)))))

;; "Let in Be" solution to bind x y z
[z 1 x 7 y 3]

;; Regular expressions
"ABC"

(fn [& args]
  "Returns a maximum among its arguments"
  (reduce #(if (> %1 %2) %1 %2) (first args) args))

(fn [a b]
  "Interleaves two sequences"
  (flatten (map #(list %1 %2) a b)))

;; Not a solution, but useful function
(defn map2
  "Takes fn and sequence. And calls fn upon pair elements of a sequence;
   fn should accept two arguments"
  [f coll]
  (loop [acc []
	 s coll]
    (let [e (first s) g (second s)]
      (cond (nil? e) acc
	    (nil? g) (recur (conj acc e) (rest s))
	    :else (recur (conj acc (f e g))
			 (rest s))))))
	
(fn [val coll]
  "Separates the items of a sequence by an arbitrary value"
  (if (empty? coll)
    coll
    (reduce #(conj %1 val %2) [(first coll)] (rest coll))))

(fn [coll n]
  "drops every n-th item from a sequence"
  (loop [acc [] s coll i 1]
    (let [e (first s)]
      (if-not e
	acc
	(recur (if (= 0 (mod i n)) acc (conj acc e))
	       (rest s)
	       (inc i))))))

(defn tmp [n]
  "Calculates factorial"
  (loop [acc 1 i 1]
    (if (> i n)
      acc
      (recur (* acc i) (inc i)))))

(fn [n]
  "Calculates factorial"
  (loop [acc 1 i 1]
    (if (> i n)
      acc
      (recur (* acc i) (inc i)))))

(defn tmp [coll n]
  "Reverses the interleave process into n number of subsequences"
  (loop [acc [] s coll cur 0]
    (let [e (first s)]
      (if-not e
	acc
	(recur (assoc acc cur
		      (conj (if (> cur (dec (count acc))) [] (acc cur))
		       e))
	       (rest s)
	       (mod (inc cur) n))))))

(fn [coll n]
  "Reverses the interleave process into n number of subsequences"
  (loop [acc [] s coll cur 0]
    (let [e (first s)]
      (if-not e
	acc
	(recur (assoc acc cur
		      (conj (if (> cur (dec (count acc))) [] (acc cur))
		       e))
	       (rest s)
	       (mod (inc cur) n))))))

(fn [n v]
  "Rotates sequence n times. If n < 0 rotates right, if n > 0 rotates left"
  (let [m (dec (count v))
	step (cond (zero? n) 0
		   (< n 0) 1
		   :else -1)]
    (loop [i n acc (if (vector? v) v (vec v))]
      (if (zero? i)
	acc
	(recur (+ i step)
	       (if (< n 0)
		 (vec (list* (last acc) (subvec acc 0 m)))
		 (conj (subvec acc 1) (first acc))))))))

;; Solution to "Intro to iterate"
'(1 4 7 10 13)


;; Solution to "Contain yourself"
4

;; Solution to "Intro to some"
6

(fn [f]
  "Flips the order of arguments of f"
  (fn [x y]
    (f y x)))

(fn [n a]
  "Split a sequence into two parts"
  (loop [acc [] s a i n]
    (let [e (first s)]
      (if (zero? i)
	(vector acc (vec s))
	(recur (conj acc e)
	       (rest s)
	       (dec i))))))

;; Solution to "Split by Type"
(fn [a]
  "Split a sequence by type"
  (vals (group-by type a)))

;; Solution to "Intro to Destructuring"
[c e]

;; Solution to "Advanced Destructuring"
'(1 2 3 4 5)

(fn [x]
  "Find a longest increasing sub-sequence"
  (letfn [(compete
	   [wannabe established]
	   (if (> (count wannabe) (count established))
	     wannabe established))]
    (loop [candidate [] champ [] a x]
      (let [e (first a)]
	(if-not e
	  (let [rc (compete candidate champ)] (if (> (count rc) 1) rc []))
	  (if (or (empty? candidate) (<= e (peek candidate)))
	    (recur [e]  (compete candidate champ)  (rest a))
	    (recur (conj candidate e)  champ  (rest a))))))))

(fn [n x]
  "Returns a sequence of lists of n items each.
   Lists of less than x items not returned"
  (loop [acc [] a x i 0 part []]
    (let [e (first a)]
      (if-not e
	(if (= n (count part))
	  (conj acc part)
	  acc)
	(if (= i n)
	  (recur (conj acc part)
		 (rest a)
		 1
		 [e])
	  (recur acc
		 (rest a)
		 (inc i)
		 (conj part e)))))))

(defn tmp [x]
  "Returns a map containing the number of occurences of each distinct item in a sequence"
  (loop [acc {} a (group-by identity x)]
    (let [e (first a)]
      (if-not e
	acc
	(recur (assoc acc (key e) (count (val e)))
	       (rest a))))))

(fn [x]
  "Returns a map containing the number of occurences of each distinct item in a sequence"
  (loop [acc {} a (group-by identity x)]
    (let [e (first a)]
      (if-not e
	acc
	(recur (assoc acc (key e) (count (val e)))
	       (rest a))))))


; Write a function which, given a key and map, returns true iff
; the map contains an entry with that key and its value is nil.

(def contains-and-nil?
  (fn [k m]
    (if (not (contains? m k))
      false
      (if (= nil (k m)) true false))))

(defn contains-and-nil? [k m]
  (if (not (contains? m k))
      false
      (if (= nil (k m))
        true
        false)))

(true? (contains-and-nil? :a {:a nil :b 2}))
(false? (contains-and-nil? :b {:a nil :b 2}))
(false? (contains-and-nil? :c {:a nil :b 2}))

; # 156  - given keys, create a new map map with a default value
(defn mapk [default ks]
  (zipmap ks (repeat (count ks) default)))

(fn [default ks]
  (zipmap ks (repeat (count ks) default)))

; # 166
; For any orderable data type it's possible to derive all of the basic comparison
; operations (<, ≤, =, ≠, ≥, and >) from a single operation (any operator but = or ≠ will work).
; Write a function that takes three arguments, a less than operator for the data and
; two items to compare. The function should return a keyword describing the relationship
; between the two items. The keywords for the relationship between x and y are as follows:
; x = y → :eq
; x > y → :gt
; x < y → :lt

(defn f1 [op x y]
  (cond
    (op x y) :lt
    (= x y)  :eq
    (not (op x y)) :gt))

(fn [op x y]
  (cond
    (op x y) :lt
    (op y x) :gt
    (and (not (op x y))
         (not (op y x))) :eq))

;; #145.

(for [x (range 40)
      :when (= 1 (rem x 4))]
  x)

;; #99 Write a function which multiplies two numbers and returns the result as a sequence of its digits.
;(= (__ 1 1) [1])
;(= (__ 99 9) [8 9 1])
;(= (__ 999 99) [9 8 9 0 1])

(fn [x y]
  (map #(- (int %) 48) (seq (str (* x y)))))

; #122
; Convert a binary number, provided in the form of a string, to its numerical value.
; (= 0     (__ "0"))
; (= 7     (__ "111"))
; (= 8     (__ "1000"))
; (= 9     (__ "1001"))
; (= 255   (__ "11111111"))
; (= 1365  (__ "10101010101"))
; (= 65535 (__ "1111111111111111"))

(fn [s]
  (reduce
    (fn [acc d] (+ (* 2 acc) d))
    0
    (map #(- (int %) 48) (seq s))))

; #107 Given a positive integer n, return a function (f x) which computes x**n
; (= 256 ((__ 2) 16),  ((__ 8) 2))
; (= [1 8 27 64] (map (__ 3) [1 2 3 4]))
; (= [1 2 4 8 16] (map #((__ %) 2) [0 1 2 3 4]))

(fn [n]
  (fn [x]
    (loop [acc 1 i n]
      (if (= i 0)
        acc
        (recur (* x acc) (dec i))))))


; #135
; infix calculator

(fn [x op y & xs]
  (loop [acc (op x y) r xs]
    (if (empty? r)
      acc
      (let [[op2 y2 & more] r]
        (recur (op2 acc y2) more)))))


(infix 2 + 3)
(infix 2 + 3 - 4)
(infix 2 + 3 - 4 * 2)
(infix 2 + 3 - 4 * 2 / 0.5 * 20)

; #143 Create a function that computes the dot product of two sequences.
; You may assume that the vectors will have the same length.

(fn [c1 c2] (reduce + (map * c1 c2)))


; #128 Recognize cards

  (fn [card]
  (let [suites {:S :spade :H :heart :D :diamond :C :club}
        ranks  {:2 0  :3 1  :4 2  :5 3
                :6 4  :7 5  :8 6  :9 7
                :T 8  :J 9  :Q 10 :K 11
                :A 12}
         [i j] (map #(keyword  (str %)) (seq card))]
    {:suit (suites i) :rank (ranks j)}))

; #118 reimplement function map

(fn ! [f coll]
  (lazy-seq
    (if coll
      (cons (f (first coll)) (! f (next coll))))))


; #105 Given an input sequence of keywords and numbers, create a map such that each key in
; the map is a keyword, and the value is a sequence of all the numbers (if any)
; between it and the next keyword in the sequence.
; (= {} (__ []))
; (= {:a [1]} (__ [:a 1]))

(fn [m]
  (second
    (reduce
      (fn [[slot acc] x]
        (if (keyword? x)
          [x    (assoc acc x [])]
          [slot (assoc acc slot (conj (acc slot) x))]))
      [nil {}]
      m)))

; # 102 When working with java, you often need to create an object with fieldsLikeThis,
; but you'd rather work with a hashmap that has :keys-like-this until it's time to convert.
; Write a function which takes lower-case hyphen-separated strings and converts them
; to camel-case strings.
;
;(= (__ "something") "something")
;(= (__ "multi-word-key") "multiWordKey")
;(= (__ "leaveMeAlone") "leaveMeAlone")

(fn [w]
   (apply str (second
          (reduce
            (fn [[flag acc] x]
              (if (= \- x)
                [true acc]
                [false (if flag
                  (conj acc (first (.. x toString toUpperCase)))
                  (conj acc x))]))
            [false []]
          (seq w)))))


; #137 Digits and bases
;
; Write a function which returns a sequence of digits of a non-negative number (first argument) in numerical
; system with an arbitrary base (second argument). Digits should be represented with their integer values,
; e.g. 15 would be [1 5] in base 10, [1 1 1 1] in base 2 and [15] in base 16.
;
; (= [1 2 3 4 5 0 1] (__ 1234501 10))
; (= [0] (__ 0 11))
; (= [1 0 0 1] (__ 9 2))

(fn [num base]
  (loop [acc [] n num]
    (if (< n base)
      (reverse (conj acc n))
      (let [r (rem n base)]
        (recur (conj acc r) (/ (- n r) base))))))

; #132 Insert between two items

(fn [op val coll]
  (second
    (reduce
      (fn [[prev acc] x]
        [x (if (op prev x)
          (conj acc val x)
          (conj acc x))])
      [(first coll) []]
      coll)))

; #144
; Write an oscillating iterate: a function that takes an initial value and a variable
; number of functions. It should return a lazy sequence of the functions applied
; to the value in order, restarting from the first function after it hits the end.
(defn f1 [& mm]
  (let [n (count mm)]
    (lazy-seq
      (loop [acc [] fs mm]
        (recur (cons acc (first fs))))
  (println mm))))

; #97 Pascal's triangle
; [1]
; [1 1]
; [1 2 1]
; [1 3 3 1]

; ! is a way to self-reference an anonymous function
(fn ! [level]
  (if (= 1 level)
    [1]
    (conj (second
             (reduce
                (fn [[prev acc] x]
                  [x (conj acc (+ prev x))])
                [0 []]
                (! (dec level)))) 1)))

; #95 Write a predicate which checks whether or not a given sequence represents a binary tree.
; Each node in the tree must have a value, a left child, and a right child.

(defn btree? [t]
  (and
    (sequential? t)
    (= 3 (count t))
    (let [[val left right] t]
      (and
        (not (sequential? val))
        (not (nil? val))
        (or (nil? left) (btree? left))
        (or (nil? right) (btree? right))))))

(fn ! [t]
  (and
    (sequential? t)
    (= 3 (count t))
    (let [[val left right] t]
      (and
        (not (sequential? val))
        (not (nil? val))
        (or (nil? left) (! left))
        (or (nil? right) (! right))))))


; # 120 Sum of square of digits
; 
; Write a function which takes a collection of integers.
; Return the count of how many elements are smaller
; than the sum of their squared component digits. 
; For example: 10 is larger than 1 squared plus 0 squared;
; whereas 15 is smaller than 1 squared plus 5 squared.

(fn [coll]
  (letfn [(int-to-seq [n] (map #(- (int %) 48) (seq (str n))))
          (f2 [x] (if (< x
                         (reduce (fn [acc n] (+ acc (* n n)))
                                 0
                                 (int-to-seq x)))
                    1 0))]
    (reduce (fn [acc x] (+ acc (f2 x))) 0 coll)))
           

; # 115 A balanced number is one whose component digits 
; have the same sum on the left and right halves of the number.
; Write a function which accepts an integer n, and returns
; true if n is balanced.

(fn [n]
  (cond (< n 0) false
        (< n 9) true
        :else (letfn [(int-to-seq [n] (map #(- (int %) 48) (seq (str n))))]
                (let [s (vec (int-to-seq n))
                      len (count s)
                      m (int (/ len 2))
                      l (subvec s 0 m)
                      r (subvec s (+ m (if (even? len) 0 1)))]
                  (= (reduce + l)
                     (reduce + r))))))


; #92 Read Roman numbers

(fn [rom]
  (let [roman {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}
        s (map roman (reverse (seq rom)))]
    (second 
      (reduce (fn [[prev sum] n]
                [n (+ sum (if (>= n prev) n (- n)))])
              [0 0]
              s))))


; #121 Given a mathematical formula in prefix notation, return a
; function that calculates the value of the formula. 
; The formula can contain nested calculations using the four 
; basic mathematical operators, numeric constants, and symbols
; representing variables. The returned function has to accept 
; a single parameter containing the map of variable names to their values. 

(fn [expression]
  (fn [m]
    (letfn [(atomm? [x]
                    (or (nil? x)
                        (not (seq? x))))
            (eva [exp]
               (if (atomm? exp)
                 (if (contains? m exp) (m exp) exp)
                 (let [[op & args] exp]
                   (cond
                     (= '+ op) (apply + (map eva args))
                     (= '- op) (apply - (map eva args))
                     (= '* op) (apply * (map eva args))
                     (= '/ op) (apply / (map eva args))                     
                     :else 100))))]
      (eva expression))))

; #104 Write Roman numerals
(fn [n]
  (let [romans1 {1000 "M"  900 "CM" 500 "D" 100 "C" 90 "XC" 50 "L" 40 "XL"
                 10 "X"  9 "IX" 8 "VIII" 7 "VII"
                 6 "VI" 5 "V" 4 "IV" 3 "III" 2 "II" 1 "I"}]
    (letfn [(find-range [n]
                        (first (filter #(>= n %) (sort > (keys romans1)))))
            (rom [n acc]
                 (if (= 0 n)
                   acc
                   (let [r (find-range n)]
                     (rom (- n r) (conj acc (romans1 r))))))]
      (apply str (rom n [])))))


; # 157 Transform a sequence into a sequence of pairs containing the original elements along with their index.

(fn [coll] (map vector coll (range)))
  
; #100 write a function to calculate the least common multiplier

(defn f [& args]
  (letfn [(multipliers [n] (apply sorted-set (map #(* n %) (range 1 20))))]
    (apply clojure.set/intersection (map multipliers args))))
