(ns tt.csvslurp)

;;; This is a set of functions to read the CSV file with database table
;;; metadata, and later generate a DDL script to create tables according
;;; to such metadata
;;;
(defn slurpcsv [file]
  "Load a CSV-delimited file into a list of vectors"
  (map #(clojure.string/split % #",")
       (clojure.string/split (slurp file) #"\r\n")))

(defn third [coll]
  "Returns a third item"
  (nth coll 2))

(defn delimit-by [val coll]
  "Separates items of a sequence by an arbitrary value"
  (if (empty? coll)
    coll
    (reduce #(conj %1 val %2) [(first coll)] (rest coll))))

;;;; ------------------ Test ---------------------

;;
;; This CSV file describes a database table. It has three fields:
;; table name, column name, and column type
;;
(def metafile "C:/tmp/file-out.txt")
(def sqlfile "C:/tmp/staging-tables.sql")
(def dict (group-by first (slurpcsv metafile)))

(defn gendrop [table]
  "Generates a drop statement"
    (str "DROP TABLE " table ";\n"))

(defn gencreate [[table columns]]
  "Generates DDL statement"
  (str
    (apply str "CREATE TABLE " table " (\n"
           (delimit-by ",\n"
                       (map #(str "  "
                                  (second %1) "     "
                                  (third %1)) columns)))
    "\n);\n\n"))

(def drops (reduce str (map gendrop (keys dict))))
(def tabs (reduce str (map gencreate dict)))

(spit sqlfile drops)
(spit sqlfile "\n\n" :append true)
(spit sqlfile tabs :append true)




