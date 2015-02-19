(ns scrabble.core
  (:require [clojure.math.combinatorics :as combo])
  (:use clojure.set))

(def words (clojure.string/split-lines (slurp "/usr/share/dict/words")))

(defn permute-str [s]
    (map clojure.string/join (combo/permutations (seq s))))

(defn permute-main [s words]
  (let [regexen (map #(re-pattern (str "^" % "$")) (permute-str s))
        pairs (combo/cartesian-product regexen words)]
  
  (prn (filter identity (map #(apply re-find %) pairs)))))


; http://stackoverflow.com/questions/19953036/clojure-pmap-vs-map
(defn chunked-pmap [f partition-size coll]
  (->> coll                           ; Start with original collection.

       (partition-all partition-size) ; Partition it into chunks.

       (pmap (comp doall              ; Map f over each chunk,
                   (partial map f)))  ; and use doall to force it to be
                                      ; realized in the worker thread.

       (apply concat)))               ; Concatenate the chunked results
                                      ; to form the return value.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;             Sets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def zip (partial map vector))

; http://stackoverflow.com/questions/1676891/mapping-a-function-on-the-values-of-a-map-in-clojure
; map a function as values in a hashmap
(defn mash [f m]
    (into {} (zip m (map f m))))
  ;(into {} (for [k m] [k (f k)])))

(defn set-main [s]
    (let [word-sets (mash set words)
          input-set (set s)
          ; strict subset won't understand .'s, will prematurely filter words
          subsets (filter (fn [[wrd st]] (clojure.set/subset? st input-set)) word-sets)]
            (prn (take 10 subsets))
          (permute-main s
            (keys subsets))
         ))

(def -main set-main)







