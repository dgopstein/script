(ns scrabble.core
  (:require [clojure.math.combinatorics :as combo]))

(defn permute-str [s]
    (map clojure.string/join (combo/permutations (seq s))))

(defn read-words [] (clojure.string/split-lines (slurp "/usr/share/dict/words")))

(defn -main [s]
  (let [words (read-words)
        regexen (map #(re-pattern (str "^" % "$")) (permute-str s))
        pairs (combo/cartesian-product regexen words)]
  
  (prn (filter identity (map #(apply re-find %) pairs)))))
