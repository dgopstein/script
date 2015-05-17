(ns keylog-reader
  (:require [clojure.tools.cli :refer [cli]]))

; http://stackoverflow.com/questions/18660687/clojure-take-while-and-n-more-items
(defn take-while-and-n-more [pred n coll]
    (let [[head tail] (split-with pred coll)]
             (concat head (take n tail))))

(defn next-token [log-str]
  (let [c (first log-str)]
    (case c
      '<' (take-while-and-n-more (not= '>') 1 log-str)
      c)))

(defn parse-log [log-str]
  (let [token (next-token log-str)]
  (conj token
        (parse-log (drop (count token) log-str)))))

(defn -main [& args]
 (let [[opts argv banner] (cli args
                           ["-h" "--help" "Print this help"
                           :default false :flag true])]
  (when (:help opts)
     (println banner))

     
  (let [filename (first args)
        file  (slurp filename)]
       
    (parse-log file))))

