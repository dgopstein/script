(ns keylog-reader
  (:require [clojure.tools.cli :refer [cli]]))

; http://stackoverflow.com/questions/18660687/clojure-take-while-and-n-more-items
(defn take-while-and-n-more [pred n coll]
    (let [[head tail] (split-with pred coll)]
             (concat head (take n tail))))

(def re-meta #"<[^<>]+>")
(def re-char #".")
(def re-token (re-pattern (str "(?s)" re-meta "|" re-char)))

(defn -main [& args]
 (let [[opts argv banner] (cli args
                           ["-h" "--help" "Print this help"
                           :default false :flag true])]
  (when (:help opts)
     (println banner))

     
  (let [filename (first args)
        file  (slurp filename)
        tokens (re-seq re-token file)
        counts (frequencies tokens)
        sorted (sort-by val > counts)]
    (println sorted)
    )))

