(ns keylog-reader
  (:require [clojure.tools.cli :refer [cli]]))

; http://stackoverflow.com/questions/18660687/clojure-take-while-and-n-more-items
(defn take-while-and-n-more [pred n coll]
    (let [[head tail] (split-with pred coll)]
             (concat head (take n tail))))

(def re-meta #"<[^<>]+>")
(def re-char #".")
(def re-token (re-pattern (str "(?s)" re-meta "|" re-char)))

(def lowercase "`1234567890-=qwertyuiop[]\\asdfghjkl;'zxcvbnm,./")
(def uppercase "~!@#$%^&*()_+QWERTYUIOP{}|ASDFGHJKL:\"ZXCVBNM<>?")

(def downcase-map (zipmap (re-seq #"." uppercase) (re-seq #"." lowercase)))

(defn parse-shift [token]
  (let [downcase (downcase-map token)]
    (if downcase ["<SHIFT>" downcase] [token])))

(defn count-from [fltr input]
  (count (filter #(.contains fltr %) input)))
  

(defn -main [& args]
 (let [[opts argv banner] (cli args
                           ["-h" "--help" "Print this help"
                           :default false :flag true])]
  (when (:help opts)
     (println banner))

     
  (let [filename (first args)
        file  (slurp filename)
        tokens (re-seq re-token file)
        unshifted (mapcat parse-shift tokens)
        counts (frequencies unshifted)
        sorted (sort-by val > counts)]
    (println (clojure.string/join "\n" (map reverse sorted))

    (println "!@#$%^&*(): " (count-from "!@#$%^&*()" tokens))
    (println "1234567890: " (count-from "1234567890" tokens))
    
    ))))

