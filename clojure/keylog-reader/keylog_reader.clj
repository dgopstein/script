(ns keylog-reader
    (:require [clojure.tools.cli :refer [cli]])
    (:require [clojure.string :as str])
  )

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

(defn rename [token]
  (get {" " ["<SPACE>"]} token [token]))

(defn fix-tokens [token]
  ;(reduce #(mapcat %2 %1) token [parse-shift rename]))
  (let [shifted (parse-shift token)
        renamed (mapcat rename shifted)]
    renamed))


(defn count-from [fltr input]
  (count (filter #(.contains fltr %) input)))
  
(defn remove-junk [s]
  (str/join "\n" (filter
    #(or (str/starts-with? %1 "![")
         (< (count %1) 3000))
    (str/split s #"\n"))))

(defn -main [& args]
 (let [[opts argv banner] (cli args
                           ["-h" "--help" "Print this help"
                           :default false :flag true])]
  (when (:help opts)
     (println banner))

     
  (let [filename   (first args)
        file       (slurp filename)
        clean-file (remove-junk file)
        tokens     (re-seq re-token clean-file)
        renamed    (mapcat fix-tokens tokens)
        counts     (frequencies renamed)
        sorted     (sort-by val > counts)]
    (println (str/join "\n" (map (fn [[k v]] (str v "," (pr-str k))) (cons ["key", "count"] sorted)))

    ;(println "!@#$%^&*(): " (count-from "!@#$%^&*()" tokens))
    ;(println "1234567890: " (count-from "1234567890" tokens))
    
    ))))

