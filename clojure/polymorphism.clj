(defmulti add class)

(defmethod add java.lang.String [data]
  (str "\"" data "\""))

(defmethod add Integer [data]
  (apply + data))


(prn (add 1 2 3))
(prn (add "1" "2" "3"))

