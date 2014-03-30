(tc +)

(define identity
{a --> a}
  A -> A)
\*
(define count-digits 
{number --> number}
  A ->
    (cases (and (A >= 0) (A <= 9)) 0
          (< A 0) (count-digits (* -1 A))
          else (+ 1 (count-digits (/ A 10)))))

(count-digits 123)
*\
