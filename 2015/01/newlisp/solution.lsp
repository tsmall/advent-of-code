(set 'input
  (trim (read-file "../input.txt")))

(set 'current-floor 0)
(set 'steps-to-basement 0)
(set 'basement-found? false)

(define (change-floor c)
  (cond
    ((= c (char "(")) (++ current-floor))
    ((= c (char ")")) (-- current-floor))))

(define (move c)
  (change-floor c)
  (when (not basement-found?)
    (++ steps-to-basement)
    (if (< current-floor 0)
      (set 'basement-found? true))))

(dostring (c input)
  (move c))

(println "Part 1: " current-floor)
(println "Part 2: " steps-to-basement)

(exit)
