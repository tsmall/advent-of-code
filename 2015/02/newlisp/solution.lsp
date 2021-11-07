(define (parse-box line)
  (map int (parse line "x")))

(define (areas box)
  (map * box (rotate box)))

(define (perimeters box)
  (map (curry * 2) (map + box (rotate box))))

(define (volume box)
  (apply * box))

(define (surface-area box)
  (apply + (map (curry * 2) (areas box))))

(define (paper-needed box)
  (+
    (surface-area box)
    (apply min (areas box))))

(define (ribbon-needed box)
  (+
    (volume box)
    (apply min (perimeters box))))

(set 'total-paper-needed 0)
(set 'total-ribbon-needed 0)

(set 'in-file (open "../input.txt" "read"))
(while (read-line in-file)
  (let (box (parse-box (current-line)))
    (++ total-paper-needed (paper-needed box))
    (++ total-ribbon-needed (ribbon-needed box))))

(println "Part 1: " total-paper-needed)
(println "Part 2: " total-ribbon-needed)

(exit)
