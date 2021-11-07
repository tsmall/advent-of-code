;; -----------------------------------------------------------------------------
;; Logic


(define (move position direction)
  (cond
   ((= direction (char "^")) (setf (position 1) (++ (position 1))))
   ((= direction (char ">")) (setf (position 0) (++ (position 0))))
   ((= direction (char "v")) (setf (position 1) (-- (position 1))))
   ((= direction (char "<")) (setf (position 0) (-- (position 0)))))
  position)


(define (record-visit position grid)
  (setf (grid position) true))


;; Counts the number of true values in two-dimensional matrix `m'.
(define (count-matrix m)
  (let (c 0)
    (dolist (row m)
      (dolist (x row)
        (if x (inc c))))
    c))


;; -----------------------------------------------------------------------------
;; Solution


(setq input
  (trim (read-file "../input.txt")))


;; Part 1

(setq position '(0 0))
(setq Grid:Grid (array 100 100))

(record-visit position Grid)

(dostring (direction input)
  (setq position (move position direction))
  (record-visit position Grid))

(println "Part 1: " (count-matrix Grid))


;; Part 2

(setq positions '((0 0) (0 0)))
(setq Grid:Grid (array 100 100))

(record-visit (positions 0) Grid)

(dostring (direction input)
  (setf (positions 0) (move (positions 0) direction))
  (record-visit (positions 0) Grid)
  (swap (positions 0) (positions 1)))

(println "Part 2: " (count-matrix Grid))


(exit)
