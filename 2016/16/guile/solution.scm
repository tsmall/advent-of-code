(define-module (aoc y2017 d16))

(use-modules (ice-9 vlist))

(define (generate-data input desired-size)
  (define buffer (make-vector (* 2 desired-size)))
  (define i 0)
  (letrec* ((push (lambda (v)
                    (vector-set! buffer i v)
                    (set! i (+ i 1))))
            (init-buffer (lambda (remaining-input)
                           (when (not (nil? remaining-input))
                             (push (list-ref input i))
                             (init-buffer (cdr remaining-input)))))
            (generate (lambda ()
                        (if (>= i desired-size)
                            (make-shared-array buffer list desired-size)
                            (begin
                              (push #\0)
                              (do ((j (- i 2) (- j 1)))
                                  ((< j 0))
                               (push
                                (case (vector-ref buffer j)
                                  ((#\0) #\1)
                                  ((#\1) #\0))))
                             (generate))))))
    (init-buffer input)
    (generate)))

(define (checksum data)
  (letrec ((loop (lambda (data data-len buffer)
                   (if (odd? data-len)
                       (make-shared-array data list data-len)
                       (begin
                         (do ((i 0 (+ i 2))
                              (j 0 (+ j 1)))
                             ((= i data-len) (loop buffer j data))
                           (vector-set! buffer j
                                        (if (char=? (vector-ref data i)
                                                    (vector-ref data (+ i 1)))
                                            #\1
                                            #\0))))))))
    (let* ((len (array-length data))
           (datav (make-vector len)))
      (do ((i 0 (+ i 1)))
          ((= i len))
        (vector-set! datav i (array-ref data i)))
      (loop datav len (make-vector len)))))

(define (solve-for-length input desired-length)
  (checksum (generate-data (string->list input) desired-length)))

(define input
  "01000100010010111")

(define (part-one)
  (solve-for-length input 272))

(define (part-two)
  (solve-for-length input 35651584))

;; (part-one)
;; (part-two)
