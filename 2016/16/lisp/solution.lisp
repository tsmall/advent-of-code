(defpackage :aoc.2016.16
  (:use :common-lisp))

(in-package :aoc.2016.16)

(defun expand-data (data)
  (let ((original-length (length data)))
    (vector-push-extend #\0 data)
    (do ((i (- original-length 1) (- i 1)))
        ((< i 0) nil)
      (let ((c (elt data i)))
        (vector-push-extend (case c (#\0 #\1) (#\1 #\0)) data)))))

(defun generate-data (input desired-length)
  (let ((result (make-array (length input)
                            :element-type 'character
                            :initial-contents input
                            :adjustable t
                            :fill-pointer t)))
    (do ((result (adjust-array result (* 2 desired-length))))
        ((> (length result) desired-length)
         (subseq result 0 desired-length))
      (expand-data result))))

(defun checksum-step (data buffer)
  (do ((i 0 (+ i 2)))
      ((= i (length data)) nil)
    (let ((a (elt data i))
          (b (elt data (+ i 1))))
      (vector-push
       (if (eql a b) #\1 #\0)
       buffer))))

(defun checksum (data)
  (labels ((rec (data buffer)
             (setf (fill-pointer buffer) 0)
             (if (oddp (length data))
                 data
                 (progn
                   (checksum-step data buffer)
                   (rec buffer data)))))
    (rec (make-array (length data)
                     :element-type 'character
                     :initial-contents data
                     :fill-pointer t)
         (make-array (length data)
                     :element-type 'character
                     :fill-pointer t))))

(defparameter input
  "01000100010010111")

(defun solve-for-size (size)
  (checksum (generate-data input size)))

(defun part-one ()
  (solve-for-size 272))

(defun part-two ()
  (solve-for-size 35651584))

;; (time (part-one)) "10010010110011010"
;; (time (part-two)) "01010100101011100"
