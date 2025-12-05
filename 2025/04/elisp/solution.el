;;; solution.el --- Advent of Code 2025 Day 4 -*- lexical-binding: t; -*-

(require 'cl-lib)

;;; Commentary:
;;
;; This is my solution to Day 4 of the Advent of Code for 2025.

;;; Code:

(defun aoc-2025-04--parse-input (buffer)
  "Parse paper roll positions from BUFFER into a hash table."
  (let ((positions (make-hash-table :test 'equal))
        (row 0)
        (col 0))
    (with-current-buffer buffer
      (goto-char (point-min))
      (while (not (eobp))
        (let ((char (char-after)))
          (cond
           ((eq char ?@)
            (puthash (cons row col) t positions)
            (cl-incf col))
           ((eq char ?\n)
            (cl-incf row)
            (setq col 0))
           ((eq char ?.)
            (cl-incf col))
           (t
            nil)))
        (forward-char 1)))
    positions))

(defun aoc-2025-04--remove-rolls (positions)
  (let ((count 0)
        (candidates '()))
    (maphash (lambda (pos _value)
               (let ((row (car pos))
                     (col (cdr pos))
                     (neighbors 0))
                 (dolist (neighbor (list (cons (1- row) (1- col)) (cons (1- row) col) (cons (1- row) (1+ col))
                                         (cons row      (1- col))                     (cons row      (1+ col))
                                         (cons (1+ row) (1- col)) (cons (1+ row) col) (cons (1+ row) (1+ col))))
                   (when (gethash neighbor positions)
                     (cl-incf neighbors)))
                 (when (< neighbors 4)
                   (cl-incf count)
                   (push pos candidates))))
             positions)
    (dolist (pos candidates)
      (remhash pos positions))
    count))

(defun aoc-2025-04-part-1 (buffer)
  (let ((positions (aoc-2025-04--parse-input buffer)))
    (aoc-2025-04--remove-rolls positions)))

(defun aoc-2025-04-part-2 (buffer)
  (let ((positions (aoc-2025-04--parse-input buffer))
        (removed 1)
        (total-removed 0))
    (while (> removed 0)
      (setq removed (aoc-2025-04--remove-rolls positions)
            total-removed (+ total-removed removed)))
    total-removed))

;; (aoc-2025-04-part-1 (get-buffer "input.txt"))
;; (aoc-2025-04-part-2 (get-buffer "input.txt"))

;;; solution.el ends here
