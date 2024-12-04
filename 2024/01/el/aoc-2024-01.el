;;; aoc-2024-01.el --- Advent of Code 2024 Day 1 -*- lexical-binding: t; -*-

(defun aoc-2024-01-parse-buffer (buffer)
  (with-current-buffer buffer
    (goto-char (point-min))
    (let ((left-numbers  '())
          (right-numbers '()))
      (while (re-search-forward "\\([0-9]+\\) +\\([0-9]+\\)" nil t)
        (push (string-to-number (match-string 1)) left-numbers)
        (push (string-to-number (match-string 2)) right-numbers))
      (list left-numbers
            right-numbers))))

(defun aoc-2024-01-part-1 (buffer)
  (let* ((numbers (aoc-2024-01-parse-buffer buffer))
         (left-numbers  (car numbers))
         (right-numbers (cadr numbers)))
    (cl-loop for left  in (sort left-numbers  #'<)
             for right in (sort right-numbers #'<)
             sum (abs (- left right)))))

(defun aoc-2024-01-part-2 (buffer)
  (let* ((numbers (aoc-2024-01-parse-buffer buffer))
         (left-numbers  (car numbers))
         (right-numbers (cadr numbers)))
    (cl-loop for left in left-numbers
             sum (* left
                    (seq-count
                     (lambda (right) (= left right))
                     right-numbers)))))

(defun aoc-2024-01 (buffer)
  (list (aoc-2024-01-part-1 buffer)
        (aoc-2024-01-part-2 buffer)))

;; (aoc-2024-01 (get-buffer "example.txt"))
;; (aoc-2024-01 (get-buffer "input.txt"))
