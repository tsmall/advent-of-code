;;; solution.el --- Advent of Code 2025 Day 3 -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)

;;; Commentary:
;;
;; This is my solution to Day 3 of the Advent of Code for 2025.

;;; Code:

(defun aoc-2025-03--largest-possible-joltage (k numbers)
  (let* ((digits (string-to-list numbers))
         (n (length digits))
         (result '())
         (start 0))
    (dotimes (i k)
      (let* ((end (1+ (- n (- k i))))
             (subseq (seq-subseq digits start end))
             (max-digit (apply #'max subseq))
             (max-idx (+ start (cl-position max-digit subseq))))
        (push max-digit result)
        (setq start (1+ max-idx))))
    (string-to-number (concat (nreverse result)))))

(ert-deftest test-aoc-2025-03--largest-possible-joltage ()
  (should (= 98 (aoc-2025-03--largest-possible-joltage 2 "987654321111111")))
  (should (= 89 (aoc-2025-03--largest-possible-joltage 2 "811111111111119")))
  (should (= 78 (aoc-2025-03--largest-possible-joltage 2 "234234234234278")))
  (should (= 92 (aoc-2025-03--largest-possible-joltage 2 "818181911112111")))
  (should (= 987654321111 (aoc-2025-03--largest-possible-joltage 12 "987654321111111")))
  (should (= 811111111119 (aoc-2025-03--largest-possible-joltage 12 "811111111111119")))
  (should (= 434234234278 (aoc-2025-03--largest-possible-joltage 12 "234234234234278")))
  (should (= 888911112111 (aoc-2025-03--largest-possible-joltage 12 "818181911112111"))))

(defun aoc-2025-03--max-total-joltage (k buffer)
 (with-current-buffer buffer
   (save-excursion
     (goto-char (point-min))
     (let ((total-joltage 0))
       (while (not (eobp))
         (let ((line (buffer-substring-no-properties 
                      (line-beginning-position) 
                      (line-end-position))))
           (unless (string-empty-p line)
             (incf total-joltage (aoc-2025-03--largest-possible-joltage k line))))
         (forward-line 1))
       total-joltage))))

(defun aoc-2025-03-part-1 (buffer)
  (aoc-2025-03--max-total-joltage 2 buffer))

(defun aoc-2025-03-part-2 (buffer)
  (aoc-2025-03--max-total-joltage 12 buffer))

;; (ert-run-tests-interactively "test-aoc-2025-03")
;; (aoc-2025-03-part-1 (get-buffer "input.txt"))
;; (aoc-2025-03-part-2 (get-buffer "input.txt"))

;;; solution.el ends here
