;;; solution.el --- Advent of Code 2025 Day 5 -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'dash)
(require 'ert)

;;; Commentary:
;;
;; This is my solution to Day 5 of the Advent of Code for 2025.

;;; Code:

(defun aoc-2025-05--parse-range (line)
  "Return the parsed range in LINE as (START . END)."
  (--> line
       (string-split it "-")
       (-map #'string-to-number it)
       (apply #'cons it)))

(ert-deftest test-aoc-2025-05--parse-range ()
  (should (equal '(3 . 5) (aoc-2025-05--parse-range "3-5")))
  (should (equal '(524259904620664 . 529840053343145) (aoc-2025-05--parse-range "524259904620664-529840053343145"))))

(defun aoc-2025-05--parse-all-ranges (buffer)
  "Return all ranges at top of BUFFER in as ((START . END) ...)."
  (let ((ranges '()))
    (with-current-buffer buffer
      (goto-char (point-min))
      (while (not (looking-at-p "^$"))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position))))
          (push (aoc-2025-05--parse-range line) ranges))
        (forward-line 1)))
    ranges))

(defun aoc-2025-05--in-range (ranges n)
  "Return non-nil if N is in a range in RANGES."
  (catch 'found
    (dolist (range ranges)
      (cl-destructuring-bind (start . end) range
        (when (<= start n end)
          (throw 'found t))))))

(ert-deftest test-aoc-2025-05--in-range ()
  (let ((ranges '((3 . 5) (10 . 14) (12 . 18))))
    (should (aoc-2025-05--in-range ranges 3))
    (should (aoc-2025-05--in-range ranges 12))
    (should (not (aoc-2025-05--in-range ranges 9)))
    (should (aoc-2025-05--in-range ranges 15))))

(defun aoc-2025-05--merge-ranges (ranges)
  "Return a non-overlapping, merged version of RANGES."
  (let ((sorted (sort ranges (lambda (a b) (< (car a) (car b)))))
        merged)
    (dolist (range sorted)
      (if (and merged
               (<= (car range) (cdar merged)))
          (setf (cdar merged) (max (cdr range) (cdar merged)))
        (push range merged)))
    (nreverse merged)))

(ert-deftest test-aoc-2025-05--merge-ranges ()
  (should (equal '((3 . 5)) (aoc-2025-05--merge-ranges '((3 . 5)))))
  (should (equal '((3 . 5) (10 . 14)) (aoc-2025-05--merge-ranges '((3 . 5) (10 . 14)))))
  (should (equal '((3 . 5) (10 . 18)) (aoc-2025-05--merge-ranges '((3 . 5) (10 . 14) (12 . 18))))))

(defun aoc-2025-05--numbers-in-range (range)
  "Return the count of numbers in RANGE."
  (cl-destructuring-bind (start . end) range
    (1+ (- end start))))

(ert-deftest test-aoc-2025-05--numbers-in-range ()
  (should (= 3 (aoc-2025-05--numbers-in-range '(3 . 5))))
  (should (= 5 (aoc-2025-05--numbers-in-range '(10 . 14))))
  (should (= 7 (aoc-2025-05--numbers-in-range '(12 . 18)))))

(defun aoc-2025-05-part-1 (buffer)
  "Return the answer to Part 1, given input in BUFFER."
  (let ((fresh-ids 0))
    (with-current-buffer buffer
      (let ((ranges (aoc-2025-05--parse-all-ranges buffer)))
        (forward-line 1)
        (while (not (eobp))
          (let ((n (string-to-number (thing-at-point 'line))))
            (when (aoc-2025-05--in-range ranges n)
              (cl-incf fresh-ids)))
          (forward-line 1))))
    fresh-ids))

(defun aoc-2025-05-part-2 (buffer)
  "Return the answer to Part 1, given input in BUFFER."
  (let ((count 0)
        (ranges (aoc-2025-05--merge-ranges
                 (aoc-2025-05--parse-all-ranges buffer))))
    (dolist (range ranges)
      (cl-incf count (aoc-2025-05--numbers-in-range range)))
    count))

;; (ert-run-tests-interactively "test-aoc-2025-05")
;; (aoc-2025-05-part-1 (get-buffer "input.txt"))
;; (aoc-2025-05-part-2 (get-buffer "input.txt"))

;;; solution.el ends here
