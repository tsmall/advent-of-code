;;; solution.el --- Advent of Code 2025 Day 2 -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'dash)
(require 'ert)

;;; Commentary:
;;
;; This is my solution to Day 2 of the Advent of Code for 2025.

;;; Code:

(defun aoc-2025-02--id-is-invalid (n)
  "Return non-nil if the number N is an invalid ID.
Invalid IDs are numbers that consist of exactly two repeating numbers."
  (let* ((str (number-to-string n))
         (len (length str))
         (mid (/ len 2)))
    (string= (substring str 0 mid)
             (substring str mid))))

(ert-deftest test-aoc-2025-02--is-is-invalid ()
  (should (aoc-2025-02--id-is-invalid 11))
  (should (aoc-2025-02--id-is-invalid 123123))
  (should (not (aoc-2025-02--id-is-invalid 123))))

(defun aoc-2025-02--id-is-repeated-at-least-twice (number)
  "Return non-nil if NUMBER contains a digit sequence repeated at least twice."
  (let* ((str (number-to-string number))
         (len (length str)))
    (catch 'found
      (dotimes (seq-len (/ len 2))
        (let ((actual-len (1+ seq-len)))
          (when (zerop (mod len actual-len))
            (let ((pattern (substring str 0 actual-len))
                  (matches t))
              (dotimes (i (/ len actual-len))
                (unless (string= pattern
                                 (substring str
                                            (* i actual-len)
                                            (* (1+ i) actual-len)))
                  (setq matches nil)))
              (when matches
                (throw 'found pattern)))))))))

(ert-deftest test-aoc-2025-02--id-is-repeated-at-least-twice ()
  (should (aoc-2025-02--id-is-repeated-at-least-twice 11)))

(defun aoc-2025-02--find-invalid-ids (f ranges)
  "Returns all invalid IDs found in RANGES, using function F."
  (cl-loop with invalid-ids = '()
           for (start . end) in ranges
           do (cl-loop for id from start to end
                       if (funcall f id)
                       do (push id invalid-ids))
           finally return invalid-ids))

(ert-deftest test-aoc-2025-02--find-invalid-ids ()
  (let ((ranges '((11 . 22)
                  (95 . 115)
                  (998 . 1012)
                  (1188511880 . 1188511890)
                  (222220 . 222224)
                  (1698522 . 1698528)
                  (446443 . 446449)
                  (38593856 . 38593862)
                  (565653 . 565659)
                  (824824821 . 824824827)
                  (2121212118 . 2121212124))))
    (should (equal (aoc-2025-02--find-invalid-ids 'aoc-2025-02--id-is-invalid ranges)
                   '(38593859 446446 222222 1188511885 1010 99 22 11)))))

(defun aoc-2025-02--parse-ranges (input)
  "Parses the input string into a list of (START . END) ranges."
  (mapcar (lambda (range)
            (let ((parts (split-string range "-")))
              (cons (string-to-number (car parts))
                    (string-to-number (cadr parts)))))
          (split-string input ",")))

(ert-deftest test-aoc-2025-02--parse-ranges ()
  (should (equal (aoc-2025-02--parse-ranges "11-22") '((11 . 22))))
  (should (equal (aoc-2025-02--parse-ranges "11-22,95-115") '((11 . 22) (95 . 115))))
  (should (equal (aoc-2025-02--parse-ranges "11-22,95-115,998-1012") '((11 . 22) (95 . 115) (998 . 1012)))))

(defun aoc-2025-02--parse-input (buffer)
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (aoc-2025-02--parse-ranges
       (buffer-substring-no-properties (line-beginning-position)
                                       (line-end-position))))))

(defun aoc-2025-02-part-1 (buffer)
  (->> buffer
       aoc-2025-02--parse-input
       (aoc-2025-02--find-invalid-ids 'aoc-2025-02--id-is-invalid)
       (apply '+)))

(defun aoc-2025-02-part-2 (buffer)
  (->> buffer
       aoc-2025-02--parse-input
       (aoc-2025-02--find-invalid-ids 'aoc-2025-02--id-is-repeated-at-least-twice)
       (apply '+)))

;; (ert-run-tests-interactively "test-aoc-2025-02")
;; (aoc-2025-02-part-1 (get-buffer "input.txt"))
;; (aoc-2025-02-part-2 (get-buffer "input.txt"))

;;; solution.el ends here
