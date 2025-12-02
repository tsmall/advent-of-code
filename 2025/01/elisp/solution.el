;;; solution.el --- Advent of Code 2025 Day 1 -*- lexical-binding: t; -*-

(require 'cl-lib)

;;; Commentary:
;;
;; This is my solution to Day 1 of the Advent of Code for 2025.

;;; Code:

(defun aoc-2025-01-parse-instruction (str)
  "Parse a single instruction in STR like \"R68\" into (operation . number)."
  (cons (pcase (substring str 0 1)
          ("L" '-)
          ("R" '+))
        (string-to-number (substring str 1))))

(defun aoc-2025-01-parse-buffer (buffer)
  "Parse BUFFER with one instruction per line."
  (with-current-buffer buffer
    (goto-char (point-min))
    (let ((instructions '()))
      (while (not (eobp))
        (let ((line (string-trim (buffer-substring-no-properties
                                  (line-beginning-position)
                                  (line-end-position)))))
          (when (string-match-p "^[LR][0-9]+$" line)
            (push (aoc-2025-01-parse-instruction line) instructions)))
        (forward-line 1))
      (nreverse instructions))))

(defun aoc-2025-01-part-1 (buffer)
  "Solve Part 1 given input in BUFFER.
Count every time we land on 0 after the full rotation."
  (cl-loop with x = 50
           with zeroes = 0
           for (f . n) in (aoc-2025-01-parse-buffer buffer)
           do (setq x (mod (funcall f x n) 100))
           if (zerop x) do (cl-incf zeroes)
           finally return zeroes))

;; (aoc-2025-01-part-1 (get-buffer "example.txt")) => 3
;; (aoc-2025-01-part-1 (get-buffer "input.txt"))   => 1023

(defun aoc-2025-01-part-2 (buffer)
  "Solve Part 2 given input in BUFFER.
Count every time we land on 0 during any click, not just at endpoints."
  (cl-loop with x = 50
           with zeroes = 0
           for (f . n) in (aoc-2025-01-parse-buffer buffer)
           do (cl-loop repeat n
                       do (setq x (mod (funcall f x 1) 100))
                       if (zerop x) do (cl-incf zeroes))
           finally return zeroes))

;; (aoc-2025-01-part-2 (get-buffer "example.txt"))
;; (aoc-2025-01-part-2 (get-buffer "input.txt"))

;;; solution.el ends here
