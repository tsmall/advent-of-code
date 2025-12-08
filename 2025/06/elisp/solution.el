;;; solution.el --- Advent of Code 2025 Day 6 -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'dash)

;;; Commentary:
;;
;; This is my solution to Day 6 of the Advent of Code for 2025.

;;; Code:

(defun aoc-2025-06--parse-input-rows (buffer)
  "Return list of list of parsed rows in BUFFER."
  (let ((rows '()))
    (with-current-buffer buffer
      (goto-char (point-min))
      (while (not (looking-at-p "[*+]"))
        (--> (thing-at-point 'line)
             (split-string it)
             (-map #'string-to-number it)
             (push it rows))
        (forward-line 1))
      (--> (thing-at-point 'line)
           (split-string it)
           (push it rows)))
    rows))

(defun aoc-2025-06-part-1 (buffer)
  (let ((rows (aoc-2025-06--parse-input-rows buffer))
        (total 0))
    (while (car rows)
      (let ((symbol (intern (caar rows)))
            (numbers (mapcar #'car (cdr rows))))
        (setq rows (mapcar #'cdr rows))
        (cl-incf total (apply symbol numbers))))
    total))

(defun aoc-2025-06-part-2 (buffer)
  (let ((total 0))
    (with-current-buffer buffer
      (goto-char (point-min))
      (let ((numbers '())
            operator
            (col 0))
        (while (not (eolp))
          (let ((digits '()))
            (while (not (eobp))
              (let ((char (char-after)))
                (cond
                 ((and char (memq char '(?+ ?*)))
                  (setq operator (intern (char-to-string char))))
                 ((and char (<= ?0 char ?9))
                  (push char digits))))
              (forward-line)
              (move-to-column col))
            (if digits
                (push (->> (nreverse digits)
                           (apply #'string)
                           (string-to-number))
                      numbers)
              (progn
                (setq total (+ total (apply operator numbers))
                      numbers '()
                      operator nil))))
          (goto-char (point-min))
          (move-to-column (cl-incf col)))
        (cl-incf total (apply operator numbers))))
    total))

;; (aoc-2025-06-part-1 (get-buffer "input.txt"))
;; (aoc-2025-06-part-2 (get-buffer "input.txt"))

;;; solution.el ends here
