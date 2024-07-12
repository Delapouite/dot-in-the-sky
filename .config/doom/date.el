;;; date.el -*- lexical-binding: t; -*-

(defun iso8601-format (&optional time)
  "Format time string with %FT%T%z TIME"
  (format-time-string "%FT%T%z" time))

(defun iso8601-to-epoch (&optional iso)
  "Parse iso8601 string to unix epoch timestamp"
  (string-to-number (format-time-string "%s" (if iso (date-to-time iso)))))

(defun iso8601-diff-days (iso)
  "Return the number of days between today and ISO"
  (let ((now (iso8601-to-epoch))
        (past (iso8601-to-epoch iso)))
    (/ (- now past) 86400)))
