;;; property-drawer-dates.el -*- lexical-binding: t; -*-

; relative dates in property drawers
; currenly not used

(defcustom org+-dateprop-reltime-number-of-items 3
  "Number of time items shown for relative time."
  :type 'number
  :group 'org)

(defcustom org+-dateprop-properties '("asked-at"
                                      "built-at"
                                      "closed-at"
                                      "contributed-at"
                                      "created-at"
                                      "fetched-at"
                                      "last-commit-at"
                                      "merged-at"
                                      "published-at"
                                      "updated-at"
                                      "upgraded-at")
  "Names of properties with dates."
  :type 'org+-dateprop-properties-widget
  :group 'org)

(defun org+-next-property-drawer (&optional limit)
  "Search for the next property drawer.
When a property drawer is found position point behind :PROPERTIES:
and return the property-drawer as org-element.
Otherwise position point at the end of the buffer and return nil."
  (let (found drawer)
    (while (and (setq found (re-search-forward org-drawer-regexp limit 1)
              found (match-string-no-properties 1))
        (or (and (setq drawer (org-element-context))
             (null (eq (org-element-type drawer) 'property-drawer)))
            (string-match found "END"))))
    (and found drawer)))

(defun org+-time-since-string (date)
  "Return a string representing the time since DATE."
  (let* ((time-diff (nreverse (seq-subseq (decode-time (time-subtract (current-time) (encode-time date))) 0 6)))
     (cnt 0))
    (setf (car time-diff) (- (car time-diff) 1970))
    (mapconcat
     #'identity
     (cl-loop
      for cnt from 1 upto org+-dateprop-reltime-number-of-items
      for val in time-diff
      for time-str in '("year" "month" "day" "hour" "minute" "second")
      unless (= val 0)
      collect (format "%d %s%s" val time-str (if (> val 1) "s" ""))
      )
     " ")))

(defvar-local org+-dateprop--overlays nil
  "List of overlays used for custom properties.")

(defun org+-dateprop-properties-re (properties)
  "Return regular expression corresponding to `org+-dateprop-properties'."
  (org-re-property (regexp-opt properties) t))

(defvar org+-dateprop--properties-re (org+-dateprop-properties-re org+-dateprop-properties)
  "Regular expression matching the properties listed in `org+-dateprop-properties'.
You should not set this regexp diretly but through customization of `org+-dateprop-properties'.")

(defun my/org-dateprop (&optional absolute)
  "Toggle display of ABSOLUTE or relative time of
properties in `org-dateprop-properties'."
  (interactive "P")
  (if org+-dateprop--overlays
      (progn (mapc #'delete-overlay org+-dateprop--overlays)
         (setq org+-dateprop--overlays nil))
    (unless absolute
      (org-with-wide-buffer
       (goto-char (point-min))
       (let (drawer-el)
     (while (setq drawer-el (org+-next-property-drawer))
       (let ((drawer-end (org-element-property :contents-end drawer-el)))
         (while (re-search-forward org+-dateprop--properties-re drawer-end t)
           ;; See `org-property-re' for the regexp-groups.
           ;; Group 3 is PROPVAL without surrounding whitespace.
           (let* ((val-begin (match-beginning 3))
              (val-end (match-end 3))
              (time (org-parse-time-string (replace-regexp-in-string "[[:alpha:]]" " " (match-string 3))))
              (time-diff-string (format "%s ago" (org+-time-since-string time)))
              (o (make-overlay val-begin val-end)))
         (overlay-put o 'display time-diff-string)
         (overlay-put o 'org+-dateprop t)
         (push o org+-dateprop--overlays))
           ))))))))

(define-widget 'org+-dateprop-properties-widget
  'repeat
  "Like widget '(repeat string) but also updates `org+-dateprop-properties'."
  :value-to-external
  (lambda (_ value)
    (setq org+-dateprop--properties-re (org+-dateprop-properties-re value)) value)
  :args '(string))
