;;; describe.el -*- lexical-binding: t; -*-

;; Related to help map

(map! :leader :desc "Describe alias" "d a" #'describe-alias
      :leader :desc "Describe abbrev" "d A" #'list-abbrevs
      :leader :desc "Describe binding" "d b" #'describe-binding
      :leader :desc "Describe char" "d c" #'describe-char
      :leader :desc "Describe face" "d F" #'describe-face
      :leader :desc "Describe key" "d k" #'describe-key
      :leader :desc "Describe mode" "d m" #'describe-mode
      :leader :desc "Describe package" "d p" #'describe-package
      :leader :desc "Describe text-properties" "d t" #'describe-text-properties
      :leader :desc "Describe theme" "d T" #'describe-theme
      :leader :desc "Describe variable" "d v" #'describe-variable
      :leader :desc "Describe widget" "d w" #'describe-widget

      :leader :desc "Describe function" "d f f" #'describe-function
      :leader :desc "Describe builtin function" "d f b" #'describe-builtin-function
      :leader :desc "Describe obsolete function" "d f o" #'describe-obsolete-function

      :leader :desc "Describe command" "d x x" #'describe-command
      :leader :desc "Describe builtin command" "d x b" #'describe-builtin-command
      :leader :desc "Describe obsolete command" "d x o" #'describe-obsolete-command)

(defun obsolete-p (symbol)
  "Return non nil if SYMBOL is obsolete"
  (or (plist-member (symbol-plist symbol) 'byte-obsolete-info)
      (plist-member (symbol-plist symbol) 'byte-obsolete-variable)))

(defun my/describe-function-prompt (predicate)
  "Prompt for a function from `describe-function' with PREDICATE."
  (let* ((fn (function-called-at-point))
         (prompt (format-prompt "Describe function" fn))
         (enable-recursive-minibuffers t)
         (val (completing-read prompt
               #'help--symbol-completion-table
               predicate
               t nil nil
               (and fn (symbol-name fn)))))
    (unless (equal val "")
      (setq fn (intern val)))
    (unless (and fn (symbolp fn))
      (user-error "You didn't specify a function's symbol"))
    (unless (or (fboundp fn) (get fn 'function-documentation))
      (user-error "Symbol's function definition is void: %s" fn))
    (list fn)))

(defun my/describe-command-prompt (predicate)
  "Prompt for a function from `describe-command' with PREDICATE."
  (let* ((fn (caar command-history))
         (prompt (format-prompt "Describe command" fn))
         (enable-recursive-minibuffers t)
         (val (completing-read prompt
               #'help--symbol-completion-table
               predicate
               t nil nil
               (and fn (symbol-name fn)))))
    (unless (equal val "")
      (setq fn (intern val)))
    (unless (and fn (symbolp fn))
      (user-error "You didn't specify a command's symbol"))
    (unless (or (fboundp fn) (get fn 'function-documentation))
      (user-error "Symbol is not a command: %s" fn))
    (list fn)))

(defun describe-alias (fun)
  "Display the full documentation of aliased FUNCTION (a symbol).
When called from Lisp, FUNCTION may also be a function object."
  (interactive (my/describe-function-prompt
                (lambda (f)
                  (and (function-alias-p f)
                       (or (fboundp f) (get f 'function-documentation))))))
  (describe-function fun))

(defun describe-obsolete-function (fun)
  "Display the full documentation of obsolete FUNCTION (a symbol).
When called from Lisp, FUNCTION may also be a function object."
  (interactive (my/describe-function-prompt
                (lambda (f)
                  (and (obsolete-p f)
                       (or (fboundp f) (get f 'function-documentation))))))
  (describe-function fun))

(defun describe-obsolete-command (command)
  "Display the full documentation of obsolete COMMAND (a symbol).
When called from Lisp, COMMAND may also be a function object."
  (interactive (my/describe-command-prompt
                (lambda (f)
                  (and (obsolete-p f)
                       (commandp f)))))
  (describe-command command))

(defun describe-builtin-function (fun)
  "Display the full documentation of builtin FUNCTION (a symbol).
When called from Lisp, FUNCTION may also be a function object."
  (interactive (my/describe-function-prompt
                (lambda (f)
                  (and (subrp (symbol-function f))
                       (or (fboundp f) (get f 'function-documentation))))))
  (describe-function fun))

(defun describe-builtin-command (command)
  "Display the full documentation of builtin COMMAND (a symbol).
When called from Lisp, COMMAND may also be a function object."
  (interactive (my/describe-command-prompt
                (lambda (f)
                  (and (subrp (symbol-function f))
                       (commandp f)))))
  (describe-command command))
