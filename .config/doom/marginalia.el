;;; marginalia.el -*- lexical-binding: t; -*-
;;; https://github.com/minad/marginalia/

(use-package! marginalia
  :config
  (add-to-list 'marginalia-prompt-categories '("\\<Callable\\>" . function))

  (defun marginalia--function-location (func)
    (let ((res (find-lisp-object-file-name func (symbol-function func))))
      (if (symbolp res) (symbol-name res) (f-base res))))

  (defun marginalia-annotate-function (cand)
    "Annotate function CAND with its documentation string."
    (when-let (sym (intern-soft cand))
      (when (fboundp sym)
        (marginalia--fields
         (:left (marginalia-annotate-binding cand))
         ((marginalia--symbol-class sym) :face 'marginalia-type)
         ((marginalia--function-args sym) :face 'marginalia-value :truncate 0.7)
         ((marginalia--function-doc sym) :face 'marginalia-documentation :truncate 1.0)
         ((marginalia--function-location sym) :face 'marginalia-file-name :truncate 0.5))))))
