;;; lang/org/autoload/org-src.el -*- lexical-binding: t; -*-

;;;###autoload
(defun my/org-edit-src-save-and-exit ()
  (interactive)
  (org-edit-src-save)
  (org-edit-src-exit)
  ;; Prevents accidental text insertion
  (evil-normal-state))
