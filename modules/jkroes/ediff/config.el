;;; -*- lexical-binding: t; -*-

;; Split window into left and right halves. Feel free to change this. See
;; ediff-toggle-split to change this interactively.

(setq ediff-split-window-function #'split-window-horizontally)

;; Kill the *Ediff Registry* buffer after it is no longer needed.
;; This buffer lists active ediff sessions, but I only ever have
;; one active ediff session anyway.

(add-hook 'ediff-quit-hook (cmd! (kill-buffer "*Ediff Registry*")))

;; Run ediff in another frame and disable conflicting Doom
;; configuration in ~/.config/emacs/lisp/doom-ui.el
(after! ediff
  (remove-hook 'ediff-before-setup-hook #'doom-ediff-save-wconf-h)
  (add-hook 'ediff-before-setup-hook (defun ediff-in-new-frame () (select-frame (make-frame))))
  (remove-hook 'ediff-quit-hook #'doom-ediff-restore-wconf-h)
  (remove-hook 'ediff-suspend-hook #'doom-ediff-restore-wconf-h)
  (add-hook! 'ediff-quit-hook :append #'delete-frame))

;; Set the bindings to the local keymap on startup
(add-hook 'ediff-startup-hook #'my/ediff-mode-bindings)
