;;; utility.el -*- lexical-binding: t; -*-

;;; utility.el --- Utility functions for Doom Emacs -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Justin Kroes
;;
;; Author: Justin Kroes <jkroes14@ucsbalum.com>
;; Maintainer: Justin Kroes <jkroes14@ucsbalum.com>
;; Created: February 09, 2024
;; Modified: February 09, 2024
;; Version: 0.0.1
;; Homepage: https://github.com/jkroes/doom-utility-functions.git
;; Package-Requires: ((emacs 28.2))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Utility functions for Doom Emacs
;;
;;; Code:

(defconst IS-WSL (and (string-match "-[Mm]icrosoft" operating-system-release)
                      (eq system-type 'gnu/linux)))

;; Overrides ~/.config/doom/modules/editor/evil/init.el
(defvar evil-collection-mode-list
    '(edebug
      ediff
      info
      vundo
      devdocs))

(defun concat-path (directory &rest components)
  "Append COMPONENTS to DIRECTORY and return the resulting string.

DIRECTORY must be an absolute path.

Arguments otherwise are the same as for `file-name-concat'."
  (unless (file-name-absolute-p directory)
    (error "DIRECTORY is not an absolute path"))
  (expand-file-name (apply 'file-name-concat (cons directory components))))


;; TODO Unify these functions for different modes
(defun lisp-comment-pad-dashes-right ()
  "Fill a three-semicolon lisp comment with dash characters from
the end of the line to `fill-column'."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (save-match-data (looking-at "^;;;"))
      (end-of-line)
      (just-one-space)
      (insert (make-string (- fill-column (current-column)) ?-)))))

(defun ruby-comment-pad-dashes-right ()
  "Fill a three-semicolon lisp comment with dash characters from
the end of the line to `fill-column'."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (save-match-data (looking-at "^#"))
      (end-of-line)
      (just-one-space)
      (insert (make-string (- fill-column (current-column)) ?-)))))

(defun open-in-windows (path &rest _)
  "Open a file from WSL with the default application in Windows"
  (let ((browse-url-generic-program "/mnt/c/Windows/System32/cmd.exe")
        (browse-url-generic-args '("/c" "start" "")))
    (browse-url-generic
     (substring
      (shell-command-to-string
       (format "wslpath -w '%s'" path))
      0 -1))))


(defun unbind-command (keymap command)
  (require 'dash)
  (let ((all-bindings (where-is-internal command keymap)))
    (-map (lambda (binding) (unbind-key binding keymap))
          all-bindings)))


(defun unbind-commands (keymap cmds)
  (let ((cmds (ensure-list cmds)))
    (dolist (command cmds)
      (unbind-command keymap command))))


(defun startup-font-size ()
  (pcase (display-monitor-attributes-list)
    ;; Dual monitor setup
    ((pred (lambda (dmal)
             (length= dmal 2))) 18)
    ;; Macbook Air (M3, 2024)
    ((pred (lambda (dmal)
             (and (length= dmal 1)
                  (equal (last (caar dmal) 2) '(1470 956))))) 16)))



(provide 'utility)
;;; utility.el ends here
