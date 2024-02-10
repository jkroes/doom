;;; pre-module-init.el -*- lexical-binding: t; -*-

;; This file contains code that should run prior to any module
;; files. Utility functions are loaded early in case they are
;; used by any module init.el.

(load-doom-private "utility.el")

(defconst IS-WSL (and (string-match "-[Mm]icrosoft" operating-system-release)
                      (eq system-type 'gnu/linux)))

;; Overrides ~/.config/doom/modules/editor/evil/init.el
(defvar evil-collection-mode-list
    '(edebug
      ediff
      info
      vundo
      devdocs))
