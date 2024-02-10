;;; hackery.el -*- lexical-binding: t; -*-

;; Cancel out the use-package :config in
;; ~/.config/emacs/lisp/doom-editor.el by calling
;; use-package-hook! after it is defined and before the
;; use-package! in that file.
(with-eval-after-load (expand-file-name
                       "modules/config/use-package/init.el"
                       doom-emacs-dir)
  (use-package-hook! smartparens :pre-config nil))

(with-eval-after-load (expand-file-name
                       "lisp/doom-editor.el"
                       doom-emacs-dir)
  ;; Disable smartparens-global-mode; I dislike smartparens
  (remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)
  ;; Execute only the following two lines from the previously
  ;; disabled :config block to fix the following bug:
  ;;
  ;; BUG evil-join-a fails to remove comment character when
  ;; joining comment lines when smartparens is disabled. The
  ;; cause is doom-point-in-comment-p, where
  ;; doom-point-in-comment-functions is empty because it no
  ;; longer includes its otherwise sole value:
  ;; sp-point-in-comment.
  (require 'smartparens)
  (add-to-list 'doom-point-in-string-functions 'sp-point-in-string)
  (add-to-list 'doom-point-in-comment-functions 'sp-point-in-comment))
