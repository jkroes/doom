;;; graveyard.el -*- lexical-binding: t; -*-

;;;; shell snippets -------------------------------------------------------

;; NOTE Abandoning this project in favor of running the-way out of vterm. It
;; can search both description and shell snippet, and it allows spaces within
;; the description. It's live preview is similar to annotatins provided with
;; tempel-insert. And it provides placeholders as well.

;; (use-package! tempel)
;; (use-package! tempel-collection)

;; ;; User-defined templates live in ~/.config/doom/templates
;; (unless (listp tempel-path) (setq tempel-path (list tempel-path)))
;; (push (expand-file-name "~/.config/doom/templates") tempel-path)

;; ;; Originally inspired by:
;; ;; https://github.com/akermu/emacs-libvterm/issues/50.
;; ;; https://www.reddit.com/r/emacs/comments/jd4tu4/how_does_one_insert_text_in_vterm_from_elisp/
;; (defun my/tempel-insert ()
;;   "When used within vterm-mode, insert the snippet into a temporary
;; buffer to allow for snippet completiong using bindings in
;; tempel-map (e.g., tempel-next). As soon as the snippet is
;; finalized by calling `tempel-next' or `tempel-previous' enough
;; times (see TempEl's README.org), insert the results into the original
;; buffer."
;;   (interactive)
;;   (when (eq major-mode 'vterm-mode) ; TODO Handle other terminal and shell modes
;;     (let ((old (current-buffer))
;;           (scratch (make-temp-name "scratch")))
;;       (switch-to-buffer scratch)
;;       (sh-mode) ; Can't use vterm-mode, might as well use mode w/ shell syntax
;;       (when evil-mode (evil-insert-state)) ; For placeholder completion
;;       (defadvice! my/tempel--disable (fn &rest args)
;;         :around #'tempel--disable
;;         (apply fn args)
;;         (when (null tempel--active)
;;           (copy-region-as-kill (point-min) (point-max))
;;           (kill-buffer scratch)
;;           (switch-to-buffer old)
;;           (vterm-yank)
;;           (advice-remove 'tempel--disable 'my/tempel--disable)))))
;;     (call-interactively 'tempel-insert))

;; NOTE If you want normal org cycle behavior when using evil-org (subtree ->
;; contents -> collapse), uncomment this. Note that this only applies when the
;; evil module's +everywhere flag is enabled. Since I've disabled the flag and
;; ported evil-org's use-package and disabled this anyway, I'm archiving this
;; (remove-hook 'org-tab-first-hook '+org-cycle-only-current-subtree-h)

;;;; emacs lisp -------------------------------------------------

;; TODO Remove this if no longer needed. From the backup config.
;; (defvar +emacs-lisp-outline-regexp "[ \t]*;;;;* [^ \t\n]")

;; TODO Re-enable this if you switch from corfu to company
;; Due to alterations to private company module, emacs-lisp otherwise has
;; empty `company-backends'
;;(set-company-backend! 'emacs-lisp-mode '(company-capf))

org ----------------------------------------------------------------------

;; NOTE Line spacing pads the bottom of lines, while line height pads the top.
;; Both are meant to work with literal lines, not visual ones. Line height adds
;; padding only to the final visual line, which is definitely not desirable.
;; Line spacing is better but won't pad between visual lines at all. It also
;; seems like the effect is quite different for the same (floating point)
;; values for the two text properties.
;; (defun test ()
;;   (interactive)
;;   (add-text-properties (point-min) (point-max) '(line-spacing 0.5 line-height 0)))

org-superstar ------------------------------------------------------------
