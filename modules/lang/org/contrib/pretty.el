;;; lang/org/contrib/pretty.el -*- lexical-binding: t; -*-
;;;###if (modulep! +pretty)
(after! org
  (setq org-highlight-latex-and-related '(native script entities)))

;; NOTE As of 4/16/23, I switched from org-visual-indent and
;; org-dynamic-bullets to org-superstar. Commits prior to
;; this time contain code for the deprecated packages in
;; modules/lang/org/contrib/pretty.el.

;; NOTE Three configurations for hiding leading stars on org headings:
;;
;; 1. Enable `org-superstar-remove-leading-stars' to hide leading stars and
;; hide the "indentation" from those characters
;;
;; 2. Disable `org-superstar-remove-leading-stars' and enable
;; `org-hide-leading-stars' to apply the `org-hide' face to leading stars,
;; which might require customization depending on your theme. The intention is
;; for this face's foreground to match the background. Note that stars will be
;; visible when `hl-line-mode' is enabled or the cursor is on a star.
;;
;; 3. Disable `org-superstar-remove-leading-stars' and `org-hide-leading-stars'
;; and set `org-superstar-leading-bullet' to `?\s' to preserve all indentation
;; but still hide leading stars. If `org-indent-mode' is enabled, you also
;; need to disable `org-indent-mode-turns-on-hiding-stars' to disable
;; `org-hide-leading-stars'.

;; NOTE See `org-superstar-restart' for enabling changes made on the fly

;; (use-package! org-superstar ; "prettier" bullets
;;   :hook (org-mode . org-superstar-mode)
;;   :config
;;   ;; Make leading stars truly invisible, by rendering them as spaces!
;;   (setq org-superstar-leading-bullet ?\s
;;         org-superstar-leading-fallback ?\s
;;         org-superstar-remove-leading-stars nil
;;         org-superstar-headline-bullets-list '(?\s ?\s ?\s ?\s)
;;         org-superstar-special-todo-items t
;;         org-superstar-todo-bullet-alist
;;         '(("TODO" . ?\s)
;;           ("DONE" . ?☑))))

;; NOTE Uncomment this if you disable org-superstar-remove-leading-stars and
;; enable org-hide-leading-stars. It works in either situation, but I figured
;; it was best to comment it out to reduce the overhead.
;;
;; (setq hl-line-range-function #'my/hl-line-skip-org-hide-stars)
;; (defun my/hl-line-skip-org-hide-stars ()
;;   "Don't apply the `hl-line' overlay to org heading stars. Note
;; that leading stars are still visible via the cursor."
;;   (if (and hl-line-mode
;;            (eq major-mode 'org-mode)
;;            (not (bound-and-true-p org-superstar-remove-leading-stars))
;;            org-hide-leading-stars
;;            (org-at-heading-p))
;;       (cons (+ (line-beginning-position) (1+ (org-current-level)))
;;             (line-beginning-position 2))
;;     (cons (line-beginning-position)
;;           (line-beginning-position 2))))


(use-package! org-fancy-priorities ; priority icons
  :hook (org-mode . org-fancy-priorities-mode)
  :hook (org-agenda-mode . org-fancy-priorities-mode)
  :config (setq org-fancy-priorities-list '("⚑" "⬆" "■")))


(use-package! org-appear ; better markup edit
  :hook (org-mode . org-appear-mode))
