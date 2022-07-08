;;; lang/org/contrib/pretty.el -*- lexical-binding: t; -*-
;;;###if (featurep! +pretty)
(after! org
  (setq org-highlight-latex-and-related '(native script entities)))


(use-package! org-appear
  :defer t
  :init
  (setq org-hide-emphasis-markers t
        org-appear-autoemphasis t
        org-pretty-entities t
        org-appear-autoentities t
        org-link-descriptive t
        org-appear-autolinks t
        ;; TODO Can't get this working
        org-appear-autosubmarkers t
        ;; Toggle org-appear off after 1-second idle over an element
        org-appear-trigger #'always
        org-appear-delay 1)
  :hook (org-mode . org-appear-mode))

(setq org-hide-leading-stars nil
      org-startup-indented nil)

(use-package! org-visual-indent
  :hook (org-mode . org-visual-indent-mode))

(use-package! org-dynamic-bullets
  :hook (org-mode . org-dynamic-bullets-mode))

;; HACK org versions later than at least 9.5.1 result in an error
;; for org-dynamic-bullets--body-p. Seems there may have been
;; breaking changes to the org-element library.
(after! org-dynamic-bullets
  (defun org-dynamic-bullets--body-p ()
    "Does the current heading have text in its body? \"Body\" is
defined as any text, including property drawers, following
the heading and before the next heading."
    (plist-get (cadr (org-element-at-point)) :contents-begin)))

;;; Old notes that might or might not be applicable
;; With this enabled, the cursor moves offscren to the right when not using
;; visual-line-mode. See
;; https://www.reddit.com/r/orgmode/comments/dri5rc/cursor_not_visible_at_the_end_of_line/.
;; NOTE: If using just org-indent-mode instead of this, you may see not all
;; leading stars are hidden. See
;; https://github.com/integral-dw/org-superstar-mode/issues/21
