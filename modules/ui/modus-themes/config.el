;;; ui/modus-themes/config.el -*- lexical-binding: t; -*-

;; NOTE When variable pitch is enabled, indentation will appear
;; not to line up when inserting a line after a list item. This
;; is because the width of a space is different than that of a
;; hyphen.
;;
;; Roboto is nearly identical to SF Pro but slightly
;; slimmer. These fonts are what Obsidian.md uses on MacOS.
;; (setq doom-variable-pitch-font
;;       (font-spec :family "Roboto"
;;                  :size (if (length= (display-monitor-attributes-list) 2) 20 18)))


(use-package modus-themes
  :config
  (setq doom-theme 'modus-vivendi)

  ;; Configure certain faces like org code blocks to inherit from
  ;; the `fixed-pitch' face when `variable-pitch-mode' is enabled
  ;; in a given mode. A non-modus example of how to do this is
  ;; shown in
  ;; https://zzamboni.org/post/beautifying-org-mode-in-emacs/.
  ;; See also org-variable-pitch.el
  (when doom-variable-pitch-font (setq modus-themes-mixed-fonts t))

  ;; Render docstrings and comments in italic
  (setq modus-themes-italic-constructs t)

  ;; Different backgrounds for org-src blocks with different
  ;; languages. gray-background fontifies org-block-begin-line
  ;; and org-block-end-line, which bleeds out onto other lines
  ;; when folded, even if these faces are not extended.
  (setq modus-themes-org-blocks 'tinted-background)

  ;; Additional languages that should use code block highlighting
  (after! org-src
    (push '("ruby" modus-themes-nuanced-cyan)
          org-src-block-faces))

  ;; Make the region (marked text) preserve the underlying text
  ;; colors; experiment with different backgrounds
  ;; (setq modus-themes-common-palette-overrides
  ;;       '((bg-region bg-ochre) ; try to replace `bg-ochre' with `bg-lavender', `bg-sage'
  ;;         (fg-region unspecified)))

  ;; Customize org-mode title and heading size. Note that H9 is
  ;; possible, but it will have the same settings as for H1, H10
  ;; as for H2, etc.
  (setq modus-themes-headings '((0 2.0)
                                (1 1.2)
                                (2 1.2)
                                (3 1.2)
                                (4 1.2)
                                (5 1.2)
                                (6 1.2)
                                (7 1.2)
                                (8 1.2)))

  ;; Pad the mode line vertically
  (defun my-modus-themes-custom-faces ()
    (modus-themes-with-colors
      (custom-set-faces
       `(mode-line ((,c :box (:line-width 4 :color ,bg-mode-line-active))))
       `(mode-line-inactive ((,c :box (:line-width 4 :color ,bg-mode-line-inactive)))))))

  (add-hook 'modus-themes-after-load-theme-hook #'my-modus-themes-custom-faces))
