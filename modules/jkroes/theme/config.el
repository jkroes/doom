;;; -*- lexical-binding: t; -*-

(defun jkroes/startup-font-size ()
"On startup set the font to a smaller size if working on the
 laptop without external monitors"
  (pcase (display-monitor-attributes-list)
    ;; dual monitor setup
    ((pred (lambda (dmal)
             ;; TODO Font size should depend on monitor resolution
             ;; E.g. for 4096x1152 a font size of 16 is
             ;; preferable. Use the largest monitor's size, then
             ;; eliminate dependence on number of monitors (i.e.,
             ;; length of `dmal')
             (length= dmal 2))) 18)
    ;; macbook air (m3, 2024)
    ((pred (lambda (dmal)
             (and (length= dmal 1)
                  (equal (last (caar dmal) 2) '(1470 956))))) 14)))

;; NOTE This package is built-in but differs from the external package. If
;; switching to the built-in, you will need to translate the configuration.
(use-package! modus-themes
  :defer t
  :init
  ;; Render docstrings and comments in italic
  (setq modus-themes-italic-constructs t)

  ;; Customize org-mode title and heading size. Note that H9 is
  ;; possible, but it will have the same settings as for H1, H10
  ;; as for H2, etc.
  (setq modus-themes-headings '((0 3.0)
                                (1 1.2)
                                (2 1.2)
                                (3 1.2)
                                (4 1.2)
                                (5 1.2)
                                (6 1.2)
                                (7 1.2)
                                (8 1.2)))

  ;; Make the fringe invisible
  (setq modus-themes-common-palette-overrides
        '((fringe unspecified)
          ;; Make display-line-numbers less conspicuous
          (fg-line-number-inactive "gray50")
          (fg-line-number-active fg-main)
          (bg-line-number-inactive unspecified)
          (bg-line-number-active unspecified))))

;; Modus themes recommend dimmer with these settings in lieu of solaire-mode
(use-package! dimmer
  :hook (doom-first-input . dimmer-mode)
  :init
  (setq dimmer-fraction 0.3)
  (setq dimmer-adjustment-mode :foreground)
  (setq dimmer-use-colorspace :rgb)
  (add-hook 'ediff-before-setup-hook (cmd! (dimmer-mode -1)))
  (add-hook 'ediff-quit-hook (cmd! (dimmer-mode +1)))
  :config
  ;; See https://github.com/gonewest818/dimmer.el/issues/49. This solution
  ;; prevents dimming when e.g. vertico and corfu are active.
  (advice-add
   'dimmer-config-change-handler :override
   #'jkroes/instead-force-only-if-predicates-falsy)

  ;; TODO Configure locations where dimming should not take place as you
  ;; discover more of them
  (dimmer-configure-which-key)
  (dimmer-configure-org)
  (jkroes/dimmer-configure-embark)
  (jkroes/dimmer-configure-corfu))
