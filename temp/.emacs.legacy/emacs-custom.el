(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#657b83"])
 '(auto-save-default nil)
 '(backward-delete-char-untabify-method 'hungry)
 '(before-save-hook '(delete-trailing-whitespace))
 '(column-number-mode t)
 '(compilation-message-face 'default)
 '(confirm-kill-processes nil)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   '("3319c893ff355a88b86ef630a74fad7f1211f006d54ce451aab91d35d018158f" "5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea" "28a104f642d09d3e5c62ce3464ea2c143b9130167282ea97ddcc3607b381823f" "ea5822c1b2fb8bb6194a7ee61af3fe2cc7e2c7bab272cbb498a0234984e1b2d9" "0fffa9669425ff140ff2ae8568c7719705ef33b7a927a0ba7c5e2ffcfac09b75" "0f0a885f4ce5b6f97e33c7483bfe4515220e9cbd9ab3ca798e0972f665f8ee4d" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "dcdd1471fde79899ae47152d090e3551b889edf4b46f00df36d653adc2bf550d" "37f32706ffc6d7d021adf6b4d2a84eae7e0cfb7871cd39e21eaddc77c52bf4a7" "332fcf3c7208aca9fab65d54203f78a242482e7fd65f5725a2482c20b1730732" "35b0b0e531731e270708ddb342dc2e576a31fb298dcbc56a206596a43afac54f" "274fa62b00d732d093fc3f120aca1b31a6bb484492f31081c1814a858e25c72e" default))
 '(delete-by-moving-to-trash t)
 '(desktop-auto-save-timeout nil)
 '(desktop-load-locked-desktop t)
 '(desktop-path '("~/.emacs.d/"))
 '(desktop-restore-eager t)
 '(desktop-save t)
 '(display-buffer-base-action '((display-buffer-reuse-window display-buffer-same-window)))
 '(edebug-all-defs nil)
 '(eldoc-echo-area-use-multiline-p t nil nil "May not have an effect. Test on longer function signatures sometime.")
 '(electric-layout-mode t nil nil "Compare behavior with and without in ess-R. Without this, newlines within {} or () inserts two newlines between them, indents the first and moves cursor to it. Setting this seems to interfere with that behavior, which is controlled by ess-roxy-newline-and-indent. This is because ess-r-mode sets electric-layout-rules to insert a newline after {, but doesn't enable electric-layout-mode. Must be an oversight. If setting this globally, disable it in R to keep the desired behavior. ")
 '(electric-pair-mode t nil nil "Nice default behavior. Delete delimiter pair when deleting opening paren of empty pair (electric-pair-delete-adjacent-pairs). Skips over closing delim when you try to insert over an existing delim (electric-pair-skip-self). Inserts singles to preserve balance (electric-pair-preserve-balance). Insertion around active region, with point after whichever delim you typed--opening or closing. Example of cool defaults: In lisp comments, ` inserts `'.")
 '(electric-pair-pairs '((34 . 34) (8216 . 8217) (8220 . 8221) (123 . 125)))
 '(emacs-lisp-docstring-fill-column t)
 '(ess-R-font-lock-keywords
   '((ess-R-fl-keyword:keywords . t)
     (ess-R-fl-keyword:constants . t)
     (ess-R-fl-keyword:modifiers . t)
     (ess-R-fl-keyword:fun-defs . t)
     (ess-R-fl-keyword:assign-ops . t)
     (ess-R-fl-keyword:%op% . t)
     (ess-fl-keyword:fun-calls . t)
     (ess-fl-keyword:numbers . t)
     (ess-fl-keyword:operators . t)
     (ess-fl-keyword:delimiters . t)
     (ess-fl-keyword:= . t)
     (ess-R-fl-keyword:F&T . t)))
 '(ess-ask-for-ess-directory nil)
 '(ess-eldoc-abbreviation-style 'mild)
 '(ess-eldoc-show-on-symbol t)
 '(ess-eval-visibly nil)
 '(ess-indent-with-fancy-comments nil nil nil "TODO Test whether this was the cause of comments being forced to the right margin")
 '(ess-style 'RStudio)
 '(eval-expression-print-length nil)
 '(eval-expression-print-level nil)
 '(fci-rule-color "#073642")
 '(fill-column 80)
 '(global-display-fill-column-indicator-mode t)
 '(global-whitespace-mode t)
 '(highlight-changes-colors '("#d33682" "#6c71c4"))
 '(highlight-symbol-colors
   '("#3b6b40f432d6" "#07b9463c4d36" "#47a3341e358a" "#1d873c3f56d5" "#2d86441c3361" "#43b7362d3199" "#061d417f59d7"))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   '(("#073642" . 0)
     ("#5b7300" . 20)
     ("#007d76" . 30)
     ("#0061a8" . 50)
     ("#866300" . 60)
     ("#992700" . 70)
     ("#a00559" . 85)
     ("#073642" . 100)))
 '(hl-bg-colors
   '("#866300" "#992700" "#a7020a" "#a00559" "#243e9b" "#0061a8" "#007d76" "#5b7300"))
 '(hl-fg-colors
   '("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36"))
 '(hl-paren-colors '("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900"))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(make-backup-files nil)
 '(max-lisp-eval-depth 2000)
 '(max-specpdl-size 50000)
 '(menu-bar-mode t)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-tilt-scroll t)
 '(nrepl-message-colors
   '("#dc322f" "#cb4b16" "#b58900" "#5b7300" "#b3c34d" "#0061a8" "#2aa198" "#d33682" "#6c71c4"))
 '(outline-minor-mode-cycle t)
 '(outline-minor-mode-highlight 'override)
 '(recentf-max-saved-items 100 nil nil "Affects counsel-recentf. Possibly also ivy-switch-buffer with ivy-use-virtual-buffers enabled.")
 '(safe-local-variable-values
   '((eval add-hook 'after-save-hook
           (lambda nil
             (org-babel-tangle))
           nil t)))
 '(same-window-buffer-names '("*Help*"))
 '(scroll-bar-mode nil)
 '(scroll-conservatively 1000000 nil nil "Seems to prevent auto-recentering of point when scrolling")
 '(show-paren-mode t)
 '(tab-always-indent 'complete)
 '(tab-bar-close-button-show nil)
 '(tab-bar-new-tab-choice "*scratch*")
 '(tab-width 4)
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(text-mode-hook '(text-mode-hook-identify))
 '(tool-bar-mode nil)
 '(tricky 1)
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#dc322f")
     (40 . "#cb4366eb20b4")
     (60 . "#c1167942154f")
     (80 . "#b58900")
     (100 . "#a6ae8f7c0000")
     (120 . "#9ed892380000")
     (140 . "#96be94cf0000")
     (160 . "#8e5397440000")
     (180 . "#859900")
     (200 . "#77679bfc4635")
     (220 . "#6d449d465bfd")
     (240 . "#5fc09ea47092")
     (260 . "#4c68a01784aa")
     (280 . "#2aa198")
     (300 . "#303498e7affc")
     (320 . "#2fa1947cbb9b")
     (340 . "#2c879008c736")
     (360 . "#268bd2")))
 '(vc-annotate-very-old-color nil)
 '(vc-follow-symlinks t nil nil "I want to follow symlinks to the actual file even if it is
version-controlled. I don't know what benefit settings this to nil confers. I
simply wanted to avoid the prompt in org-mode, where I've set the default file
attachment method to be a symbolic link.")
 '(warning-suppress-types '((org-roam)))
 '(weechat-color-list
   '(unspecified "#002b36" "#073642" "#a7020a" "#dc322f" "#5b7300" "#859900" "#866300" "#b58900" "#0061a8" "#268bd2" "#a00559" "#d33682" "#007d76" "#2aa198" "#839496" "#657b83"))
 '(whitespace-display-mappings '((tab-mark 9 [124 9] [92 9])))
 '(whitespace-style '(face trailing tabs tab-mark newline-mark))
 '(winner-mode t)
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit aw-mode-line-face :height 3.0))))
 '(marginalia-documentation ((t (:inherit font-lock-doc-face))) nil "Inspired by ivy-rich"))
