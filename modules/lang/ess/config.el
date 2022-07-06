;;; lang/ess/config.el -*- lexical-binding: t; -*-

(after! projectile
  (add-to-list 'projectile-project-root-files "DESCRIPTION"))


;;
;;; Packages

(use-package! ess
  :commands stata SAS
  :hook
  ;; NOTE This isn't necessary when using the ligatures feature
  (ess-mode . prettify-symbols-mode) ; pretty ligatures
  ;; Nice default behavior. Delete delimiter pair when deleting opening paren of
  ;; empty pair (electric-pair-delete-adjacent-pairs). Skips over closing delim
  ;; when you try to insert over an existing delim (electric-pair-skip-self).
  ;; Inserts singles to preserve balance (electric-pair-preserve-balance).
  ;; Insertion around active region, with point after whichever delim you
  ;; typed--opening or closing. Example of cool defaults: In lisp comments, `
  ;; inserts `'.")
  (ess-r-mode . electric-pair-mode)
  :init
  (unless (featurep! :lang julia)
    (add-to-list 'auto-mode-alist '("\\.jl\\'" . ess-julia-mode)))
  :config
  (setq ess-offset-continued 'straight
        ess-use-flymake (not (featurep! :checkers syntax))
        ess-nuke-trailing-whitespace-p t
        ess-style 'RStudio
        ess-history-directory (expand-file-name "ess-history/" doom-cache-dir)
        ess-eldoc-abbreviation-style 'mild
        ess-eldoc-show-on-symbol t
        ess-ask-for-ess-directory nil
        ess-indent-with-fancy-comments nil
        ess-use-company nil
        ;; Prefer lsp to eldoc signatures for ess-r-mode. Better to have
        ;; barebones signatures for S3 generic functions like print and
        ;; data.frame, and some tidyverse functions like select(), than to
        ;; haphazardly mix arguments from different S3 methods and use that for
        ;; all object types. (Maybe I just don't understand ess-r-mode's
        ;; signatures.) Either way, enable only one. Otherwise, there are two
        ;; signatures.
        ess-use-eldoc (not (featurep! :tools lsp)))

  (set-docsets! 'ess-r-mode "R")

  ;; NOTE: lsp-enable-snippet has no effect on ess-r-mode. Unclear if a bug or
  ;; intended behavior. Compare to behavior in e.g. python-mode.
  (when (featurep! +lsp)
    (add-hook 'ess-r-mode-local-vars-hook #'lsp! 'append)
    ;; HACK until ess-r's lsp documentation gets fixed, don't use any lsp
    ;; lookup functions
    ;; NOTE I would prefer to only disable the document function, but I couldn't
    ;; figure out how to do that
    (add-hook 'ess-r-mode-hook
              (lambda ()
                (make-local-variable 'lsp-mode-hook)
                (remove-hook 'lsp-mode-hook #'+lookup--init-lsp-mode-handlers-h t)))

    ;; See my/ess-display-help-on-object and my/company-show-doc-buffer. The R
    ;; language server (lsp-mode) returns strings without newlines.
    ;; company-box presents this as a single line. Even with code to wrap
    ;; box doc childframes, the forattming is a mess.
    (setq-hook! 'ess-r-mode-hook company-box-doc-enable nil))

  (set-repl-handler! 'ess-r-mode #'+ess/open-r-repl)
  (set-repl-handler! 'ess-julia-mode #'+ess/open-julia-repl)
  ;; This is overridden by lsp-mode. "K" will open an lsp-help buffer
  (set-lookup-handlers! '(ess-r-mode ess-julia-mode)
    :documentation #'ess-display-help-on-object)

  (set-evil-initial-state! 'ess-r-help-mode 'normal)
  (set-eval-handler! 'ess-help-mode #'ess-eval-region-and-go)
  (set-eval-handler! 'ess-r-help-mode #'ess-eval-region-and-go)

  ;; NOTE Uncomment to use ess-r's company backends instead of lsp and
  ;; company-capf.
  ;; (setq-hook! 'ess-r-mode-hook
  ;;   lsp-completion-provider :none)
  ;; (set-company-backend! 'ess-r-mode (car ess-r-company-backends))

  (setq-hook! 'ess-r-mode-hook
    ;; HACK Fix #2233: Doom continues comments on RET, but ess-r-mode doesn't
    ;;      have a sane `comment-line-break-function', so...
    comment-line-break-function nil)

  (map! (:after ess-help
          (:map ess-help-mode-map
            :n "q"  #'kill-current-buffer
            :n "Q"  #'ess-kill-buffer-and-go
            :n "K"  #'ess-display-help-on-object
            :n "go" #'ess-display-help-in-browser
            :n "gO" #'ess-display-help-apropos
            :n "gv" #'ess-display-vignettes
            :m "]]" #'ess-skip-to-next-section
            :m "[[" #'ess-skip-to-previous-section)
          (:map ess-doc-map
            "h"    #'ess-display-help-on-object
            "p"    #'ess-R-dv-pprint
            "t"    #'ess-R-dv-ctable
            [up]   #'comint-next-input
            [down] #'comint-previous-input
            [C-return] #'ess-eval-line))

        :map ess-mode-map
        :n [C-return] #'ess-eval-line
        :localleader
        "," #'ess-eval-region-or-function-or-paragraph-and-step
        "'" #'R
        [tab]     #'ess-switch-to-inferior-or-script-buffer
        [backtab] #'ess-switch-process
        ;; REPL
        "B" #'ess-eval-buffer-and-go
        "b" #'ess-eval-buffer
        "d" #'ess-eval-region-or-line-and-step
        "D" #'ess-eval-function-or-paragraph-and-step
        "L" #'ess-eval-line-and-go
        "l" #'ess-eval-line
        "R" #'ess-eval-region-and-go
        "r" #'ess-eval-region
        "F" #'ess-eval-function-and-go
        "f" #'ess-eval-function
        ;; predefined keymaps
        "h" 'ess-doc-map
        "x" 'ess-extra-map
        "p" 'ess-r-package-dev-map
        "v" 'ess-dev-map
        ;; noweb
        :prefix "c"
        "C" #'ess-eval-chunk-and-go
        "c" #'ess-eval-chunk
        "d" #'ess-eval-chunk-and-step
        "m" #'ess-noweb-mark-chunk
        "p" #'ess-noweb-previous-chunk
        "n" #'ess-noweb-next-chunk))


(use-package! stan-mode
  :when (featurep! +stan)
  :hook (stan-mode . stan-mode-setup)
  :hook (stan-mode . eldoc-stan-setup)
  :init
  (use-package! company-stan
    :when (featurep! :completion company)
    :hook (stan-mode . company-stan-setup))

  (use-package! flycheck-stan
    :when (featurep! :checkers syntax)
    :hook (stan-mode . flycheck-stan-stanc2-setup)
    :hook (stan-mode . flycheck-stan-stanc3-setup)))
