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

    ;; See my/ess-display-help-on-object and my/company-show-doc-buffer. The R
    ;; language server (lsp-mode) returns strings without newlines.
    ;; company-box presents this as a single line. Even with code to wrap
    ;; box doc childframes, the forattming is a mess.
    (setq-hook! 'ess-r-mode-hook company-box-doc-enable nil))

  (set-repl-handler! 'ess-r-mode #'+ess/open-r-repl :persist t)
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

  (setq-hook! 'inferior-ess-mode-hook comint-use-prompt-regexp nil)

  ;; The ess completion function doesn't invoke company as far as I can tell
  (general-unbind ess-mode-map "TAB")

  ;; NOTE: The map for help with R objects (e.g., see C-c C-v and compare this
  ;; to K, which opens up an (lsp?) help-mode buffer)
  (map! (:after ess-help
         ;; NOTE: Some of these are shadowed by evil for some reason, at least
         ;; with popup enabled. This seems to be a common problem for modules
         ;; where Doom binds evil states within maps
         (:map ess-help-mode-map
          :n "q"  #'kill-current-buffer
          :n "Q"  #'ess-kill-buffer-and-go
          :n "K"  #'ess-display-help-on-object
          :n "go" #'ess-display-help-in-browser
          :n "gO" #'ess-display-help-apropos
          :n "gv" #'ess-display-vignettes
          :m "]]" #'ess-skip-to-next-section
          :m "[[" #'ess-skip-to-previous-section
          ;; NOTE: Doom bound this to ess-doc-map by mistake
          [C-return] #'ess-eval-line)
         (:map ess-doc-map
          "h"    #'ess-display-help-on-object
          "p"    #'ess-R-dv-pprint
          "t"    #'ess-R-dv-ctable))
        :localleader
        "," #'ess-eval-region-or-function-or-paragraph-and-step
        "'" #'R
        ;; TODO: Shouldn't this also be bound in the inferior buffer? I've
        ;; noticed those don't bind to localleader
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

;; NOTE Code below kept for demonstration of making local copies of
;; a keymap. Also see https://stackoverflow.com/questions/13102494/buffer-locally-overriding-minor-mode-key-bindings-in-emacs
;; for minor mode keymaps.
;; (setq essrca-map (make-sparse-keymap))
;; (set-keymap-parent essrca-map company-active-map)
;; (define-key essrca-map (kbd "C-h") #'show-company-doc-as-ess-help)
;; (add-hook 'ess-r-mode-hook
;;           (lambda ()
;;             (make-local-variable 'company-active-map)
;;             (setq company-active-map essrca-map)))

;;; Leftovers from vanilla Emacs config

  ;; TODO Leftover variable values from vanilla emacs config
  ;;  electric-pair-pairs '((34 . 34) (8216 . 8217) (8220 . 8221) (123 . 125))
  ;; ;; Compare behavior with and without in ess-R. Without this, newlines within {}
  ;; ;; or () inserts two newlines between them, indents the first and moves cursor
  ;; ;; to it. Setting this seems to interfere with that behavior, which is
  ;; ;; controlled by ess-roxy-newline-and-indent. This is because ess-r-mode sets
  ;; ;; electric-layout-rules to insert a newline after {, but doesn't enable
  ;; ;; electric-layout-mode. Must be an oversight. If setting this globally, disable
  ;; ;; it in R to keep the desired behavior.
  ;; electric-layout-mode t

  ;; Override Windows' help_type option of "html", to open help in help buffer,
  ;; not browser (see contents of .Rprofile)
  ;; (pcase system-type
  ;;   ('windows-nt
  ;;    ;; iESS searches the paths listed in the variable exec-path for inferior-ess-r-program
  ;;    (add-to-list 'exec-path "c:/Users/jkroes/Documents/R/R-3.6.2/bin")
  ;;    ;; Sets R_USER and R_LIBS_USER
  ;;    (setenv "R_USER" "c:/Users/jkroes/Documents")
  ;;    ;; run-ess-r fails when this is set to Rterm
  ;;    (setq inferior-ess-r-program "R")
  ;;    (setenv "R_PROFILE_USER" "C:/Users/jkroes/.emacs.d/.Rprofile")
  ;;    ;; RStudio downloads pandoc with rmarkdown, but outside of RStudio
  ;;    ;; you need to notify R of the executable's directory
  ;;    (setenv "RSTUDIO_PANDOC" "C:/Users/jkroes/AppData/Local/Pandoc"))
  ;;   ('darwin (setenv "R_PROFILE_USER" (concat user-emacs-directory ".Rprofile"))))

  ;; Disabling this while I render Word documents from Rmarkdown.
  ;; NOTE: Doom doesn't use customize-package. This is from my vanilla config.
  ;;(customize-package '(polymode-display-output-file nil))

  ;; TODO Test these out with voc reports. Packages need to be added to packages.el
  ;;(use-package 'poly-markdown)
  ;; NOTE: ess-r configuration and bindings are available inside chunks, where R-mode is active
  ;; I have bound polymode-export (render) to SPC-m-e-k
  ;;(use-package 'poly-R)

  ;; Prevent window displaying company documentation buffer from vanishing when
  ;; invoking a binding not in company--electric-commands
  ;; (defun forget-saved-window-config ()
  ;;   (setq company--electric-saved-window-configuration nil))
  ;; (advice-add 'company-pre-command :before 'forget-saved-window-config)

  ;; TODO Get this working in Doom. Do a search. I believe popup overwrites this.
  ;; (setq display-buffer-alist
  ;;       `(("\\*company-documentation\\*"
  ;;          (display-buffer-reuse-mode-window display-buffer-in-side-window)
  ;;          (mode. ess-r-help-mode)
  ;;          (side . right)
  ;;          (slot . 1)
  ;;          (window-width . 0.33)
  ;;          (reusable-frames . nil))
  ;;         ("\\*R Dired"
  ;;          (display-buffer-reuse-mode-window display-buffer-in-side-window)
  ;;          (side . right)
  ;;          (slot . -1)
  ;;          (window-width . 0.5)
  ;;          (reusable-frames . nil))
  ;;         ("\\*R"
  ;;          (display-buffer-reuse-mode-window display-buffer-below-selected)
  ;;          (window-height . 0.2)
  ;;          (reusable-frames . nil))
  ;;         ("\\*Help\\[R"
  ;;          (display-buffer-reuse-mode-window display-buffer-in-side-window)
  ;;          (side . right)
  ;;          (slot . 1)
  ;;          (window-width . 0.5)
  ;;          (reusable-frames . nil))
  ;;         ;; ("\\*Help\\*" display-buffer-same-window)
  ;;         ;; ("\\*Apropos\\*" display-buffer-same-window)
  ;;         )
  ;;       )


  ;; ;;(my-leader :keymaps 'ess-r-mode-map "m" 'hydra-r/body)
  ;; (defhydra hydra-r (:color pink)
  ;;   "R"
  ;;   ("d" hydra-r-debug/body :color blue)
  ;;   ("e" hydra-r-eval/body :color blue)
  ;;   ("h" hydra-r-help/body :color blue)
  ;;   ("r" my/start-r :color blue)
  ;;   ("s" ess-switch-to-inferior-or-script-buffer :color blue)
  ;;   ("z" ess-submit-bug-report :color blue)
  ;;   ;; prog-indent-sexp
  ;;   ;; ess-indent-exp
  ;;   ;; ess-indent-new-comment-line
  ;;   ;; ess-complete-object-name
  ;;   ("q" nil))

  ;; (defhydra hydra-r-help (:color pink) ; ess-doc-map
  ;;   "R-help"
  ;;   ("a" ess-display-help-apropos)
  ;;   ("e" hydra-r-eval/body :color blue)
  ;;   ("i" ess-display-package-index)
  ;;   ("m" ess-manual-lookup)
  ;;   ("o" ess-display-help-on-object)
  ;;   ("p" ess-describe-object-at-point)
  ;;   ("r" hydra-r/body :color blue)
  ;;   ("t" ess-display-demos)
  ;;   ("v" ess-display-vignettes)
  ;;   ("w" ess-help-web-search)
  ;;   ("q" nil))

  ;; (defhydra hydra-r-eval (:color pink) ; ess-rutils-map and ess-extra-map
  ;;   "R-eval"
  ;;   ("<C-return>" ess-eval-region-or-function-or-paragraph-and-step)
  ;;   ("RET" ess-eval-region-or-line-and-step)
  ;;   ("b" ess-eval-buffer-from-beg-to-here)
  ;;   ("e" ess-eval-buffer-from-here-to-end)
  ;;   ("E" ess-dirs)
  ;;   ("f" ess-load-file)
  ;;   ("i" inferior-ess-reload)
  ;;   ;; ("P" ess-request-a-process) ;; Display selected iESS process and buffer
  ;;   ("p" ess-switch-process) ;; Switch process attached to script (current process buffer auto-displays if new,
  ;;   ;; but any script evaluation will auto-display attached process buffer if not already visible
  ;;   ("s" ess-switch-to-inferior-or-script-buffer)
  ;;   ("r" hydra-r/body :color blue)
  ;;   ("R" ess-rdired)
  ;;   ("u" ess-use-this-dir)
  ;;   ("w" ess-change-directory)
  ;;   ("q" nil))

  ;; ;; (defhydra+ hydra-r-eval()
  ;; ;;   ("k" polymode-export :color blue))

  ;; ;; Note that several commands available in the inferior ess R
  ;; ;; process while debugging are absent:
  ;; ;; f (finish)
  ;; ;; s (step)
  ;; ;; help
  ;; ;; where
  ;; ;; <expr>
  ;; ;; As such, it is best to debug from the inferior process where
  ;; ;; the additional, built-in functionality is needed
  ;; ;; TODO: Add commands here to ess-debug-minor-mode-map
  ;; (defhydra hydra-r-debug (:color pink) ;; ess-debug-minor-mode-map and ess-dev-map
  ;;   "R-debug"
  ;;   ("c" ess-debug-command-continue)
  ;;   ("f" ess-debug-flag-for-debugging) ;; base:::debug()
  ;;   ("F" ess-debug-unflag-for-debugging) ;; base:::undebug()
  ;;   ("g" ess-debug-goto-debug-point)
  ;;   ("n" ess-debug-command-next)
  ;;   ("N" next-error)
  ;;   ("p" previous-error)
  ;;   ("q" ess-debug-command-quit :color blue) ;; Investigate diff b/w this and ess-debug-stop
  ;;   ("Q" ess-debug-stop :color blue)
  ;;   ("s" ess-switch-to-ESS :color blue)
  ;;   ;; ("t" ess-debug-toggle-error-action) ;; Sets value of error option (e.g. options(error=recover)) for active process
  ;;   ;; ("u" ess-debug-command-up) ;; NOTE: currently broken. Use recover() from within debugging session (i.e. browse())
  ;;   ;; ess-debug-goto-input-event-marker
  ;;   ;; ess-debug-insert-in-forward-ring
  ;;   ("q" nil))

  ;; (defun my/start-r ()
  ;;   "Start an R process."
  ;;   (interactive)
  ;;   (save-selected-window
  ;;     (run-ess-r)
  ;;     ;;(ess-rdired)
  ;;     )
  ;;   (ess-force-buffer-current))
