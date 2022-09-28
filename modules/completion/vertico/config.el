;;; completion/vertico/config.el -*- lexical-binding: t; -*-

(defvar +vertico-company-completion-styles '(basic partial-completion orderless)
  "Completion styles for company to use.

The completion/vertico module uses the orderless completion style by default,
but this returns too broad a candidate set for company completion. This variable
overrides `completion-styles' during company completion sessions.")

(defvar +vertico-consult-fd-args nil
  "Shell command and arguments the vertico module uses for fd.")

;;
;;; Packages

(use-package! vertico
  :hook (doom-first-input . vertico-mode)
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defadvice! +vertico-crm-indicator-a (args)
    :filter-args #'completing-read-multiple
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  :config
  (setq vertico-resize nil
        vertico-count 17
        vertico-cycle t)
  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (when EMACS28+
    (setq read-extended-command-predicate #'command-completion-default-include-p))
  ;; For those not using corfu--setq-default ensures we don't mess with it
  ;; if you are--this allows for vertico-based completion when you press
  ;; M-TAB. See
  ;; https://with-emacs.com/posts/tutorials/customize-completion-at-point/
  ;; and the consult README
  ;; WARNING This does not work with LSP completion
  (setq-default completion-in-region-function
                (lambda (&rest args)
                  (apply (if vertico-mode
                             #'consult-completion-in-region
                           #'completion--in-region)
                         args)))
  ;; Cleans up path when moving directories with shadowed paths syntax, e.g.
  ;; cleans ~/foo/bar/// to /, and ~/foo/bar/~/ to ~/.
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  ;; For use with vertico-repeat-select and vertico-repeat-last
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (map! :map vertico-map "DEL" #'vertico-directory-delete-char)

  ;; These commands are problematic and automatically show the *Completions* buffer
  (advice-add #'tmm-add-prompt :after #'minibuffer-hide-completions)
  (defadvice! +vertico--suppress-completion-help-a (fn &rest args)
    "Make ffap-menu work with vertico"
    :around #'ffap-menu-ask
    (letf! ((#'minibuffer-completion-help #'ignore))
      (apply fn args))))


(use-package! orderless
  :after-call doom-first-input-hook
  :config
  (defadvice! +vertico--company-capf--candidates-a (fn &rest args)
    "Highlight company matches correctly, and try default completion styles before
orderless."
    :around #'company-capf--candidates
    (let ((orderless-match-faces [completions-common-part])
          (completion-styles +vertico-company-completion-styles))
      (apply fn args)))

  (defun +vertico-orderless-dispatch (pattern _index _total)
    (cond
     ;; Ensure $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" pattern)
      `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x200000-\x300000]*$")))
     ;; Ignore single !
     ((string= "!" pattern) `(orderless-literal . ""))
     ;; Without literal
     ((string-prefix-p "!" pattern) `(orderless-without-literal . ,(substring pattern 1)))
     ;; Character folding
     ((string-prefix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 1)))
     ((string-suffix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 0 -1)))
     ;; Initialism matching
     ((string-prefix-p "`" pattern) `(orderless-initialism . ,(substring pattern 1)))
     ((string-suffix-p "`" pattern) `(orderless-initialism . ,(substring pattern 0 -1)))
     ;; Literal matching
     ((string-prefix-p "=" pattern) `(orderless-literal . ,(substring pattern 1)))
     ((string-suffix-p "=" pattern) `(orderless-literal . ,(substring pattern 0 -1)))
     ;; Flex matching
     ((string-prefix-p "~" pattern) `(orderless-flex . ,(substring pattern 1)))
     ((string-suffix-p "~" pattern) `(orderless-flex . ,(substring pattern 0 -1)))))
  (add-to-list
   'completion-styles-alist
   '(+vertico-basic-remote
     +vertico-basic-remote-try-completion
     +vertico-basic-remote-all-completions
     "Use basic completion on remote files only"))
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        ;; note that despite override in the name orderless can still be used in
        ;; find-file etc.
        completion-category-overrides '((file (styles +vertico-basic-remote orderless partial-completion)))
        orderless-component-separator #'orderless-escapable-split-on-space)
  (add-hook 'orderless-style-dispatchers #'+vertico-orderless-dispatch)
  ;; ...otherwise find-file gets different highlighting than other commands
  (set-face-attribute 'completions-first-difference nil :inherit nil))


(use-package! consult
  :defer t
  :preface
  (define-key!
    [remap apropos]                       #'consult-apropos
    [remap bookmark-jump]                 #'consult-bookmark
    [remap evil-show-marks]               #'consult-mark
    [remap evil-show-jumps]               #'+vertico/jump-list
    [remap evil-show-registers]           #'consult-register
    [remap goto-line]                     #'consult-goto-line
    [remap imenu]                         #'consult-imenu
    [remap locate]                        #'consult-locate
    [remap load-theme]                    #'consult-theme
    [remap man]                           #'consult-man
    [remap recentf-open-files]            #'consult-recent-file
    [remap switch-to-buffer]              #'consult-buffer
    [remap switch-to-buffer-other-window] #'consult-buffer-other-window
    [remap switch-to-buffer-other-frame]  #'consult-buffer-other-frame
    [remap yank-pop]                      #'consult-yank-pop
    [remap persp-switch-to-buffer]        #'+vertico/switch-workspace-buffer)
  (advice-add #'multi-occur :override #'consult-multi-occur)
  :config
  (defadvice! +vertico--consult-recent-file-a (&rest _args)
    "`consult-recent-file' needs to have `recentf-mode' on to work correctly"
    :before #'consult-recent-file
    (recentf-mode +1))

  (setq consult-project-function #'doom-project-root
        consult-narrow-key "<"
        consult-line-numbers-widen t
        consult-async-min-input 2
        consult-async-refresh-delay  0.15
        consult-async-input-throttle 0.2
        consult-async-input-debounce 0.1
        consult-preview-key (if (display-graphic-p) (kbd "C-SPC") (kbd "C-@")))
  (when IS-MAC (setq consult-locate-args "mdfind"))
  (unless +vertico-consult-fd-args
    (setq +vertico-consult-fd-args
          (if doom-projectile-fd-binary
              (format "%s --color=never -i -H -E .git --regex %s"
                      doom-projectile-fd-binary
                      (if IS-WINDOWS "--path-separator=/" ""))
            consult-find-args)))
  ;; Preview themes on any key after 0.5s idle
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.5 any))
  (when (featurep! :lang org)
    ;; Pressing "o SPC" within consult-buffer will limit candidates to org buffers
    (defvar +vertico--consult-org-source
      (list :name     "Org Buffer"
            :category 'buffer
            :narrow   ?o
            :hidden   t
            :face     'consult-buffer
            :history  'buffer-name-history
            :state    #'consult--buffer-state
            :new
            (lambda (name)
              (with-current-buffer (get-buffer-create name)
                (insert "#+title: " name "\n\n")
                (org-mode)
                (consult--buffer-action (current-buffer))))
            :items
            (lambda ()
              (mapcar #'buffer-name
                      (if (featurep 'org)
                          (org-buffer-list)
                        (seq-filter
                         (lambda (x)
                           (eq (buffer-local-value 'major-mode x) 'org-mode))
                         (buffer-list)))))))
    (add-to-list 'consult-buffer-sources '+vertico--consult-org-source 'append)))

;; Switch to directory (consult-dir) and recursively find file within dir
;; (consult-dir-jump-file). In particular, use when within the minibuffer.
;; consult-dir will replace the prompt of any filepath-completing function
;; with the selected dir!
(use-package! consult-dir
  :bind (([remap list-directory] . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file))
  :config
  (when (featurep! :tools docker)
    (defun +vertico--consult-dir-docker-hosts ()
      "Get a list of hosts from docker."
      (when (require 'docker-tramp nil t)
        (let ((hosts)
              (docker-tramp-use-names t))
          (dolist (cand (docker-tramp--parse-running-containers))
            (let ((user (unless (string-empty-p (car cand))
                          (concat (car cand) "@")))
                  (host (car (cdr cand))))
              (push (concat "/docker:" user host ":/") hosts)))
          hosts)))

    (defvar +vertico--consult-dir-source-tramp-docker
      `(:name     "Docker"
        :narrow   ?d
        :category file
        :face     consult-file
        :history  file-name-history
        :items    ,#'+vertico--consult-dir-docker-hosts)
      "Docker candiadate source for `consult-dir'.")

    (add-to-list 'consult-dir-sources '+vertico--consult-dir-source-tramp-docker t))

  (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-ssh t)
  (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-local t))

(use-package! embark
  :defer t
  :init
  ;; https://github.com/hlissner/doom-emacs/issues/5564
  ;; Usage: Type any prefix followed by "C-h" (no which-key popup) or "C-h h"
  ;; (which-key popup visible) to invoke `embark-prefix-help-command'
  ;; (technically, `embark-bindings').
  (add-hook 'which-key-mode-hook
            (lambda () (setq which-key--prefix-help-cmd-backup
                             #'my/embark-prefix-help-command)))
  (advice-add #'which-key-show-standard-help :override #'my/which-key-show-standard-help)
  (advice-add #'embark-bindings :override #'my/embark-bindings)

  (map! [remap describe-bindings] #'embark-bindings
        "C-;"               #'embark-act  ; to be moved to :config default if accepted
        (:map minibuffer-local-map
         "C-;"               #'embark-act
         "C-c C-;"           #'embark-export
         "C-c C-l"           #'embark-collect
         :desc "Export to writable buffer" "C-c C-e" #'+vertico/embark-export-write)
        (:leader
         :desc "Actions" "a" #'embark-act)) ; to be moved to :config default if accepted
  :config
  (require 'consult)

  (set-popup-rule! "^\\*Embark Export:" :size 0.35 :ttl 0 :quit nil)

  ;; NOTE required after embark loads b/c embark uses with-eval-after-load to modify
  ;; embark-indicators when vertico is present
  (setq embark-prompter 'embark-completing-read-prompter
        embark-indicators '(embark-minimal-indicator embark-highlight-indicator))

  ;; Configure org-attach-open to work with embark:
  ;; 2. Transform marginalia category from `attach' to `file' and convert target to filepath
  (add-to-list 'embark-transformer-alist '(attach . embark--expand-attachment))

  ;; add the package! target finder before the file target finder,
  ;; so we don't get a false positive match.
  (let ((pos (or (cl-position
                  'embark-target-file-at-point
                  embark-target-finders)
                 (length embark-target-finders))))
    (cl-callf2
        cons
        '+vertico-embark-target-package-fn
        (nthcdr pos embark-target-finders)))
  (embark-define-keymap +vertico/embark-doom-package-map
    "Keymap for Embark package actions for packages installed by Doom."
    ("h" doom/help-packages)
    ("b" doom/bump-package)
    ("c" doom/help-package-config)
    ("u" doom/help-package-homepage))
  (setf (alist-get 'package embark-keymap-alist) #'+vertico/embark-doom-package-map)
  (map! (:map embark-file-map
         :desc "Open target with sudo"        "s"   #'doom/sudo-find-file
         (:when (featurep! :tools magit)
          :desc "Open magit-status of target" "g"   #'+vertico/embark-magit-status)
         (:when (featurep! :ui workspaces)
          :desc "Open in new workspace"       "TAB" #'+vertico/embark-open-in-new-workspace))))

;; NOTE Marginalia annotates minibuffer completions, but if a minibuffer-completion
;; command is not the top-level command executed, it might not be annotated
;; correctly. E.g. I modified +org/dwim-at-point to call org-attach-open. To
;; have these commands annotated correctly, you need to either rebind this-command
;; to the command you want annotated before executing it, or else execute it
;; via execute-extended-command; e.g.
;; (execute-extended-command nil "org-attach-open")

;; `org-attach-open' does not use the path returned by `org-attach-dir' as
;; minibuffer input. `embark--vertico-selected' constructs embark targets from
;; the candidate and the minibuffer input, so the target is not the full
;; path. By associating `org-attach-open' to a novel marginalia category, and
;; this category to an embark transformer function, we can execute actions from
;; `embark-file-map' on the full filepath of an attachment.

(use-package! marginalia
  :hook (doom-first-input . marginalia-mode)
  :init
  (map! :map minibuffer-local-map
        :desc "Cycle marginalia views" "M-A" #'marginalia-cycle)
  :config
  (when (featurep! +icons)
    (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup))
  (advice-add #'marginalia--project-root :override #'doom-project-root)
  (advice-add #'marginalia-annotate-binding :override #'my/marginalia-annotate-binding)
  (pushnew! marginalia-command-categories
            '(+default/find-file-under-here . file)
            '(doom/find-file-in-emacsd . project-file)
            '(doom/find-file-in-other-project . project-file)
            '(doom/find-file-in-private-config . file)
            '(doom/describe-active-minor-mode . minor-mode)
            '(flycheck-error-list-set-filter . builtin)
            '(persp-switch-to-buffer . buffer)
            '(projectile-find-file . project-file)
            '(projectile-recentf . project-file)
            '(projectile-switch-to-buffer . buffer)
            '(projectile-switch-project . project-file)
            ;; Configure org-attach-open to work with embark:
            ;; 1. Create a novel marginalia category
            '(org-attach-open . attach)))


(use-package! embark-consult
  :after (embark consult)
  :config
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))


(use-package! wgrep
  :commands wgrep-change-to-wgrep-mode
  :config (setq wgrep-auto-save-buffer t))


(use-package! vertico-posframe
  :when (featurep! +childframe)
  :hook (vertico-mode . vertico-posframe-mode)
  :config
  (add-hook 'doom-after-reload-hook #'posframe-delete-all))
