;;; completion/vertico/config.el -*- lexical-binding: t; -*-

(defvar +vertico-company-completion-styles '(basic partial-completion orderless)
  "Completion styles for company to use.

The completion/vertico module uses the orderless completion style by default,
but this returns too broad a candidate set for company completion. This variable
overrides `completion-styles' during company completion sessions.")

(defvar +vertico-consult-dir-container-executable "docker"
  "Command to call for listing container hosts.")

(defvar +vertico-consult-dir-container-args nil
  "Command to call for listing container hosts.")

;;
;;; Packages

;; List of minibuffer keys:
;;
;;   consult-history (C-s): Insert candidate from history
;;   yank (C-y)
;;   yank-pop (M-y)
;;   move-end-of-line (C-e)
;;   move-beginning-of-line (C-a)
;;   delete-char/delete-forward-char (C-d / <deletechar> or <kp-delete>)
;;   evil-delete-back-to-indentation (C-u)
;;   universal-argument (M-u)
;;   vertico-directory-delete-char (DEL)
;;   undo (C-z)
;;   vertico-last (M->): Jump to last candidate
;;   vertico-first (M-<)
;;   vertico-next (C-j)
;;   vertico-previous (C-k)
;;   vertico-scroll-up (C-n)
;;   vertico-scroll-down (C-p)
;;   vertico-next-group (M-}, M-j)
;;   vertico-previous-group (M-{, M-k)
;;   vertico-exit (RET): Select candidate and exit
;;   vertico-save (M-w): Copy the selected candidate
;;   vertico-exit-input (M-RET): Exit with minibuffer text selected
;;   vertico-insert (TAB): Insert selected candidate into minibuffer. Compare to `embark-select'

(use-package! vertico
  :hook (doom-first-input . vertico-mode)
  :init
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

  ;; Hide commands in M-x which do not work in the current mode
  (setq read-extended-command-predicate #'command-completion-default-include-p)

  (setq-default completion-in-region-function
                (lambda (&rest args)
                  (apply (if vertico-mode
                             #'consult-completion-in-region
                           #'completion--in-region)
                         args)))

  (map! :when (modulep! :editor evil)
        :map vertico-map
        ;; "C-SPC" #'+vertico/embark-preview
        "C-j"   #'vertico-next
        "M-j" #'vertico-next-group
        ;; Shadows `kill-line', but S-<backspace> and C-S-<backspace> are still available
        "C-k"   #'vertico-previous
        "M-k" #'vertico-previous-group)

  ;; Cleans up path when moving directories with shadowed paths syntax, e.g.
  ;; cleans ~/foo/bar/// to /, and ~/foo/bar/~/ to ~/.
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (map! :map vertico-map "DEL" #'vertico-directory-delete-char)

  ;; These commands are problematic and automatically show the *Completions* buffer
  (advice-add #'tmm-add-prompt :after #'minibuffer-hide-completions)
  (defadvice! +vertico--suppress-completion-help-a (fn &rest args)
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
        completion-category-overrides '((file (styles +vertico-basic-remote orderless partial-completion))
                                        (project-file (styles +vertico-basic-remote orderless partial-completion)))
        orderless-style-dispatchers '(jkroes/orderless-dispatch)
        orderless-component-separator #'orderless-escapable-split-on-space)
  ;; ...otherwise find-file gets different highlighting than other commands
  (set-face-attribute 'completions-first-difference nil :inherit nil))


(use-package! consult
  :defer t
  :preface
  (define-key!
    [remap bookmark-jump]                 #'consult-bookmark
    [remap evil-show-marks]               #'consult-mark
    [remap evil-show-jumps]               #'+vertico/jump-list
    [remap evil-show-registers]           #'consult-register
    [remap goto-line]                     #'consult-goto-line
    [remap imenu]                         #'consult-imenu
    [remap Info-search]                   #'consult-info
    [remap locate]                        #'consult-locate
    [remap load-theme]                    #'consult-theme
    [remap man]                           #'consult-man
    [remap recentf-open-files]            #'consult-recent-file
    [remap switch-to-buffer]              #'consult-buffer
    [remap switch-to-buffer-other-window] #'consult-buffer-other-window
    [remap switch-to-buffer-other-frame]  #'consult-buffer-other-frame
    [remap yank-pop]                      #'consult-yank-pop
    [remap persp-switch-to-buffer]        #'+vertico/switch-workspace-buffer)
  :config
  (defadvice! +vertico--consult-recentf-a (&rest _args)
    "`consult-recent-file' needs to have `recentf-mode' on to work correctly.
`consult-buffer' needs `recentf-mode' to show file candidates."
    :before (list #'consult-recent-file #'consult-buffer)
    (recentf-mode +1))

  (setq consult-project-function #'doom-project-root
        consult-narrow-key "<"
        consult-line-numbers-widen t
        consult-async-min-input 2
        consult-async-refresh-delay  0.15
        consult-async-input-throttle 0.2
        consult-async-input-debounce 0.1
        consult-fd-args
        '((if (executable-find "fdfind" 'remote) "fdfind" "fd")
          "--color=never"
          ;; https://github.com/sharkdp/fd/issues/839
          "--full-path --absolute-path"
          "--hidden --exclude .git"
          (if (featurep :system 'windows) "--path-separator=/")))

  ;; Use Spotlight as the backend for locate on macOS
  (when (featurep :system 'macos) (setq consult-locate-args "mdfind"))

  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key "C-SPC")
  (when (modulep! :config default)
    (consult-customize
     +default/search-project +default/search-other-project
     +default/search-project-for-symbol-at-point
     +default/search-cwd +default/search-other-cwd
     +default/search-notes-for-symbol-at-point
     +default/search-emacsd
     :preview-key "C-SPC"))
  (consult-customize
   consult-theme
   :preview-key (list "C-SPC" :debounce 0.5 'any))
  (when (modulep! :lang org)
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


(use-package! consult-dir
  :defer t
  :init
  (map! [remap list-directory] #'consult-dir
        (:after vertico
         :map vertico-map
         "C-x C-d" #'consult-dir
         "C-x C-j" #'consult-dir-jump-file))
  :config
  ;; When selecting a directory with consult-dir, replace the
  ;; original directory in the minibuffer prompt rather than
  ;; shadowing it. This is cleaner but does prevent the user from
  ;; deleting the new dir to recover the original dir
  (setq consult-dir-shadow-filenames nil)

  ;; Use `+default/find-file-under-here' instead of `consult-find'
  ;; as the back-end for `consult-dir-jump-file'
  (setq consult-dir-jump-file-command (cmd! (call-interactively #'+default/find-file-under-here)))

  ;; DEPRECATED: Remove when projectile is replaced with project.el
  (setq consult-dir-project-list-function #'consult-dir-projectile-dirs)

  (when (modulep! :tools docker)
    ;; TODO: Replace with `tramp-container--completion-function' when we drop
    ;;   support for <29
    (defun +vertico--consult-dir-container-hosts (host)
      "Get a list of hosts from HOST."
      (cl-loop for line in (cdr
                            (ignore-errors
                              (apply #'process-lines +vertico-consult-dir-container-executable
                                     (append +vertico-consult-dir-container-args (list "ps")))))
               for cand = (split-string line "[[:space:]]+" t)
               collect (format "/%s:%s:/" host (car (last cand)))))

    (defun +vertico--consult-dir-podman-hosts ()
      (let ((+vertico-consult-dir-container-executable "podman"))
        (+vertico--consult-dir-container-hosts "podman")))

    (defun +vertico--consult-dir-docker-hosts ()
      (let ((+vertico-consult-dir-container-executable "docker"))
        (+vertico--consult-dir-container-hosts "docker")))

    (defvar +vertico--consult-dir-source-tramp-podman
      `(:name     "Podman"
        :narrow   ?p
        :category file
        :face     consult-file
        :history  file-name-history
        :items    ,#'+vertico--consult-dir-podman-hosts)
      "Podman candidate source for `consult-dir'.")

    (defvar +vertico--consult-dir-source-tramp-docker
      `(:name     "Docker"
        :narrow   ?d
        :category file
        :face     consult-file
        :history  file-name-history
        :items    ,#'+vertico--consult-dir-docker-hosts)
      "Docker candidate source for `consult-dir'.")

    (add-to-list 'consult-dir-sources '+vertico--consult-dir-source-tramp-podman t)
    (add-to-list 'consult-dir-sources '+vertico--consult-dir-source-tramp-docker t))

  (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-ssh t)
  (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-local t))

(use-package! consult-flycheck
  :when (and (modulep! :checkers syntax)
             (not (modulep! :checkers syntax +flymake)))
  :after (consult flycheck))

(use-package! consult-yasnippet
  :when (modulep! :editor snippets)
  :defer t
  :init (map! [remap yas-insert-snippet] #'consult-yasnippet))


(use-package! embark
  :defer t
  :init
  ;; NOTE These bindings are not active until a short time after
  ;; the first user input, because which-key is loaded on `doom-first-input-hook'.
  (after! which-key
    ;; When the which-key popup is visible after typing a key
    ;; prefix, type "C-h (C-)h" to use embark to display and select
    ;; bindings under the current key prefix.
    (map! :map which-key-C-h-map
          "h" #'jkroes/embark-prefix-help-command
          "C-h" #'jkroes/embark-prefix-help-command)

    ;; The pager text is rendered by replacing each command with its key in
    ;; `which-key-C-h-map'
    (setq which-key-C-h-map-prompt
          (string-replace "\\[which-key-show-standard-help]"
                          "\\[jkroes/embark-prefix-help-command]"
                          which-key-C-h-map-prompt))

    ;; When the which-key popup is not visible, type a key prefix
    ;; and "C-h" to use embark to display and select bindings under
    ;; the current key prefix. This assumes `which-key-idle-delay'
    ;; is sufficiently high to allow for two key presses before
    ;; the popup appears.
    (setq which-key--prefix-help-cmd-backup #'jkroes/embark-prefix-help-command))

  (map! [remap describe-bindings] #'embark-bindings
        "C-;" #'embark-act
        "C-:" #'embark-act-all
        (:leader
         :desc "Actions" "a" #'embark-act))
  :config
  ;; Don't prompt to confirm actions on multiple embark selections
  (setq embark-confirm-act-all nil)

  ;; Cycle current embark selection with the same key used to
  ;; launch embark-act
  (setq embark-cycle-key "C-;")

  ;; Same delay for the verbose indicator buffer to display as for which-key
  (setq embark-mixed-indicator-delay 0.1)

  ;; Use the same key to launch `embark-completing-read-prompter'
  ;; from embark-act as the one used to launch
  ;; jkroes/embark-prefix-bindings from any key prefix (see below)
  (setq embark-help-key "C-h")

  ;; Key to enable executing a command based on its associated binding displayed
  ;; during `embark-completing-read-prompter'. It should be a key that is not
  ;; normally part of a command-name and thus would not be used to
  ;; match an embark action.
  (setq embark-keymap-prompter-key ",")

  ;; Use completing-read to select an embark action without typing
  ;; `embark-help-key' after embark-act
  ;;(setq embark-prompter 'embark-completing-read-prompter

  ;; Disable extra popups showing available bindings when
  ;; `embark-completing-read-prompter' is the default. NOTE Embark uses
  ;; `with-eval-after-load' to modify `embark-indicators' when vertico is
  ;; present, so this must be configured after embark loads
  (when (eq embark-prompter 'embark-completing-read-prompter)
    (setq embark-indicators
          (cl-set-difference embark-indicators '(embark-mixed-indicator))))

  ;; Transform the `attach' completion category to `file', so that we can
  ;; execute actions from `embark-file-map' on attachments.
  (add-to-list 'embark-transformer-alist '(attach . embark--expand-attachment))

  (require 'consult)

  (set-popup-rule! "^\\*Embark Export:" :size 0.35 :ttl 0 :quit nil)

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
  (defvar-keymap +vertico/embark-doom-package-map
    :doc "Keymap for Embark package actions for packages installed by Doom."
    :parent embark-general-map
    "h" #'doom/help-packages
    "b" #'doom/bump-package
    "c" #'doom/help-package-config
    "u" #'doom/help-package-homepage)
  (setf (alist-get 'package embark-keymap-alist) #'+vertico/embark-doom-package-map)
  (map! (:map embark-file-map
         ;; When Emacs runs on WSL, open files externally in Windows
         (:when IS-WSL "x" #'open-in-windows)
         ;; Adds file to bookmarks
         "b" #'my/bookmark-set
         :desc "Open target with sudo"        "s"   #'doom/sudo-find-file
         (:when (modulep! :tools magit)
           :desc "Open magit-status of target" "g"   #'+vertico/embark-magit-status)
         (:when (modulep! :ui workspaces)
           :desc "Open in new workspace"       "TAB" #'+vertico/embark-open-in-new-workspace
           :desc "Open in new workspace"       "<tab>" #'+vertico/embark-open-in-new-workspace))))


(use-package! marginalia
  :hook (doom-first-input . marginalia-mode)
  :init
  (map! :map minibuffer-local-map
        :desc "Cycle marginalia views" "M-A" #'marginalia-cycle)
  :config
  ;; This is used by several annotation functions in marginalia, so it must be
  ;; overriden with advice. An annotator function can be called within the
  ;; definition of another annotation function, and multiple annotation
  ;; functions can be specified in marginalia-annotator-registry (see below).
  (advice-add #'marginalia-annotate-binding
              :override #'my/marginalia-annotate-binding)

  ;; Omit defining file from symbol annotations. This is an alternative to
  ;; overriding via advice for annotation functions that are not used within
  ;; other annotation functions
  (setf (car (alist-get 'symbol marginalia-annotator-registry))
        'my/marginalia-annotate-symbol)

  ;; Define the `attach' completion category for org-attach-open
  (add-to-list 'marginalia-command-categories 
               '(org-attach-open . attach))
  ;; Associate an annotation function with the `attach' completion category
  (add-to-list 'marginalia-annotator-registry
               '(attach marginalia-annotate-attachment builtin none))

  (when (modulep! +icons)
    (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))
  (advice-add #'marginalia--project-root :override #'doom-project-root)
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
            '(projectile-switch-project . project-file))
)


(use-package! wgrep
  :commands wgrep-change-to-wgrep-mode
  :config (setq wgrep-auto-save-buffer t))


(use-package! vertico-posframe
  :when (modulep! +childframe)
  :hook (vertico-mode . vertico-posframe-mode)
  :config
  (add-hook 'doom-after-reload-hook #'posframe-delete-all))

;; From https://github.com/minad/vertico/wiki#candidate-display-transformations-custom-candidate-highlighting
;;
;; Uses `add-face-text-property' instead of `propertize' unlike the above snippet
;; because `'append' is necessary to not override the match font lock
;; See: https://github.com/minad/vertico/issues/389
(use-package! vertico-multiform
  :hook (vertico-mode . vertico-multiform-mode)
  :config
  (defvar +vertico-transform-functions nil)

  (cl-defmethod vertico--format-candidate :around
    (cand prefix suffix index start &context ((not +vertico-transform-functions) null))
    (dolist (fun (ensure-list +vertico-transform-functions))
      (setq cand (funcall fun cand)))
    (cl-call-next-method cand prefix suffix index start))

  (defun +vertico-highlight-directory (file)
    "If FILE ends with a slash, highlight it as a directory."
    (when (string-suffix-p "/" file)
      (add-face-text-property 0 (length file) 'marginalia-file-priv-dir 'append file))
    file)

  (defun +vertico-highlight-enabled-mode (cmd)
    "If MODE is enabled, highlight it as font-lock-constant-face."
    (let ((sym (intern cmd)))
      (with-current-buffer (nth 1 (buffer-list))
      (if (or (eq sym major-mode)
              (and
               (memq sym minor-mode-list)
               (boundp sym)
               (symbol-value sym)))
          (add-face-text-property 0 (length cmd) 'font-lock-constant-face 'append cmd)))
        cmd))

  ;; Optional grid format for `embark-completing-read-prompter'
  (add-to-list 'vertico-multiform-categories '(embark-keybinding grid))

  (add-to-list 'vertico-multiform-categories
               '(file
                 (+vertico-transform-functions . +vertico-highlight-directory)))
  (add-to-list 'vertico-multiform-commands
               '(execute-extended-command
                 (+vertico-transform-functions . +vertico-highlight-enabled-mode))))
