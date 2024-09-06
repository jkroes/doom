;;; completion/corfu/config.el -*- lexical-binding: t; -*-

(defvar +corfu-want-ret-to-confirm t
  "Configure how the user expects RET to behave.
Possible values are:
- t (default): Insert candidate if one is selected, pass-through otherwise;
- `minibuffer': Insert candidate if one is selected, pass-through otherwise,
              and immediatelly exit if in the minibuffer;
- nil: Pass-through without inserting.")

(defvar +corfu-buffer-scanning-size-limit (* 1 1024 1024) ; 1 MB
  "Size limit for a buffer to be scanned by `cape-dabbrev'.")

(defvar +corfu-want-minibuffer-completion nil
  "Whether to enable Corfu in the minibuffer.
Setting this to `aggressive' will enable Corfu in more commands which
use the minibuffer such as `query-replace'.")

(defvar +corfu-want-tab-prefer-expand-snippets nil
  "If non-nil, prefer expanding snippets over cycling candidates with
TAB.")

(defvar +corfu-want-tab-prefer-navigating-snippets nil
  "If non-nil, prefer navigating snippets over cycling candidates with
TAB/S-TAB.")

(defvar +corfu-want-tab-prefer-navigating-org-tables nil
  "If non-nil, prefer navigating org tables over cycling candidates with
TAB/S-TAB.")


;;
;;; Packages

(use-package! corfu
  :hook (doom-first-input . global-corfu-mode)
  :init
  (add-hook! 'minibuffer-setup-hook
    (defun +corfu-enable-in-minibuffer ()
      "Enable Corfu in the minibuffer."
      (when (pcase +corfu-want-minibuffer-completion
              ('aggressive
               (not (or (bound-and-true-p mct--active)
                        (bound-and-true-p vertico--input)
                        (and (featurep 'auth-source)
                             (eq (current-local-map) read-passwd-map))
                        (and (featurep 'helm-core) (helm--alive-p))
                        (and (featurep 'ido) (ido-active))
                        (where-is-internal 'minibuffer-complete
                                           (list (current-local-map)))
                        (memq #'ivy--queue-exhibit post-command-hook))))
              ('nil nil)
              (_ (where-is-internal #'completion-at-point
                                    (list (current-local-map)))))
        (setq-local corfu-echo-delay nil)
        (corfu-mode +1))))
  :config
  (setq corfu-auto t
        corfu-auto-delay 0.18
        corfu-auto-prefix 2
        global-corfu-modes '((not erc-mode
                                  circe-mode
                                  help-mode
                                  gud-mode
                                  vterm-mode)
                             t)
        corfu-cycle t
        corfu-preselect 'prompt
        corfu-count 16
        corfu-max-width 120
        corfu-on-exact-match nil
        corfu-quit-at-boundary (if (or (modulep! :completion vertico)
                                       (modulep! +orderless))
                                   'separator t)
        corfu-quit-no-match corfu-quit-at-boundary
        tab-always-indent 'complete)

  ;; HACK When `corfu-popupinfo-mode' is disabled `corfu-info-documentation'
  ;; spawns a help buffer, which the popup module catches, but the two are not
  ;; yet compatible. `scroll-other-window' will not scroll the help buffer, and
  ;; "q" will quit the original buffer. (The latter behavior is likely tied to
  ;; corfu restoring the window configuration via pre-command-hook.)
  ;; (remove-hook 'corfu-mode-hook #'corfu-popupinfo-mode)
  ;; (defadvice! +popup--ignore-corfu-info-documentation (fn &rest args)
  ;;   :around #'corfu-info-documentation
  ;;   (push '("^\\*\\([Hh]elp\\|Apropos\\)" nil) display-buffer-alist)
  ;;   (apply fn args)
  ;;   (pop display-buffer-alist))

  ;; Frees up RET for other uses, assuming it isn't bound elsewhere to
  ;; corfu-map
  (unbind-command #'corfu-insert corfu-map)

  ;; BUG Only some of Doom's bindings to corfu-map work out of the box. Those
  ;; such as C-j, C-k, C-u, and C-d that are bound in evil global maps do not
  ;; without this. See https://github.com/minad/corfu/issues/12. The
  ;; out-of-the-box bindings are also inadequate because they omit many commands
  ;; in corfu-map and fail to bind the ones that are present to insert state and
  ;; are thus shadowed. See
  ;; ~/.config/doom/modules/config/default/+evil-bindings.el for revised bindings.
  (advice-add 'corfu--setup :after (cmd! (evil-normalize-keymaps)))
  (advice-add 'corfu--teardown :after (cmd! (evil-normalize-keymaps)))

  (add-to-list 'completion-category-overrides `(lsp-capf (styles ,@completion-styles)))
  (add-to-list 'corfu-auto-commands #'lispy-colon)
  (add-to-list 'corfu-continue-commands #'+corfu/move-to-minibuffer)
  (add-to-list 'corfu-continue-commands #'+corfu/smart-sep-toggle-escape)
  (add-hook 'evil-insert-state-exit-hook
            (cmd! (when (and (frame-live-p corfu--frame)
                             (frame-visible-p corfu--frame))
                    (corfu-quit))))

  ;; HACK: If you want to update the visual hints after completing minibuffer
  ;;   commands with Corfu and exiting, you have to do it manually.
  (defadvice! +corfu--insert-before-exit-minibuffer-a ()
    :before #'exit-minibuffer
    (when (or (and (frame-live-p corfu--frame)
                   (frame-visible-p corfu--frame))
              (and (featurep 'corfu-terminal)
                   (popon-live-p corfu-terminal--popon)))
      (when (member isearch-lazy-highlight-timer timer-idle-list)
        (apply (timer--function isearch-lazy-highlight-timer)
               (timer--args isearch-lazy-highlight-timer)))
      (when (member (bound-and-true-p anzu--update-timer) timer-idle-list)
        (apply (timer--function anzu--update-timer)
               (timer--args anzu--update-timer)))
      (when (member (bound-and-true-p evil--ex-search-update-timer)
                    timer-idle-list)
        (apply (timer--function evil--ex-search-update-timer)
               (timer--args evil--ex-search-update-timer)))))

  ;; HACK: If your dictionaries aren't set up in text-mode buffers, ispell will
  ;;   continuously pester you about errors. This ensures it only happens once
  ;;   per session.
  (defadvice! +corfu--auto-disable-ispell-capf-a (fn &rest args)
    "If ispell isn't properly set up, only complain once per session."
    :around #'ispell-completion-at-point
    (condition-case-unless-debug e
        (apply fn args)
     ('error
      (message "Error: %s" (error-message-string e))
      (message "Auto-disabling `text-mode-ispell-word-completion'")
      (setq text-mode-ispell-word-completion nil)
      (remove-hook 'completion-at-point-functions #'ispell-completion-at-point t)))))

(use-package! cape
  :defer t
  :init
  (add-hook! 'prog-mode-hook
    (defun +corfu-add-cape-file-h ()
      (add-hook 'completion-at-point-functions #'cape-file -10 t)))
  (add-hook! '(org-mode-hook markdown-mode-hook)
    (defun +corfu-add-cape-elisp-block-h ()
      (add-hook 'completion-at-point-functions #'cape-elisp-block 0 t)))
  ;; Enable Dabbrev completion basically everywhere as a fallback.
  (when (modulep! +dabbrev)
    (setq cape-dabbrev-check-other-buffers t)
    ;; Set up `cape-dabbrev' options.
    (defun +dabbrev-friend-buffer-p (other-buffer)
      (< (buffer-size other-buffer) +corfu-buffer-scanning-size-limit))
    (add-hook! '(prog-mode-hook
                 text-mode-hook
                 conf-mode-hook
                 comint-mode-hook
                 minibuffer-setup-hook
                 eshell-mode-hook)
      (defun +corfu-add-cape-dabbrev-h ()
        (add-hook 'completion-at-point-functions #'cape-dabbrev 20 t)))
    (after! dabbrev
      (setq dabbrev-friend-buffer-function #'+dabbrev-friend-buffer-p
            dabbrev-ignored-buffer-regexps
            '("\\` "
              "\\(TAGS\\|tags\\|ETAGS\\|etags\\|GTAGS\\|GRTAGS\\|GPATH\\)\\(<[0-9]+>\\)?")
            dabbrev-upcase-means-case-search t)
      (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
      (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
      (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode)))

  ;; Make these capfs composable.
  (advice-add #'lsp-completion-at-point :around #'cape-wrap-noninterruptible)
  (advice-add #'lsp-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'comint-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'eglot-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'pcomplete-completions-at-point :around #'cape-wrap-nonexclusive)
  ;; From the `cape' readme. Without this, Eshell autocompletion is broken on
  ;; Emacs28.
  (when (< emacs-major-version 29)
    (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
    (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)))

(use-package! yasnippet-capf
  :when (modulep! :editor snippets)
  :defer t
  :init
  (add-hook! 'yas-minor-mode-hook
    (defun +corfu-add-yasnippet-capf-h ()
      (add-hook 'completion-at-point-functions #'yasnippet-capf 30 t))))

(use-package! corfu-terminal
  :when (modulep! :os tty)
  :when (not (display-graphic-p))
  :hook ((corfu-mode . corfu-terminal-mode)))


;;
;;; Extensions

(use-package! corfu-history
  :hook ((corfu-mode . corfu-history-mode))
  :config
  (after! savehist (add-to-list 'savehist-additional-variables 'corfu-history)))

(use-package! corfu-popupinfo
  :hook ((corfu-mode . corfu-popupinfo-mode))
  :config
  (setq corfu-popupinfo-delay '(nil . 0.3)
        ;; Less jarring to avoid hiding when `corfu-popupinfo-delay' is short
        corfu-popupinfo-hide nil
        corfu-popupinfo-max-height 40))

(use-package! nerd-icons-corfu
  :when (modulep! +icons)
  :defer t
  :init
  (after! corfu
    (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)))

;; If vertico is not enabled, orderless will be installed but not configured.
;; That may break smart separator behavior, so we conditionally configure it.
(use-package! orderless
  :when (and (not (modulep! :completion vertico))
             (modulep! +orderless))
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles orderless partial-completion)))
        orderless-component-separator #'orderless-escapable-split-on-space)
  (set-face-attribute 'completions-first-difference nil :inherit nil))
