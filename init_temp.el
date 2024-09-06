;;; tree-sitter

;; https://www.masteringemacs.org/article/how-to-get-started-tree-sitter

;; C-h a -ts-mode$ shows available treesitter modes in Emacs
;; TODO Change mode hooks from *-mode-hook to *-ts-mode-hook
;; TODO Old indentation customizations may not work
;; TODO Font lock faces have changed in the new ts modes
;; TODO Customization options are different for the new modes
;; TODO Use ts modes with eglot and other packages. See
;; https://www.adventuresinwhy.com/post/eglot/
;; TODO C-M-f, C-M-SPC, and C-M-k will no longer work well in ts modes.
;; There is a way to remove the ts-based behavior from these commands,
;; but consider using the Combobulate package instead.

;; Language grammars
(setq treesit-language-source-alist
      '((lua "https://github.com/tree-sitter-grammars/tree-sitter-lua")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (r "https://github.com/r-lib/tree-sitter-r")
        (ruby "https://github.com/tree-sitter/tree-sitter-ruby")))

(with-eval-after-load 'treesit
  ;; Install missing language grammars
  (dolist (source treesit-language-source-alist)
    (unless (treesit-ready-p (car source) t)
      (treesit-install-language-grammar (car source))))
  ;; Use treesitter mode instead of base mode
  (add-to-list 'major-mode-remap-alist '(lua-mode . lua-ts-mode))
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
  ;; TODO If you install emacs-ess, replace this with a line that modifies
  ;; major-mode-remap-alist
  (add-to-list 'auto-mode-alist '("\\.[rR]\\'" . r-ts-mode))
  (add-to-list 'major-mode-remap-alist '(ruby-mode . ruby-ts-mode)))

;;; lisp/lib/sessions.el ------------------------------------------------------

;; NOTE I use sessions.el to quickly save and load frame layouts (windows and
;; buffers) so that I can quickly restart Emacs while developing its config. It
;; may have effects beyond that, but I haven't seen any major bugs yet. I prefer
;; doom/quickload-session for one-time saves, and my/load-session for
;; sessions I plan to reopen multiple times.

;; HACK Fix for https://github.com/doomemacs/doomemacs/issues/7580
(setq desktop-base-file-name ".emacs_desktop")

(map! :map doom-leader-quit/session-map
      :desc "Restore last session"         "l" #'my/quickload-session
      :desc "Save session to file"         "L" #'my/load-session)

(defun my/quickload-session ()
  (interactive)
  "Quickload without a confirmation prompt."
  (doom/quickload-session t))

(defun my/load-session ()
  "Load session from file. Do not delete the session file after loading
(use `doom/quickload-session' if you want auto-deletion). Do not prompt for
confirmation."
  (interactive)
  (let* ((desktops (cl-reduce #'remove
                              ;; Use separate session files from quickload
                              (list ".." "." desktop-base-file-name)
                              :initial-value (directory-files desktop-dirname)
                              :from-end t))
         (original (if (length= desktops 1)
                       (expand-file-name (car desktops) desktop-dirname)
                     (read-file-name "Session to restore:"
                                     (file-name-as-directory desktop-dirname)
                                     nil
                                     t)))
         (backup (concat original "_bck")))
    (copy-file original backup)
    (doom/load-session original)
    (copy-file backup original)
    (delete-file backup)))

;;; completion ----------------------------------------------------------------

;; Based on https://github.com/minad/corfu/wiki, but it actually works...
;; NOTE This setup will not be torn down properly if lsp-mode is disabled.
;; See the definition of lsp-completion-mode if you want to improve the
;; code
;; (unless (featurep! :completion company)
;;   (setq lsp-completion-provider :none)
;;   ;; OPTIONAL: The first word uses orderless-flex for filtering. This means the
;;   ;; characters are matched in order but do not have to be consecutive. It
;;   ;; returns more results than a pure orderless style would, at least initially
;;   ;; (add-hook 'lsp-completion-mode-hook
;;   ;; NOTE This also affects vertico; e.g., you can't use regex like "^" on the first word
;;   ;;           (lambda ()
;;   ;;             (add-to-list 'orderless-style-dispatchers
;;   ;;                          (lambda (_pattern index _total)
;;   ;;                            (and (eq index 0) 'orderless-flex)))))
;;   ;; Make lsp completion use orderless
;;   (add-hook 'lsp-completion-mode-hook
;;             (lambda ()
;;               (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
;;                     '(orderless))))
;;   ;; Bust capf caches, refreshing candidates more often (e.g., on backspace)
;;   (add-hook 'lsp-completion-mode-hook
;;             (lambda ()
;;               (setq-local completion-at-point-functions
;;                           (list (cape-capf-buster #'lsp-completion-at-point))))))

;; TODO Marginlia annotations don't seem to shift left to account for
;; left-truncated candidates.
;; Left-truncate matches/candidates in e.g. consult-line (spc s b) and
;; consult-recent-file (spc f r) and the recent file source for consult-buffer
;; (spc b b)
;; (use-package! vertico-truncate
;;   :config
;;   (vertico-truncate-mode))


;; Use which-key to discover evil motions (e.g., type "va")
;; outline-minor-mode:
;; - [ ] custom faces for sections
;; - [ ] folding that optionally hides forms on toggle
;; - [ ] consult-org-heading binding
;; [ ] Read the wiki for vertico
;; [ ] Read the README and wiki for consult
;; [ ] Finish reading the wiki and README for embark starting at
;;     https://github.com/oantolin/embark?tab=readme-ov-file#quitting-the-minibuffer-after-an-action,
;;     as well as https://karthinks.com/software/fifteen-ways-to-use-embark/
;; - [ ] Customize the keymaps within embark-keymap-alist. See
;;       https://github.com/oantolin/embark/wiki/Default-Actions.
;; - [ ] Customize embark-become-keymaps
;; - [ ] Replace keybindings for things at point (e.g., browse-url) with
;;       embark-act and embark-dwim
;; [ ] Configure buffers that run in emacs state (e.g., embark-collect) or use
;; evil-collection bindings
;; TODO Enable line numbers for *Messages*. Setting the hook won't work.
;; Enable line numbers for particular modes. See display-line-numbers-mode
;; TODO I renamed ~/.config/doom/modules/lang/org to org_bck
;; to allow Doom to launch. Figure out where the issue is so that
;; you can begin using org again.
;; TODO Backup files from previous doom installation are in
;; the backup subfolder
;; TODO Pressing M-TAB in a new heading completes todo keywords.
;; See https://orgmode.org/manual/Completion.html
;; TODO Every time you update Doom or its packages, you need to
;; go to
;; ~/.config/emacs/.local/straight/repos/smartparens/smartparens-ruby.el
;; and change ruby-base-mode to ruby-mode. Do this for Emacs 28
;; until you upgrade to Emacs 29. Then delete the .elc version of
;; the file, then run doom sync.
;; TODO Get lsp-mode working within org-mode buffers
;; TODO Compare bpython and ptpython to ipython
;; TODO Look into xonsh
;; TODO Learn how to align things with evil-lion.
;; TODO Look into using https://github.com/cyrus-and/zoom
;; TODO Replace dash-docs with devdocs for the +docsets flag. This will be a
;; big project, so in the meantime I am simply rebinding spc-s-k to
;; devdocs-peruse.
;; TODO Debug the font-lock-related issues that are causing emacs to hang on
;; both macos and linux. Someone on
;; https://www.reddit.com/r/emacs/comments/k7cku8/when_emacs_hangs_what_do_you_do/
;; recommended
;; https://github.com/alphapapa/emacs-package-dev-handbook?tab=readme-ov-file#tools-8
;; TODO Also use open-dribble-file to debug Emacs. Kill Emacs as soon as it
;; hangs, then examine the file for whatever keystrokes triggered the hang.
;; TODO Read the zhsell manual (not just the man pages)
;; TODO Look into https://github.com/tuh8888/chezmoi.el
;; TODO Setup https://docs.atuin.sh/ (recommended by chezmoi for
;; tracking shell history)
;; TODO Purchase https://www.monolisa.dev/buy
;; TODO When you edit the contents
;; of a collapsed org-src block from within an org src buffer, it expands the
;; block once you exit. It would be nice to close it again upon exiting back to
;; the org buffer.
;; TODO Look into using the following packages plus (custom?)
;; functions to send text to the buffer for execution in a shell, as an
;; alternative to comint: https://codeberg.org/akib/emacs-eat
;; https://github.com/szermatt/mistty (misty-send-string)
;; https://www.masteringemacs.org/article/running-shells-in-emacs-overview
;; TODO C-h v over a long function or variable n

;;; edebug / messages -------------------------------------------------

(autoload #'edebug-instrument-function "edebug")

;; NOTE Uncomment this if not using evil-collection-edebug
;; As noted in
;; https://github.com/noctuid/evil-guide#why-dont-keys-defined-with-evil-define-key-work-immediately,
;; keymap normalization may be required in some cases. One seems to be use of
;; edebug-mode-map as an evil-intercept map. Without normalization, if in normal
;; mode SPC will trigger leader until you first switch to another evil state.
;; (add-hook 'edebug-mode-hook #'evil-normalize-keymaps)
;; (add-hook 'edebug-mode-hook
;;           (defun edebug-enter-normal-state ()
;;             (when edebug-mode (evil-normal-state))))

;; BUG When switching to buffer *Messages*, Emacs seems to go into an infinite
;; loop. It is caused by the following. Hang can even be triggered by consult
;; preview of *Messages* when within consult-buffer
;; Because this buffer is launched early, I have to use this instead of
;; `messages-buffer-mode-hook'
;; (with-current-buffer "*Messages*"
;;   (+word-wrap-mode)
;;   (display-line-numbers-mode))

;;; default -------------------------------------------------------------------

(map! "M-RET" comment-line-break-function)

;; TODO This inserts a space even if one should not be inserted.
;; e.g. during ruby multiline comments.
;; https://emacs.stackexchange.com/questions/22746/add-a-space-after-the-comment-delimiter
;; (defun comment-indent-new-line--insert-a-space-after (&rest _)
;;   "Ensure there is exactly one space after `comment-start'"
;;     (just-one-space))
;; (advice-add 'comment-indent-new-line :after #'comment-indent-new-line--insert-a-space-after)


;;; vertico -------------------------------------------------------------------

;; TODO The +icons flag to the vertico module loads package
;; all-the-icons-completion, which is used by adding
;; all-the-icons-completion-marginalia-setup to marginalia-mode-hook. It
;; creates a pseudo-issue:
;; Run spc-f-f within ~/org, then embark-act. Notice the extra space between
;; the last candidate and the bottom of the frame. This is because vertico and
;; embark use the same number of candidates but different line sizes due to
;; icons. Furthermore, if you decrease all-the-icons-scale-factor and/or
;; all-the-icons-default-adjust, then scroll down the list of org notes, you'll
;; notice that the icons for longer filenames are smaller.
;;
;; Once you have fixed this issue, re-enable the +icons flag.
;;
;; Alternatively, enable vertico-resize:
;; (after! vertico (setq vertico-resize t))

;;; emacs lisp ----------------------------------------------------------------


(defun unbind-commands (keymap &rest cmds)
  (let ((cmds (ensure-list cmds)))
    (dolist (command cmds)
      (unbind-command keymap command))))


;; modules/config/default/config.el binds this command to [C-return], which is
;; equivalent to "C-<return>" and hides any bindings to "C-RET"
(unbind-command global-map #'+default/newline-below)
(unbind-command evil-insert-state-map #'+default/newline-below)
(unbind-command evil-normal-state-map #'+default/newline-below)
(map! :map emacs-lisp-mode-map :gin "C-RET" #'eval-defun)
(map! :map emacs-lisp-mode-map :gin "C-<return>" #'eval-defun)

;;; Keybindings (legacy) ---------------------------------------------------------------

;; TODO Replace these with the sp-* commands? Investigate the differences
;; between the built-in and smartparens commands
;; NOTE These commands require evil-move-beyond-eol to work properly across
;; multiple lines. What are the cons of enabling this setting? Also see
;; recommendations for evil and lisp editing in
;; https://www.reddit.com/r/emacs/comments/rbf31m/does_anybody_else_find_evil_very_painful_for/
;; and discussion of cursor movement in
;; https://www.dr-qubit.org/Evil_cursor_model.html and additional commands in
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/List-Motion.html
;; (map! :map emacs-lisp-mode-map
;;       :when evil-move-beyond-eol
;;       :m "<up>" #'up-list
;;       :m "<down>" #'down-list
;;       :m "<left>" #'backward-list
;;       :m "<right>" #'forward-list)

;; https://www.masteringemacs.org/article/emacs-builtin-elisp-cheat-sheet
;; TODO Replace the bindings below with this. I dono't think evil-collection is
;;loaded without the +everywhere flag for evil, so do this yourself. Or
;;re-enable +everywhere and disable any tweaks/keybindings you dislike. Also
;; modify the white/blacklisted evil-collection modes. See the evil module.
;;(push 'shortdoc evil-collection-mode-list)
;;
;; (map! :map shortdoc-mode-map
;;       :m "j" #'forward-button
;;       :m "<tab>" #'forward-button
;;       :m "k" #'backward-button
;;       :m "<backtab>" #'backward-button)

;;; comint --------------------------------------------------------------------
(setq comint-prompt-read-only nil ; Read-only prompt in ess-R
      comint-scroll-to-bottom-on-input t
      ;; Prefer this to `comint-scroll-to-bottom-on-output'
      comint-scroll-show-maximum-output t
      comint-use-prompt-regexp nil) ; nil enables evil motions

;;; misc --------------------------------------------------------------------

;; (defun evil-insert-state-on-window-selection ()
;;   "When switching to modes such as inf-ruby-mode, enter insert mode."
;;   (when (memq major-mode '(inf-ruby-mode))
;;     (evil-insert-state)))

;; (add-hook 'doom-switch-window-hook
;;           #'evil-insert-state-on-window-selection)

;;; launchschool --------------------------------------------------------------

(setq-hook! 'js-mode-hook tab-width 2)

;;; toml ----------------------------------------------------------------------

(setq-hook! 'conf-toml-mode-hook tab-width 2)
(setq-hook! 'conf-unix-mode-hook tab-width 2)

;;; chezmoi (go) templates ----------------------------------------------------

(push '("\\.tmpl" . go-template-mode) auto-mode-alist)
(require 'go-template-mode)

;;; popup (experimental) ------------------------------------------------------

(with-eval-after-load 'evil-collection-devdocs
  (evil-collection-define-key 'normal 'devdocs-mode-map
    ;; Same binding as Info-top-node
    "gt" 'devdocs-first-page))

(set-popup-rule! "^\\*devdocs\\*$" :size 0.3 :quit 'current)

;;; graveyard

;; NOTE If you want normal org cycle behavior when using evil-org (subtree ->
;; contents -> collapse), uncomment this. Note that this only applies when the
;; evil module's +everywhere flag is enabled. Since I've disabled the flag and
;; ported evil-org's use-package and disabled this anyway, I'm archiving this
;; (remove-hook 'org-tab-first-hook '+org-cycle-only-current-subtree-h)

;;;; emacs lisp -------------------------------------------------

;; TODO Remove this if no longer needed. From the backup config.
;; (defvar +emacs-lisp-outline-regexp "[ \t]*;;;;* [^ \t\n]")

;; TODO Re-enable this if you switch from corfu to company
;; Due to alterations to private company module, emacs-lisp otherwise has
;; empty `company-backends'
;;(set-company-backend! 'emacs-lisp-mode '(company-capf))

;;; org ----------------------------------------------------------------------

;; NOTE Line spacing pads the bottom of lines, while line height pads the top.
;; Both are meant to work with literal lines, not visual ones. Line height adds
;; padding only to the final visual line, which is definitely not desirable.
;; Line spacing is better but won't pad between visual lines at all. It also
;; seems like the effect is quite different for the same (floating point)
;; values for the two text properties.
;; (defun test ()
;;   (interactive)
;;   (add-text-properties (point-min) (point-max) '(line-spacing 0.5 line-height 0)))
