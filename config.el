;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;;; Notes on working with Doom: ------------------------------------------------
;;
;; As with many software projects, the documentation is a mix of outdated and
;; never accurate to begin with. The following is a best guess, given this:
;;
;; doom doctor: Run for general diagnostics. E.g., do you use a version of Emacs
;; that has enabled native compilation? If you're having trouble installing it
;; see https://github.com/d12frosted/homebrew-emacs-plus/issues/536.
;;
;; chemacs has been replaced with a profile system:
;; https://github.com/doomemacs/doomemacs/tree/master/profiles
;;
;; doom h[elp]: Show help for doom cli.
;;
;; doom help sync: Show help for doom sync.
;;
;; doom sync: Run after modifying init.el (your `doom!' block); or autoload.el,
;; or any file within an autoload/ folder, within a private module. I suspect
;; this is related to `doom compile', which compiles part of your config
;; (init.el and packages.el) and modules. Changes to all but config.el are not
;; recognized automatically because those changes need to be re-compiled.
;;
;; doom sync -u: Also update emacs packages. This needs to run the first time
;; Doom is installed, and anytime packages.el is changed.
;;
;; doom upgrade: Upgrade doom emacs and, like `doom sync -u', its packages.
;; After upgrading, you might also have to run `pdf-tools-install'.
;;
;; Editing modules may break lazy loading and lead to unexpected results when
;; trying to defer a package. To see what loads and when, enable
;; `force-load-messages' in early-init.el
;;
;; `doom install|sync|upgrade' might fail on work computers, possibly because
;; recipes clone from gitlab, and gitlab is blocked. Clone the package manually
;; (from github potentially) while inside ~/.emacs.d/.local/straight/repos, then
;; try again. Try also disconnecting from the VPN. Alternatively, a package may
;; have renamed its branch from master to main. Delete the repo at issue and try
;; again.

(setq confirm-kill-emacs nil
      ;; It's really annoying when auto-wrap auto-scrolls the window to the
      ;; left because the cursor is off the RHS until you type a space.
      ;;auto-hscroll-mode nil ; 'current-line
      ;; Evil: Easier "j" and "k", harder "gg" navigation
      display-line-numbers-type 'relative
      ;; See core-keybinds.el for additional wk settings
      which-key-idle-delay 0.3
      ;; NOTE This is less useful when Doom supplies its own descriptions
      which-key-compute-remaps t
      ;; Blacklist all modes by default
      evil-collection-mode-list nil
      doom-theme 'doom-one
      doom-font (font-spec :family "Hack" :size 14)
      ;; `doom-init-leader-keys-h' binds to general-override-mode-map and calls
      ;; `'general-override-mode' (see also `general-override-auto-enable'). This
      ;; binds `doom-leader-key' and `doom-leader-alt-key', overriding any other
      ;; evil binding. See also:
      ;; https://discourse.doomemacs.org/t/how-to-re-bind-keys/56
      ;; https://github.com/noctuid/general.el#override-keymaps-and-buffer-local-keybindings
      ;; https://github.com/noctuid/evil-guide#keymap-precedence
      ;; ~/doom-emacs/.local/straight/repos/evil/evil-core.el
      ;;
      ;;If using M-SPC instead, in Windows Terminal actions settings unbind
      ;;alt+space and save settings.
      ;;doom-leader-alt-key "C-SPC"
      ;;doom-localleader-alt-key "C-SPC m"
      )

;;; macos ------------------------------------------------------------

;; For the Kinesis Advantage 2 keyboard
(setq ns-command-modifier 'control
      mac-command-modifier 'control)

;; NOTE Whatever MacOS binding is used to switch windows needs to be implemented
;; in Emacs, which might override MacOS bindings for system shortcuts. See
;; System Settings>Keyboard>Keyboard Shortcuts>Keyboard>Move focus to next
;; window
(map! "C-SPC" #'other-frame) ; NOTE This unbinds +popup/toggle

;;; comments / auto-fill

;; Only auto-fill comments
(setq comment-auto-fill-only-comments t)
(setq-default fill-column 79)
;; https://www.gnu.org/software/emacs/manual/html_node/efaq/Turning-on-auto_002dfill-by-default.html
(add-hook 'prog-mode-hook 'turn-on-auto-fill)

;;; helper functions -------------------------------------------------

(defun concat-path (&rest parts)
  "Concatenate unlimited path components"
  (cl-reduce (lambda (a b) (expand-file-name b a)) parts))

;;; doom modules -----------------------------------------------------

(autoload 'doom--help-modules-list (concat-path doom-emacs-dir "core/autoload/help.el"))
(defun doom/copy-module-to-private (category module)
  "Copy the module corresponding to the strings category and module as a private
module."
  (interactive
   (nconc
    (mapcar #'intern
            (split-string
             (completing-read "Copy module: "
                              (doom--help-modules-list)
                              nil t nil nil
                              (doom--help-current-module-str))
             " " t))))
  (let* ((path (doom-module-locate-path category module))
         (newpath (replace-regexp-in-string doom-modules-dir (car doom-modules-dirs) path)))
    (copy-directory path newpath nil t t)))

;; Compare private and non-private module file to see your modifications and
;; any changes Doom has made if you've upgraded it recently. If current buffer
;; is a private module file, compare this file; otherwise, read a filename with
;; the private module root dir as the initial input.
;;
;; TODO remove the let binding for doom-modules-dirs. It is temporary to allow
;; comparison of the outdated private modules to the new non-private modules
(autoload #'ediff-read-file-name "ediff")
(autoload #'ediff-files-internal "ediff")
(defun doom/ediff ()
  "Run Ediff on a private Doom module file and its non-private counterpart. If
the current buffer is a private module file, diff this file; otherwise, read a
filename with the root private module dir as initial input"
  (interactive)
  (let* ((doom-modules-dirs '("/Users/jkroes/.doom.d.bck/modules/"))
         (file-A
          (if (string-prefix-p (car doom-modules-dirs) default-directory)
              (buffer-file-name)
            (ediff-read-file-name
             "Private module file"
             (car doom-modules-dirs)
             (ediff-get-default-file-name)
             'no-dirs)))
         (file-B (replace-regexp-in-string
                  (car doom-modules-dirs)
                  doom-modules-dir
                  file-A)))
    (ediff-files-internal file-A file-B nil nil 'ediff-files)))

;; Run ediff in another window. See doom-ui.el.
(after! ediff
  (remove-hook 'ediff-before-setup-hook #'doom-ediff-save-wconf-h)
  (add-hook 'ediff-before-setup-hook
            (defun ediff-in-new-frame () (select-frame (make-frame))))
  (remove-hook 'ediff-quit-hook #'doom-ediff-restore-wconf-h)
  (remove-hook 'ediff-suspend-hook #'doom-ediff-restore-wconf-h)
  (add-hook! 'ediff-quit-hook :append #'delete-frame))

;; When using doom/ediff, it can be useful to copy the contents of one or both
;; diff buffers for pasting into a third buffer.
(add-hook 'ediff-startup-hook #'my/ediff-mode-bindings)
(defun my/ediff-mode-bindings ()
  (map! :map ediff-mode-map
        "ca" #'ediff-copy-A
        "cb" #'ediff-copy-B))
(defun ediff-copy-A (arg)
  (interactive "P")
  (ediff-diff-to-diff2 arg "a"))
(defun ediff-copy-B (arg)
  (interactive "P")
  (ediff-diff-to-diff2 arg "b"))
(defun ediff-diff-to-diff2 (arg &optional keys)
  (interactive "P")
  (ediff-barf-if-not-control-buffer)
  (or keys (setq keys (this-command-keys)))
  (if (eq arg '-) (setq arg -1)) ; translate neg arg to -1
  (if (numberp arg) (ediff-jump-to-difference arg))

  (let* ((char1 (aref keys 0)))
    (ediff-copy-diff2 ediff-current-difference
                     (ediff-char-to-buftype char1))))
(defun ediff-copy-diff2 (n from-buf-type)
  (let* ((reg-to-copy (ediff-get-region-contents n from-buf-type ediff-control-buffer)))
    (kill-new reg-to-copy)))

;;; edebug / messages -------------------------------------------------

;; As noted in
;; https://github.com/noctuid/evil-guide#why-dont-keys-defined-with-evil-define-key-work-immediately,
;; keymap normalization may be required in some cases. One seems to be use of
;; edebug-mode-map as an evil-intercept map. Without normalization, if in normal
;; mode SPC will trigger leader until you first switch to another evil state.
(add-hook 'edebug-mode-hook #'evil-normalize-keymaps)
(add-hook 'edebug-mode-hook (defun emacs-state-for-edebug ()
                              (if edebug-mode (evil-emacs-state)
                                (evil-exit-emacs-state))))


;; Because this buffer is launched early, I have to use this instead of
;; `messages-buffer-mode-hook'
(with-current-buffer "*Messages*" (+word-wrap-mode))

;; Print full results to the messages buffer when evaluating expressions
(setq eval-expression-print-length nil
      eval-expression-print-level  nil
      edebug-print-length 1000)

;;; vundo -------------------------------------------------------------

(use-package! vundo
  :init
  ;; Run evil-collection/modes/vundo/evil-collection-vundo.el
  (push 'vundo evil-collection-mode-list))

;;; corfu -------------------------------------------------------------

(use-package! corfu
  :hook (doom-first-input . global-corfu-mode)
  :init
  (setq corfu-cycle nil
        corfu-auto t
        corfu-auto-prefix 2
        ;; What `corfu-insert-separator' (bound to M-SPC during completion)
        ;; inserts. (Note that Doom's default binding for doom-leader-alt-key
        ;; shadows the default binding for `corfu-insert-separator'.)
        ;; NOTE Does this need to be the same as `orderless-component-separator'?
        corfu-separator ?\s
        ;; corfu normally quits if a word boundary (e.g., space) is inserted, but
        ;; a space is permitted if it was inserted by `corfu-insert-separator'.
        corfu-quit-at-boundary 'separator
        ;; Keep corfu alive without a match, following `corfu-insert-separator'
        corfu-quit-no-match 'separator
        corfu-on-exact-match 'quit
        ;; Indent and complete on tab
        tab-always-indent 'complete
        corfu-popupinfo-delay 0.5
        ;; Less jarring to avoid hiding when `corfu-popupinfo-delay' is short
        corfu-popupinfo-hide nil)
  (evil-define-key 'insert corfu-map
    [tab]         #'corfu-insert
    (kbd "TAB")   #'corfu-insert
    (kbd "C-b")   #'corfu-scroll-down
    (kbd "C-f")   #'corfu-scroll-up
    (kbd "C-,")   #'corfu-first
    (kbd "C-.")   #'corfu-last
    ;; This will shadow the binding to evil-escape in
    ;; ~/doom-emacs/modules/editor/evil/config.el
    (kbd "C-g")   #'corfu-abort
    (kbd "C-SPC") #'corfu-insert-separator
    (kbd "C-@") #'corfu-insert-separator ; For the terminal
    ;; You can also press existing bindings for scroll-other-window(-down)
    (kbd "M-p")   #'corfu-popupinfo-scroll-down
    (kbd "M-n")   #'corfu-popupinfo-scroll-up
    (kbd "C-h")   #'corfu-info-documentation) ; Works with scroll-other-window(-down)
  (advice-add 'corfu--setup :after 'evil-normalize-keymaps)
  (advice-add 'corfu--teardown :after 'evil-normalize-keymaps)
  :config
  (corfu-popupinfo-mode) ; Documentation popup next to completion
  (corfu-history-mode))

;; TODO evil-org interferes with RET. This doesn't seem to be an issue in the
;; company module. See the fix for #1335 in company/config.el.
;; Elsewhere, unbinding RET when `corfu-preselect-first' is enabled allows us to
;; insert a newline without also completing
(after! corfu
  (define-key corfu-map (kbd "RET") nil))

(defun corfu-abort ()
  "Undo changes made while corfu was active and quit. Takes one keystroke compared
to two to three for corfu-reset but resets changes all at once rather than
incrementally."
  (interactive)
  (cancel-change-group corfu--change-group)
  (corfu-quit))

;; HACK `corfu-info-documentation' spawns a help buffer, which the popup module
;; catches, but the two are not yet compatible. `scroll-other-window' will
;; not scroll the help buffer, and "q" will quit the original buffer. (The latter
;; behavior is likely tied to corfu restoring the window configuration via
;; pre-command-hook.)
;; (defadvice! +popup--ignore-corfu-info-documentation (fn &rest args)
;;   :around #'corfu-info-documentation
;;   (push '("^\\*\\([Hh]elp\\|Apropos\\)" nil) display-buffer-alist)
;;   (apply fn args)
;;   (pop display-buffer-alist))

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

(use-package! cape)

(use-package! kind-icon
  :after corfu
  :init (setq kind-icon-default-face 'corfu-default)
  :config (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;;; org --------------------------------------------------------------

(setq org-directory "~/org"
      ;; Large code blocks can slow down `org-cycle-global' noticeably
      org-src-fontify-natively nil
      org-src-ask-before-returning-to-edit-buffer nil
      ;; HACK org-element throws a lot of errors about invalidated cache
      ;; Unclear if this will fix things...
      org-element-use-cache nil)

(after! org
  ;; When running org-attach, store a file link with absolute path to the
  ;; attached file. You can also store a link via embark-act, embark-copy-as-kill
  ;; (@w), then yank.
  (setq org-attach-store-link-p 'file
        org-attach-use-inheritance nil
        ;; When disabled, (C-)M-RET inserts a (sub)heading above
        ;; when called at beginning of line; else directly below
        org-insert-heading-respect-content nil))

;; Edit org src block in the same window as org file
(after! org
  (setq org-src-window-setup 'current-window)
  (advice-remove 'org-edit-src-exit '+popup--org-edit-src-exit-a)
  (assoc-delete-all "^\\*Org Src" display-buffer-alist))

(add-hook! 'org-src-mode-hook #'evil-normalize-keymaps)

;; Save edits and exit back to org file with "q"
(map! :map org-src-mode-map :n "q" #'my/org-edit-src-save-and-exit)
(defun my/org-edit-src-save-and-exit ()
  (interactive)
  (org-edit-src-save)
  (org-edit-src-exit)
  ;; Prevents accidental text insertion
  (evil-normal-state))

(defvar org-attach-ignore-regexp-list (list "." ".." ".DS_STORE")
  "A list of filenames for org-attach to ignore")
(defadvice! +org-attach-file-list-a (directory)
  "Return a list of files in the attachment DIRECTORY.
This ignores \".\", \"..\", \".DS_STORE\", and files ending in \"~\"."
  :override #'org-attach-file-list
  (delq nil
        (mapcar (lambda (x)
                  (if (string-match
                       (concat "^"
                               (regexp-opt
                                org-attach-ignore-regexp-list)
                               "\\'")
                       x) nil x))
                (directory-files directory nil "[^~]\\'"))))

;; This advice was creating huge lags for org-babel-tangle
(after! org
  (advice-remove 'org-export-to-file '+org--dont-trigger-save-hooks-a)
  (advice-remove 'org-babel-tangle '+org--dont-trigger-save-hooks-a))

(after! org
  (when IS-WSL
    ;; Open non-text files in Windows instead of WSL
    (setf (alist-get "\\.pdf\\'" org-file-apps nil nil #'string=) #'open-in-windows)
    ;; dired is unbelievably slow on Windows shared network drives
    (setf (alist-get 'directory org-file-apps) #'open-in-windows)
    (add-to-list 'org-file-apps '("\\.png?\\'" . open-in-windows) t)
    (add-to-list 'org-file-apps '("\\.xlsx?\\'" . open-in-windows) t)
    (add-to-list 'org-file-apps '("\\.docx?\\'" . open-in-windows) t)
    (add-to-list 'org-file-apps '("\\.pptx?\\'" . open-in-windows) t)
    (setq browse-url-generic-program "/mnt/c/Windows/System32/cmd.exe"
          browse-url-generic-args '("/c" "start" "")
          browse-url-browser-function 'browse-url-generic)
    ;; Windows-based attachment directory for WSL Emacs
    (setq org-attach-id-dir "/mnt/c/Users/jkroes/OneDrive - California Department of Pesticide Regulation (1)/org-attach")))

;; The org-transclude manual recommends removing this advice
(after! org
  (advice-remove 'org-link-search '+org--recenter-after-follow-link-a))

;; Updates to org-mode-map
(map! :map org-mode-map
      :localleader
      (:prefix ("l" . "links")
               "C" #'org-compress-link
               "y" #'org-store-link-to-filepath)
      (:prefix ("s" . "tree/subtree")
               ;; Pairs with org-cut-subtree
               ;; TODO org-yank shouldn't split text if not called at beginning of a heading
               "y" #'org-yank))

;; NOTE If you want normal org cycle behavior (subtree -> contents -> collapse), uncomment this
;; (remove-hook 'org-tab-first-hook '+org-cycle-only-current-subtree-h)

;; NOTE For this to work, the docs for `org-after-todo-statistics-hook' state that
;; the heading needs a statistics cookie. To insert a statistics cookie, manually
;; type "[/]" after e.g. a heading. As you toggle between todo and done for
;; subheadings, the cookie will update to display the fraction of entries marked
;; as done.
(add-hook 'org-after-todo-statistics-hook 'my/org-summary-todo)
(defun my/org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

;; C-RET (+org/insert-item-below) already enters insert state, so make M-RET
;; and C-M-RET do the same. We eschew evil-org-define-(b|e)ol-command so that
;; different behavior can be used at beg and end of line
(advice-add 'org-meta-return :after #'evil-insert-state)
(advice-add 'org-insert-subheading :after (lambda (&rest _) (evil-insert-state)))

(defadvice! org-insert-heading-a (&optional arg invisible-ok top)
  "Make (C-)M-RET respect property drawers when inserting
  (sub)heading below current heading, assuming that
  `org-insert-heading-respect-content' is nil."
  :override 'org-insert-heading
  (interactive "P")
  (let* ((blank? (org--blank-before-heading-p (equal arg '(16))))
         (level (org-current-level))
         (stars (make-string (if (and level (not top)) level 1) ?*)))
    (cond
     ((or org-insert-heading-respect-content
          (member arg '((4) (16)))
          (and (not invisible-ok)
               (invisible-p (max (1- (point)) (point-min)))))
      ;; Position point at the location of insertion.  Make sure we
      ;; end up on a visible headline if INVISIBLE-OK is nil.
      (org-with-limited-levels
       (if (not level) (outline-next-heading) ;before first headline
         (org-back-to-heading invisible-ok)
         (when (equal arg '(16)) (org-up-heading-safe))
         (org-end-of-subtree)))
      (unless (bolp) (insert "\n"))
      (when (and blank? (save-excursion
                          (backward-char)
                          (org-before-first-heading-p)))
        (insert "\n")
        (backward-char))
      (when (and (not level) (not (eobp)) (not (bobp)))
        (when (org-at-heading-p) (insert "\n"))
        (backward-char))
      (unless (and blank? (org-previous-line-empty-p))
        (org-N-empty-lines-before-current (if blank? 1 0)))
      (insert stars " ")
      ;; When INVISIBLE-OK is non-nil, ensure newly created headline
      ;; is visible.
      (unless invisible-ok
        (pcase (get-char-property-and-overlay (point) 'invisible)
          (`(outline . ,o)
           (move-overlay o (overlay-start o) (line-end-position 0)))
          (_ nil))))
     ;; At a headline...
     ((org-at-heading-p)
      (cond ((bolp)
             (when blank? (save-excursion (insert "\n")))
             (save-excursion (insert stars " \n"))
             (unless (and blank? (org-previous-line-empty-p))
               (org-N-empty-lines-before-current (if blank? 1 0)))
             (end-of-line))
            ((and (org-get-alist-option org-M-RET-may-split-line 'headline)
                  (org-match-line org-complex-heading-regexp)
                  (org-pos-in-match-range (point) 4))
             ;; Grab the text that should moved to the new headline.
             ;; Preserve tags.
             (let ((split (delete-and-extract-region (point) (match-end 4))))
               (if (looking-at "[ \t]*$") (replace-match "")
                 (org-align-tags))
               (end-of-line)
               (when blank? (insert "\n"))
               (insert "\n" stars " ")
               (when (org-string-nw-p split) (insert split))))
            ;; --- insert heading AFTER property drawer
            (t
             (re-search-forward org-property-end-re
                                (save-excursion
                                  (outline-next-heading)
                                  (point))
                                t)
             (end-of-line)
             (when blank? (insert "\n"))
             (insert "\n" stars " "))))
     ;; On regular text, turn line into a headline or split, if
     ;; appropriate.
     ((bolp)
      (insert stars " ")
      (unless (and blank? (org-previous-line-empty-p))
        (org-N-empty-lines-before-current (if blank? 1 0))))
     (t
      (unless (org-get-alist-option org-M-RET-may-split-line 'headline)
        (end-of-line))
      (insert "\n" stars " ")
      (unless (and blank? (org-previous-line-empty-p))
        (org-N-empty-lines-before-current (if blank? 1 0))))))
  (run-hooks 'org-insert-heading-hook))

;; TODO Once marginalia and embark are configured, check to see that this
;; function shows annotations and works with embark-act for attachment headings
(defadvice! +org/dwim-at-point-a (&optional arg)
  "Additionally edit source code blocks and call org-attach-open on attachment
headings."
  :override '+org/dwim-at-point
  (interactive "P")
  (if (button-at (point))
      (call-interactively #'push-button)
    (let* ((context (org-element-context))
           (type (org-element-type context)))
      ;; skip over unimportant contexts
      (while (and context (memq type '(verbatim code bold italic underline strike-through subscript superscript)))
        (setq context (org-element-property :parent context)
              type (org-element-type context)))
      (pcase type
        ((or `citation `citation-reference)
         (org-cite-follow context arg))

        (`headline
         (cond ((member "ATTACH" (org-get-tags nil t))
                ;; HACK To enable marginalia annotations (and embark-act, which
                ;; relies on the metadata marginalia sets), we either need to
                ;; bind this-command to org-attach-open or call it with
                ;; execute-extended-command
                (let ((this-command #'org-attach-open))
                  (org-attach-open)))
               ((memq (bound-and-true-p org-goto-map)
                      (current-active-maps))
                (org-goto-ret))
               ((and (fboundp 'toc-org-insert-toc)
                     (member "TOC" (org-get-tags)))
                (toc-org-insert-toc)
                (message "Updating table of contents"))
               ((string= "ARCHIVE" (car-safe (org-get-tags)))
                (org-force-cycle-archived))
               ((or (org-element-property :todo-type context)
                    (org-element-property :scheduled context))
                (org-todo
                 (if (eq (org-element-property :todo-type context) 'done)
                     (or (car (+org-get-todo-keywords-for (org-element-property :todo-keyword context)))
                         'todo)
                   'done))))
         ;; Update any metadata or inline previews in this subtree
         (org-update-checkbox-count)
         (org-update-parent-todo-statistics)
         (when (and (fboundp 'toc-org-insert-toc)
                    (member "TOC" (org-get-tags)))
           (toc-org-insert-toc)
           (message "Updating table of contents"))
         (let* ((beg (if (org-before-first-heading-p)
                         (line-beginning-position)
                       (save-excursion (org-back-to-heading) (point))))
                (end (if (org-before-first-heading-p)
                         (line-end-position)
                       (save-excursion (org-end-of-subtree) (point))))
                (overlays (ignore-errors (overlays-in beg end)))
                (latex-overlays
                 (cl-find-if (lambda (o) (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay))
                             overlays))
                (image-overlays
                 (cl-find-if (lambda (o) (overlay-get o 'org-image-overlay))
                             overlays)))
           (+org--toggle-inline-images-in-subtree beg end)
           (if (or image-overlays latex-overlays)
               (org-clear-latex-preview beg end)
             (org--latex-preview-region beg end))))

        (`clock (org-clock-update-time-maybe))

        (`footnote-reference
         (org-footnote-goto-definition (org-element-property :label context)))

        (`footnote-definition
         (org-footnote-goto-previous-reference (org-element-property :label context)))

        ((or `planning `timestamp)
         (org-follow-timestamp-link))

        ((or `table `table-row)
         (if (org-at-TBLFM-p)
             (org-table-calc-current-TBLFM)
           (ignore-errors
             (save-excursion
               (goto-char (org-element-property :contents-begin context))
               (org-call-with-arg 'org-table-recalculate (or arg t))))))

        (`table-cell
         (org-table-blank-field)
         (org-table-recalculate arg)
         (when (and (string-empty-p (string-trim (org-table-get-field)))
                    (bound-and-true-p evil-local-mode))
           (evil-change-state 'insert)))

        (`babel-call
         (org-babel-lob-execute-maybe))

        (`statistics-cookie
         (save-excursion (org-update-statistics-cookies arg)))

        ((or `src-block `inline-src-block)
         ;; (org-babel-execute-src-block arg))
         (org-edit-src-code))

        ((or `latex-fragment `latex-environment)
         (org-latex-preview arg))

        (`link
         (let* ((lineage (org-element-lineage context '(link) t))
                (path (org-element-property :path lineage)))
           (if (or (equal (org-element-property :type lineage) "img")
                   (and path (image-type-from-file-name path)))
               (+org--toggle-inline-images-in-subtree
                (org-element-property :begin lineage)
                (org-element-property :end lineage))
             (org-open-at-point arg))))

        (`paragraph
         (+org--toggle-inline-images-in-subtree))

        ((guard (org-element-property :checkbox (org-element-lineage context '(item) t)))
         (let ((match (and (org-at-item-checkbox-p) (match-string 1))))
           (org-toggle-checkbox (if (equal match "[ ]") '(16)))))

        (_
         (if (or (org-in-regexp org-ts-regexp-both nil t)
                 (org-in-regexp org-tsr-regexp-both nil  t)
                 (org-in-regexp org-link-any-re nil t))
             (call-interactively #'org-open-at-point)
           (+org--toggle-inline-images-in-subtree
            (org-element-property :begin context)
            (org-element-property :end context))))))))

;;; org-roam --------------------------------------------------

(setq org-roam-directory org-directory)

(after! org-roam
  (setq ;; Only complete roam node titles within link brackets
        org-roam-completion-everywhere nil
        ;; Exclude ATTACH tags from org-roam database. If this isn't working,
        ;; try running `org-roam-db-clear-all',then `org-roam-db-sync'
        org-roam-db-node-include-function
        (lambda ()
          (not (member org-attach-auto-tag (org-get-tags nil t))))))

;; NOTE This hides and prevents searching on tags in e.g. org-roam-node-find
;; but does not exclude nodes with those tags. To excludes nodes by tag, see
;; `org-roam-db-node-include-function'
(after! org-roam
  (cl-defmethod org-roam-node-doom-tags2 ((node org-roam-node))
    "Return tags formatted in the same way how they appear in org files."
    (cl-remove-if (doom-rpartial
                   #'member (delq
                             nil (append
                                  (list (bound-and-true-p org-archive-tag)
                                        (bound-and-true-p org-attach-auto-tag))
                                  ;; Omit vulpea tag
                                  ;; TODO Update this when you create a variable
                                  ;; to customize the vulpea tag
                                  (list "project")
                                  (bound-and-true-p org-num-skip-tags))))
                  (org-roam-node-tags node))))

;; Replace all roam: links generated by org-roam completion with id: links, on
;; save.
(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'after-save-hook #'org-roam-link-replace-all nil t)))

;; TODO Test this to make sure it works with dendroam, keeping the title and
;; filename properly synced
;; NOTE org-roam v2 got rid of features to auto-update link descriptions when
;; a file's title changes. See `git log -G org-roam--setup-title-auto-update'
;; and https://org-roam.discourse.group/t/org-roam-v2-rename-file-or-title/1691/11
(defun azr/org-roam-modify-title ()
  "Modify title of current node and update the description portion of all ID
links to this node (i.e., of all backlinks) to the new title"
  (interactive)
  (unless (org-roam-buffer-p) (error "Not in an org-roam buffer."))
  ;; Save all buffers without prompt.
  (save-some-buffers t)
  ;; Update the title
  (let* ((old-title (org-roam-get-keyword "title"))
         (ID (org-entry-get (point) "ID"))
         (new-title (read-string "Enter new title: " old-title)))
    (org-roam-set-keyword "title" new-title)
    (save-buffer)
    ;; Update the buffer name and filename
    (let* ((new-slug (org-roam-node-slug (org-roam-node-at-point)))
           (new-file-name (replace-regexp-in-string "-.*\\.org" (format "-%s.org" new-slug) (buffer-file-name)))
           (new-buffer-name (file-name-nondirectory new-file-name)))
      (rename-buffer new-buffer-name)
      (rename-file (buffer-file-name) new-file-name 1)
      ;; OG Author: I don't know why this last command is necessary. Getting it from here:
      ;; https://stackoverflow.com/a/384346/2422698
      (set-visited-file-name new-file-name))
    (save-buffer)
    ;; Rename backlinks in the rest of the Org-roam database.
    (let* ((search (format "[[id:%s][%s]]" ID old-title))
           (replace (format "[[id:%s][%s]]" ID new-title))
           (rg-command (format "rg -t org -lF '%s' '%s'" search org-roam-directory))
           (file-list (split-string (shell-command-to-string rg-command))))
      (dolist (file file-list)
        (let ((file-open (get-file-buffer file)))
	  (find-file file)
          (beginning-of-buffer)
          (while (search-forward search nil t)
            (replace-match replace))
          (save-buffer)
          (unless file-open
            (kill-buffer)))))))

;;; dendroam --------------------------------------------------------------------

;; This code is based on
;; https://github.com/vicrdguez/dendroam/blob/main/dendroam.el

;; NOTE Nodes can be created by running org-roam-node-find;
;; org-roam-node-insert; or by typing a roam link like
;; [[roam:this is a roam link]], moving the cursor onto it,
;; and running org-open-at-point. The latter behavior is
;; similar to how dendroam works, where following a link
;; creates a new node.

;; TODO dendroam-find is not available until lazy loading loads org-roam. The
;; dendroam commands need to load it somehow, then remove this require
;; statement
(require 'org-roam)

;; TODO Create functions to create dendroam meeting and datetime notes. They
;; should prompt for a node, with current node title as initial input, then
;; prompt for a title for the child node. Currently, you need to navigate to
;; call org-roam-node-find twice to create such a note. Once to get to the
;; parent note, a second time to create the child note.
(after! org-roam
  (setq org-roam-capture-templates
        '(("d" "dendroam" plain "%?"
           :target (file+head
                    "${dendroam-slug}.org" "#+title: ${dendroam-title}")
           :immediate-finish t)
          ;; Scratch notes
          ("s" "scratch" entry "* %<%Y%m%d%H%M%S>.${title} %?"
           :target (file+head "scratch.org" "#+title: scratch")
           :jump-to-captured t
           :immediate-finish t
           :empty-lines 2)
          ;; Useful for making scratch notes under a topic. Will technically
          ;; work if called outside of a org-roam note, but that is not the
          ;; intention
          ("t" "datetime" plain "%?"
           :target (file+head "${dendroam-hierarchy}.%<%Y%m%d%H%M%S>.org" "#+title: ${title}")
           :immediate-finish t)
          ("m" "meeting" plain "%?"
           :target (file+head "${dendroam-hierarchy}.%<%Y%m%d>.org" "#+title: ${title}")
           :immediate-finish t)
          ))

  ;; NOTE For more on how structures like org-roam-node are related to
  ;; cl-defmethod, see https://nullprogram.com/blog/2018/02/14/ and
  ;; https://www.orgroam.com/manual.html#Accessing-and-Modifying-Nodes

  ;; NOTE All specializers created via cl-defmethod need to be wrapped
  ;; in a call to after! to avoid an error.

  (cl-defmethod org-roam-node-dendroam-slug ((node org-roam-node))
    "Return the input with non-alphanumeric characters replaced with underscores,
except for periods, dashes, and underscores."
    (let* ((title (org-roam-node-title node))
           (slug-trim-chars '(;; Combining Diacritical Marks https://www.unicode.org/charts/PDF/U0300.pdf
                              768 ; U+0300 COMBINING GRAVE ACCENT
                              769 ; U+0301 COMBINING ACUTE ACCENT
                              770 ; U+0302 COMBINING CIRCUMFLEX ACCENT
                              771 ; U+0303 COMBINING TILDE
                              772 ; U+0304 COMBINING MACRON
                              774 ; U+0306 COMBINING BREVE
                              775 ; U+0307 COMBINING DOT ABOVE
                              776 ; U+0308 COMBINING DIAERESIS
                              777 ; U+0309 COMBINING HOOK ABOVE
                              778 ; U+030A COMBINING RING ABOVE
                              779 ; U+030B COMBINING DOUBLE ACUTE ACCENT
                              780 ; U+030C COMBINING CARON
                              795 ; U+031B COMBINING HORN
                              803 ; U+0323 COMBINING DOT BELOW
                              804 ; U+0324 COMBINING DIAERESIS BELOW
                              805 ; U+0325 COMBINING RING BELOW
                              807 ; U+0327 COMBINING CEDILLA
                              813 ; U+032D COMBINING CIRCUMFLEX ACCENT BELOW
                              814 ; U+032E COMBINING BREVE BELOW
                              816 ; U+0330 COMBINING TILDE BELOW
                              817 ; U+0331 COMBINING MACRON BELOW
                              ))
           (leading-dash (string-match-p "^-" title))
           (trailing-dash (string-match-p "-$" title)))
      (cl-flet* ((nonspacing-mark-p (char) (memq char slug-trim-chars))
                 (strip-nonspacing-marks (s) (string-glyph-compose
                                              (apply #'string
                                                     (seq-remove #'nonspacing-mark-p
                                                                 (string-glyph-decompose s)))))
                 (cl-replace (title pair) (replace-regexp-in-string (car pair) (cdr pair) title)))
        (let* ((pairs `(("[^[:alnum:][:digit:].-]" . "_")
                        ("__*" . "_")   ; remove sequential underscore
                        ("^_*" . "")    ; remove starting underscore
                        ("_*$" . "")    ; remove ending underscore
                        ))
               (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
          (downcase slug)))))


  (cl-defmethod org-roam-node-dendroam-title (node)
    "Node title is the last component of the dendroam hierarchy."
    (car (last (split-string (org-roam-node-dendroam-hierarchy node) "\\."))))

  (cl-defmethod org-roam-node-dendroam-hierarchy ((node org-roam-node))
    (file-name-base (org-roam-node-file node)))

  (cl-defmethod org-roam-node-dendroam-hierarchy-no-title ((node org-roam-node))
    (dendroam-up-hierarchy (org-roam-node-dendroam-hierarchy node)))

  (defun dendroam-up-hierarchy (hierarchy)
    (string-join (butlast (split-string hierarchy "\\.")) "."))


  ;; BUG When using "${dendroam-full-hierarchy:*}", vertico-insert cannot
  ;; properly insert candidates into the minibuffer for functions like
  ;; org-roam-node-find that rely on this template. If you check *Messages*
  ;; buffer when debugging vertico-insert, the insertion is a node, not text.
  ;; Insertion comes with wrapping as well.
  ;; See https://github.com/org-roam/org-roam/issues/2066
  ;; +org--roam-fix-completion-width-for-vertico-a is doom's fix, but this
  ;; advice no longer works after a recent update
  (setq org-roam-node-display-template
        ;; (format "${dendroam-full-hierarchy:*} %s"
        ;;         ;; Zero-width components can still be filtered/searched on
        ;;         (propertize "${doom-tags2:40}" 'face 'org-tag)))
        "${dendroam-full-hierarchy}")

  (cl-defmethod org-roam-node-dendroam-full-hierarchy ((node org-roam-node))
    "Return hierarchy for NODE, constructed of its file title, OLP and direct title.
If some elements are missing, they will be stripped out."
    (let* ((title     (org-roam-node-title node))
           (hierarchy (org-roam-node-dendroam-hierarchy node))
           ;; (separator (propertize org-eldoc-breadcrumb-separator 'face 'shadow))
           ;;(hier-list (string-join (split-string hierarchy "\\.") separator))
           (separator (propertize org-eldoc-breadcrumb-separator 'face 'shadow))
           (olp       (org-roam-node-olp   node))
           (level     (org-roam-node-level node))
           (filetitle (org-roam-node-doom-filetitle node)))
      (cl-case level
        ;; node is a top-level file
        (0 hierarchy)
        ;; node is a level 1 heading
        (1 (concat (propertize hierarchy 'face '(shadow italic))
                   separator title))
        ;; node is a heading with an arbitrary outline path
        (t (concat (propertize hierarchy 'face '(shadow italic))
                   separator (propertize (string-join olp separator) 'face '(shadow italic))
                   separator title)))))

  (defun dendroam-refactor-hierarchy ()
    "Rename current note and all of its children"
    (interactive)
    (dendroam--refactor-hierarchy (org-roam-node-at-point)))

  ;; TODO Refactor this and dendrom--rename-note so that this calls the latter
  ;; for each file it needs to change?
  (cl-defmethod dendroam--refactor-hierarchy ((node org-roam-node))
    (let* ((hierarchy (org-roam-node-dendroam-hierarchy node))
           (new-hierarchy (read-string "Refactor: " hierarchy))
           (files (dendroam-sibling-files hierarchy))
           ;; Can't use org-roam-node-dendroam-title b/c it returns the old title,
           ;; presumably until the org-roam db syncs
           (new-title (car (last (split-string new-hierarchy "\\.")))))
      (dolist (file files)
        (let ((new-file (replace-regexp-in-string hierarchy new-hierarchy file)))
          (save-some-buffers)
          (rename-file file new-file)
          (if (equal buffer-file-name file)
              (progn
                (kill-current-buffer)
                (find-file new-file)
                ;; Update the title of the current node
                (org-roam-set-keyword "title" new-title)
                (save-buffer))
            ;; TODO This should update open buffers for all modified files, not
            ;; just the current buffer. See azr/org-roam-modify-title. In the
            ;; meantime, here is my hack.
            (kill-buffer (get-file-buffer file)))))))

  (defun dendroam-rename-note ()
    "Rename current note only (i.e., preserve hierarchy)."
    (interactive)
    (dendroam--rename-note (org-roam-node-at-point)))

  (cl-defmethod dendroam--rename-note ((node org-roam-node))
    (let* ((hierarchy (org-roam-node-dendroam-hierarchy node))
           (new-hierarchy (read-string "Rename: " hierarchy))
           ;; Can't use org-roam-node-dendroam-title b/c it returns the old title,
           ;; presumably until the org-roam db syncs
           (new-title (car (last (split-string new-hierarchy "\\."))))
           (file (buffer-file-name))
           (new-file (replace-regexp-in-string hierarchy new-hierarchy file)))
          (save-buffer)
          (rename-file file new-file)
          (kill-current-buffer)
          (find-file new-file)
          ;; Update the title of the current node for nodes where the title
          ;; should match the last component of the hierarchy
          (unless (or (dendroam--meeting-note node)
                      (dendroam--datetime-note node))
            (org-roam-set-keyword "title" new-title))
          (save-buffer)))

  (cl-defmethod dendroam--datetime-note ((node org-roam-node))
    "Return t if org-roam note is a dendroam datetime note"
    (string-match-p "^[0-9]\\{14\\}$" (org-roam-node-dendroam-title node)))

  (cl-defmethod dendroam--meeting-note ((node org-roam-node))
    "Return t if org-roam note is a dendroam meeting note"
    (require 'dash)
    (let* ((title (org-roam-node-dendroam-title node))
           (dmy (butlast (nthcdr 3 (parse-time-string title)) 3)))
      (not (-any 'null dmy))))

  ;; TODO Keep testing this. I would prefer to do simple file matching in case
  ;; the database is out of sync
  (defun dendroam-sibling-files (hierarchy)
    "Gets all the nodes that share the same HIERARCHY"
    (mapcar #'car (org-roam-db-query [:select [file]
                                      :from nodes
                                      :where (like file $r1)]
                                     (concat "%" hierarchy "%"))))

  ;; HACK When calling org-roam-node-find or any other function that calls
  ;; org-roam-node-read, `DEL' deletes a character, or if the preceding character
  ;; is a period and not the first component deletes to the nearest period. In
  ;; other words, it does like `vertico-directory-delete-char' but with `.'
  ;; instead of `/'.
  ;; NOTE You can always just use C-w if you always want to delete back to `.'
  (defun dendroam-up (&optional n)
    "Delete N directories before point."
    (interactive "p")
    (when (and (> (point) (minibuffer-prompt-end))
               (eq (char-before) ?.)
               (eq 'org-roam-node (vertico--metadata-get 'category)))
      (let ((path (buffer-substring (minibuffer-prompt-end) (point))) found)
        (dotimes (_ (or n 1) found)
          (save-excursion
            (let ((end (point)))
              (goto-char (1- end))
              (when (search-backward "." (minibuffer-prompt-end) t)
                (delete-region (1+ (point)) end)
                (setq found t))))))))

  (defun delete-dendroam (&optional n)
    "Delete N directories or chars before point."
    (interactive "p")
    (unless (dendroam-up n)
      (backward-delete-char n)))

  (defadvice! my/delete--dendroam (fn &rest args)
    :around #'org-roam-node-read
    (cl-letf (((symbol-function  'vertico-directory-delete-char) #'delete-dendroam))
      (apply fn args)))

  (defun dendroam-find-siblings ()
    (interactive)
    (dendroam--find-siblings (or (and (eq major-mode 'org-mode) (org-roam-node-at-point)) "")))

  (cl-defmethod dendroam--find-siblings ((node org-roam-node))
      (org-roam-node-find nil (org-roam-node-dendroam-hierarchy-no-title node)))

  (cl-defmethod dendroam--find-siblings ((str string))
      (org-roam-node-find nil str))

  (defun dendroam-find ()
    (interactive)
    (dendroam--find (or (and (eq major-mode 'org-mode) (org-roam-node-at-point)) "")))

  (cl-defmethod dendroam--find ((node org-roam-node))
      (org-roam-node-find nil (concat (org-roam-node-dendroam-hierarchy node) ".")))

  (cl-defmethod dendroam--find ((str string))
      (org-roam-node-find nil str))

  ;; TODO Create functions for new meeting and datetime notes based on this
  ;; function, which is based on org-roam-node-find and org-roam-node-read
  ;; TODO If the template or template keys change, this function can break.
  ;; TODO Search for parent node rather than parent file? See dendroam-sibling-files
  (defun dendroam-go-up-hierarchy ()
    "Find and visit parent node, creating one if nonexistent.
This is a convenience function that skips the org-roam-node-find."
    (interactive)
    (let* ((node (org-roam-node-at-point))
           (file (org-roam-node-file node))
           (dir (f-dirname file))
           (ext (f-ext file))
           (parent-file (concat-path dir (concat (org-roam-node-dendroam-hierarchy-no-title node) "." ext)))
           (parent-hierarchy (org-roam-node-dendroam-hierarchy-no-title node)))
      (unless (length= parent-hierarchy 0)
        (if (file-exists-p parent-file)
            (find-file parent-file)
          (org-roam-capture-
           :node (org-roam-node-create :title parent-hierarchy)
           :keys "d"
           ;;:templates org-roam-capture-templates
           :props '(:finalize find-file))
          ))))
  )



;;; org-appear -------------------------------------------

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

;;; org-superstar ----------------------------------------------------

;; NOTE As of 4/16/23, I switched from org-visual-indent and
;; org-dynamic-bullets to org-superstar. Commits prior to
;; this time contain code for the deprecated packages in
;; modules/lang/org/contrib/pretty.el.

;; pretty.el makes a mistake. It is not enough to set org-hide-leading-stars to
;; nil; you need to make sure org-indent-mode, which doom enables by default,
;; does not enable this variable buffer locally.
(setq org-indent-mode-turns-on-hiding-stars nil)
;; Implement org-superstar-todo-bullet-alist
(after! org-superstar
  (setq org-superstar-special-todo-items t)
  (org-superstar-restart))

;;; emacs lisp -------------------------------------------------

;; TODO Remove this if no longer needed. From the backup config.
;; (defvar +emacs-lisp-outline-regexp "[ \t]*;;;;* [^ \t\n]")

;; Due to alterations to private company module, emacs-lisp otherwise has
;; empty `company-backends'
(set-company-backend! 'emacs-lisp-mode '(company-capf))

;;; modeline ---------------------------------------------------

(setq doom-modeline-modal-icon nil) ;; Letter instead of icon for evil state

;; Adds `modals' to the list of segments for info's modeline. This segment
;; shows evil states, which for some reason are shown for special buffers
;; aside from info. Compare to original value for doom-modeline-format--info
(after! doom-modeline
  (doom-modeline-def-modeline 'info
    '(bar window-number modals buffer-info info-nodes buffer-position parrot
      selection-info)
    '(misc-info buffer-encoding major-mode)))

;; TODO The first info buffer shows the modeline, but successive buffers do not.
;; Investigate the modeline rules for popups. In the meantime, disable modeline
;; hiding for popups. Oh, but this doesn't work yet. Fucking Doom help docs.
;;(plist-put +popup-defaults :modeline t)

;;; evil

(setq evil-split-window-below t
      evil-vsplit-window-right t)

;;; default

;; HACK If a comment does not have a blank line above it, it blocks RET at the
;; start of a line from inserting a blank line above. Until you fix this
;; advice, it should be removed.
(advice-remove 'newline-and-indent #'+default--newline-indent-and-continue-comments-a)

;;; Keybindings ----------------------------------------------------------

(map! :m "zh"     #'evil-scroll-left
      :m "zl"     #'evil-scroll-right)

(map! :map org-mode-map
      :leader
      (:prefix ("n")
               "d" nil
               (:prefix ("d" . "dendroam")
                        "d" #'dendroam-find ; go down
                        "u" #'dendroam-go-up-hierarchy ; go up
                        "R" #'dendroam-refactor-hierarchy
                        "r" #'dendroam-rename-note)
               (:prefix "r"
                        ;; These methods fall back to org-roam-node-find
                        ;; without initial input in non-(dend)roam files
                        :desc "Find node (current)" "F" #'dendroam-find)))
                        ;;:desc "Find sibling nodes" "F" #'dendroam-find-siblings)))

;; For WSL
(map! :i "C-v" #'evil-paste-after)
