;;; completion/vertico/autoload/vertico.el -*- lexical-binding: t; -*-

;; To prevent "Defining as dynamic an already lexical var" from +vertico/embark-preview
;;;###autoload
(defvar embark-quit-after-action)

;;;###autoload
(cl-defun +vertico-file-search (&key query in all-files (recursive t) prompt args)
  "Conduct a file search using ripgrep.

:query STRING
  Determines the initial input to search for.
:in PATH
  Sets what directory to base the search out of. Defaults to the current project's root.
:recursive BOOL
  Whether or not to search files recursively from the base directory."
  (declare (indent defun))
  (unless (executable-find "rg")
    (user-error "Couldn't find ripgrep in your PATH"))
  (require 'consult)
  (setq deactivate-mark t)
  (let* ((project-root (or (doom-project-root) default-directory))
         (directory (or in project-root))
         (consult-ripgrep-args
          (concat "rg "
                  (if all-files "-uu ")
                  (unless recursive "--maxdepth 1 ")
                  "--null --line-buffered --color=never --max-columns=1000 "
                  "--path-separator /   --smart-case --no-heading "
                  "--with-filename --line-number --search-zip "
                  "--hidden -g !.git -g !.svn -g !.hg "
                  (mapconcat #'shell-quote-argument args " ")))
         (prompt (if (stringp prompt) (string-trim prompt) "Search"))
         (query (or query
                    (when (doom-region-active-p)
                      (regexp-quote (doom-thing-at-point-or-region)))))
         (consult-async-split-style consult-async-split-style)
         (consult-async-split-styles-alist consult-async-split-styles-alist))
    ;; Change the split style if the initial query contains the separator.
    (when query
      (cl-destructuring-bind (&key type separator initial _function)
          (consult--async-split-style)
        (pcase type
          (`separator
           (replace-regexp-in-string (regexp-quote (char-to-string separator))
                                     (concat "\\" (char-to-string separator))
                                     query t t))
          (`perl
           (when (string-match-p initial query)
             (setf (alist-get 'perlalt consult-async-split-styles-alist)
                   `(:initial ,(or (cl-loop for char in (list "%" "@" "!" "&" "/" ";")
                                            unless (string-match-p char query)
                                            return char)
                                   "%")
                     :type perl)
                   consult-async-split-style 'perlalt))))))
    (consult--grep prompt #'consult--ripgrep-make-builder directory query)))

;;;###autoload
(defun +vertico/project-search (&optional arg initial-query directory)
  "Performs a live project search from the project root using ripgrep.
If ARG (universal argument), include all files, even hidden or compressed ones,
in the search."
  (interactive "P")
  (+vertico-file-search :query initial-query :in directory :all-files arg))

;;;###autoload
(defun +vertico/project-search-from-cwd (&optional arg initial-query)
  "Performs a live project search from the current directory.
If ARG (universal argument), include all files, even hidden or compressed ones."
  (interactive "P")
  (+vertico/project-search arg initial-query default-directory))

;;;###autoload
(defun +vertico/search-symbol-at-point ()
  "Performs a search in the current buffer for thing at point."
  (interactive)
  (consult-line (thing-at-point 'symbol)))

;;;###autoload
(defun +vertico-embark-target-package-fn ()
  "Targets Doom's package! statements and returns the package name"
  (when (or (derived-mode-p 'emacs-lisp-mode) (derived-mode-p 'org-mode))
    (save-excursion
      (when (and (search-backward "(" nil t)
                 (looking-at "(\\s-*package!\\s-*\\(\\(\\sw\\|\\s_\\)+\\)\\s-*"))
        (let ((pkg (match-string 1)))
          (set-text-properties 0 (length pkg) nil pkg)
          `(package . ,pkg))))))

;;;###autoload
(defun +vertico/embark-export-write ()
  "Export the current vertico results to a writable buffer if possible.

Supports exporting consult-grep to wgrep, file to wdeired, and consult-location to occur-edit"
  (interactive)
  (require 'embark)
  (require 'wgrep)
  (let* ((edit-command
          (pcase-let ((`(,type . ,candidates)
                       (run-hook-with-args-until-success 'embark-candidate-collectors)))
            (pcase type
              ('consult-grep #'wgrep-change-to-wgrep-mode)
              ('file #'wdired-change-to-wdired-mode)
              ('consult-location #'occur-edit-mode)
              (x (user-error "embark category %S doesn't support writable export" x)))))
         (embark-after-export-hook `(,@embark-after-export-hook ,edit-command)))
    (embark-export)))

;;;###autoload
(defun +vertico/embark-preview ()
  "Previews candidate in vertico buffer, unless it's a consult command"
  (interactive)
  (unless (bound-and-true-p consult--preview-function)
    (if (fboundp 'embark-dwim)
        (save-selected-window
          (let (embark-quit-after-action)
            (embark-dwim)))
      (user-error "Embark not installed, aborting..."))))

;;;###autoload
(defun +vertico/enter-or-preview ()
  "Enter directory or embark preview on current candidate."
  (interactive)
  (when (> 0 vertico--index)
    (user-error "No vertico session is currently active"))
  (if (and (let ((cand (vertico--candidate)))
             (or (string-suffix-p "/" cand)
                 (and (vertico--remote-p cand)
                      (string-suffix-p ":" cand))))
           (not (equal vertico--base ""))
           (eq 'file (vertico--metadata-get 'category)))
      (vertico-insert)
    (condition-case _
        (+vertico/embark-preview)
      (user-error (vertico-directory-enter)))))

(defvar +vertico/find-file-in--history nil)
;;;###autoload
(defun +vertico/find-file-in (&optional dir initial)
  "Jump to file under DIR (recursive).
If INITIAL is non-nil, use as initial input."
  (interactive)
  (require 'consult)
  (let* ((default-directory (or dir default-directory))
         (prompt-dir (consult--directory-prompt "Find" default-directory))
         (cmd (split-string-and-unquote +vertico-consult-fd-args " ")))
    (find-file
     (consult--read
      (split-string (cdr (apply #'doom-call-process cmd)) "\n" t)
      :prompt default-directory
      :sort nil
      :initial (if initial (shell-quote-argument initial))
      :add-history (thing-at-point 'filename)
      :category 'file
      :history '(:input +vertico/find-file-in--history)))))

;;;###autoload
(defun +vertico/jump-list (jump)
  "Go to an entry in evil's (or better-jumper's) jumplist."
  (interactive
   (let (buffers)
     (require 'consult)
     (unwind-protect
         (list
          (consult--read
           ;; REVIEW Refactor me
           (nreverse
            (delete-dups
             (delq
              nil (mapcar
                   (lambda (mark)
                     (when mark
                       (cl-destructuring-bind (path pt _id) mark
                         (let* ((visiting (find-buffer-visiting path))
                                (buf (or visiting (find-file-noselect path t)))
                                (dir default-directory))
                           (unless visiting
                             (push buf buffers))
                           (with-current-buffer buf
                             (goto-char pt)
                             (font-lock-fontify-region
                              (line-beginning-position) (line-end-position))
                             (format "%s:%d: %s"
                                     (car (cl-sort (list (abbreviate-file-name (buffer-file-name buf))
                                                         (file-relative-name (buffer-file-name buf) dir))
                                                   #'< :key #'length))
                                     (line-number-at-pos)
                                     (string-trim-right (or (thing-at-point 'line) ""))))))))
                   (cddr (better-jumper-jump-list-struct-ring
                          (better-jumper-get-jumps (better-jumper--get-current-context))))))))
           :prompt "jumplist: "
           :sort nil
           :require-match t
           :category 'jump-list))
       (mapc #'kill-buffer buffers))))
  (if (not (string-match "^\\([^:]+\\):\\([0-9]+\\): " jump))
      (user-error "No match")
    (let ((file (match-string-no-properties 1 jump))
          (line (match-string-no-properties 2 jump)))
      (find-file file)
      (goto-char (point-min))
      (forward-line (string-to-number line)))))

;;;###autoload
(defun +vertico-embark-which-key-indicator ()
  "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
  (lambda (&optional keymap targets prefix)
    (if (null keymap)
        (which-key--hide-popup-ignore-command)
      (which-key--show-keymap
       (if (eq (plist-get (car targets) :type) 'embark-become)
           "Become"
         (format "Act on %s '%s'%s"
                 (plist-get (car targets) :type)
                 (embark--truncate-target (plist-get (car targets) :target))
                 (if (cdr targets) "…" "")))
       (if prefix
           (pcase (lookup-key keymap prefix 'accept-default)
             ((and (pred keymapp) km) km)
             (_ (key-binding prefix 'accept-default)))
         keymap)
       nil nil t (lambda (binding)
                   (not (string-suffix-p "-argument" (cdr binding))))))))

;;;###autoload
(defun +vertico--consult--fd-make-builder ()
  (let ((cmd (split-string-and-unquote +vertico-consult-fd-args)))
    (lambda (input)
      (pcase-let* ((`(,arg . ,opts) (consult--command-split input))
                   (`(,re . ,hl) (funcall consult--regexp-compiler
                                          arg 'extended t)))
        (when re
          (cons (append cmd
                        (list (consult--join-regexps re 'extended))
                        opts)
                hl))))))

(autoload #'consult--directory-prompt "consult")
;;;###autoload
(defun +vertico/consult-fd (&optional dir initial)
  (interactive "P")
  (if doom-projectile-fd-binary
      (pcase-let* ((`(,prompt ,paths ,dir) (consult--directory-prompt "Fd" dir))
                   (default-directory dir)
                   (builder (consult--find-make-builder paths)))
        (find-file (consult--find prompt builder initial)))
    (consult-find dir initial)))

;;;###autoload
(defun +vertico-basic-remote-try-completion (string table pred point)
  (and (vertico--remote-p string)
       (completion-basic-try-completion string table pred point)))

;;;###autoload
(defun +vertico-basic-remote-all-completions (string table pred point)
  (and (vertico--remote-p string)
       (completion-basic-all-completions string table pred point)))

;;;###autoload
(defun my/embark-prefix-help-command (popup-showing)
  "Prompt for and run a command bound in the prefix used to reach this command.
This command is intended to be used as the value of
`which-key--prefix-help-cmd-backup' when `prefix-help-command' is set to
`which-key-C-h-dispatch' and `which-key-use-C-h-commands' is set to `t'. `which-key-C-h-dispatch' calls
`which-key-show-standard-help', which calls `which-key--prefix-help-cmd-backup',
which by default is set to the original value of `prefix-help-command' when
`which-key-mode' is first enabled. In Doom Emacs with `vertico',
`prefix-help-command' is typically the original `embark-prefix-help-command'
that this function replaces. Note that this function must be called from a
modified version of `which-key-show-standard-help' that passes `popup-showing'.

In addition to using completion to select a command, you can also type @ and the
key binding (without the prefix)."
  (interactive)
  (let (keys)
    (if popup-showing
        (setq keys (which-key--current-prefix))
      (setq keys (this-command-keys-vector))
      (setq keys (seq-take keys (1- (length keys)))))
    (embark-bindings keys)))

;;;###autoload
(defun my/which-key-show-standard-help (&optional _)
  "Call the command in `which-key--prefix-help-cmd-backup'.
Usually this is `describe-prefix-bindings'.

Unlike the original function, `popup-showing' is passed t
`which-key--prefix-help-cmd-backup'."
  (interactive)
  (let ((which-key-inhibit t)
        (popup-showing (which-key--popup-showing-p)))
    (which-key--hide-popup-ignore-command)
    (cond ((and (eq which-key--prefix-help-cmd-backup
                    'describe-prefix-bindings)
                ;; If the popup is not showing, we call
                ;; `describe-prefix-bindings' directly.
                popup-showing)
           ;; This is essentially what `describe-prefix-bindings' does. We can't
           ;; use this function directly, because the prefix will not be correct
           ;; when we enter using `which-key-C-h-dispatch'.
           (describe-bindings (kbd (which-key--current-key-string))))
          ((functionp which-key--prefix-help-cmd-backup)
           (funcall which-key--prefix-help-cmd-backup popup-showing)))))

;; HACK Later versions of embark altered this function so that it no
;; longer filters bindings by the current key prefix. This is the
;; original definition from commit 35f3961cd1e6
;;;###autoload
(defun my/embark-bindings (&optional prefix)
  "Explore all current keybindings and commands with `completing-read'.
The selected command will be executed. The set keybindings can be restricted
by passing a PREFIX key."
  (interactive)
  (let ((keymap (if prefix
                    (key-binding prefix)
                  (make-composed-keymap (current-active-maps t)))))
    (unless (keymapp keymap)
      (user-error "No keybindings found"))
    (when-let (command (embark-completing-read-prompter keymap 'no-default))
      (call-interactively command))))

;; Note that the original command is still called when using
;; execute-extended-command. Only keybindings are remapped.
;;;###autoload
(defun my/marginalia-annotate-binding (cand)
  "Annotate command CAND with keybinding. If CAND is remapped to
  OTHER-COMMAND, return [remap OTHER-COMMAND]."
  (when-let* ((sym (intern-soft cand))
              (key (and (commandp sym) (where-is-internal sym nil
                                                          'first-only))))
    (let ((remap (command-remapping sym)))
      (propertize (format " (%s)" (if remap remap
                                    (key-description key)))
                  'face 'marginalia-key))))

;;;###autoload
(defun my/marginalia-annotate-variable (cand)
  (when-let (sym (intern-soft cand))
    (marginalia--fields
     ;; ((marginalia--symbol-class sym) :face 'marginalia-type)
     ((marginalia--variable-value sym))
     ;; ((marginalia--variable-value sym) :truncate 0.5)
     ;; ((documentation-property sym 'variable-documentation)
     ;;  :truncate 1.0 :face 'marginalia-documentation)
     )))

;; This affects describe-function
;;;###autoload
(defun my/marginalia-annotate-function (cand)
  (when-let (sym (intern-soft cand))
    (when (fboundp sym)
      (marginalia--fields
       (:left (marginalia-annotate-binding cand))
       ;; ((marginalia--symbol-class sym) :face 'marginalia-type)
       ;; ((marginalia--function-args sym) :face 'marginalia-value
       ;;  :truncate 0.5)
       ((marginalia--function-doc sym) :truncate 1.0
        :face 'marginalia-documentation)))))

;; This affects helpful-callable
;;;###autoload
(defun my/marginalia-annotate-symbol (cand)
  (when-let (sym (intern-soft cand))
    (marginalia--fields
     (:left (marginalia-annotate-binding cand))
     ;;((marginalia--symbol-class sym) :face 'marginalia-type)
     ((cond
       ((fboundp sym) (marginalia--function-doc sym))
       ((facep sym) (documentation-property sym 'face-documentation))
       (t (documentation-property sym 'variable-documentation)))
      :truncate 1.0 :face 'marginalia-documentation)
     ;; ((abbreviate-file-name (or (symbol-file sym) ""))
     ;;  :truncate -0.5 :face 'marginalia-file-name)
     )))

;;;###autoload
(defun embark--expand-attachment (_ target)
  (with-current-buffer (window-buffer (minibuffer-selected-window))
    (cons 'file (expand-file-name target (org-attach-dir)))))

;;;###autoload
(defun marginalia-annotate-attachment (cand)
  (marginalia-annotate-file (cdr (embark--expand-attachment nil cand))))
