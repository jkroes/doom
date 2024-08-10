(defconst NATIVECOMP (if (fboundp 'native-comp-available-p)
                         (native-comp-available-p)))
(defconst EMACS28+   (> emacs-major-version 27))
(defconst EMACS29+   (> emacs-major-version 28))
(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkeley-unix)))

(defmacro defadvice! (symbol arglist &optional docstring &rest body)
  "Define an advice called SYMBOL and add it to PLACES.

ARGLIST is as in `defun'. WHERE is a keyword as passed to `advice-add', and
PLACE is the function to which to add the advice, like in `advice-add'.
DOCSTRING and BODY are as in `defun'.

\(fn SYMBOL ARGLIST &optional DOCSTRING &rest [WHERE PLACES...] BODY\)"
  (declare (doc-string 3) (indent defun))
  (unless (stringp docstring)
    (push docstring body)
    (setq docstring nil))
  (let (where-alist)
    (while (keywordp (car body))
      (push `(cons ,(pop body) (doom-enlist ,(pop body)))
            where-alist))
    `(progn
       (defun ,symbol ,arglist ,docstring ,@body)
       (dolist (targets (list ,@(nreverse where-alist)))
         (dolist (target (cdr targets))
           (advice-add target (car targets) #',symbol))))))

(defun doom-enlist (exp)
  "Return EXP wrapped in a list, or as-is if already a list."
  (declare (pure t) (side-effect-free t))
  (if (proper-list-p exp) exp (list exp)))

(require 'cl-seq)
(defvar doom-projectile-fd-binary
  (cl-find-if #'executable-find (list "fdfind" "fd"))
  "The filename of the `fd' executable. On some distros it's 'fdfind' (ubuntu,
debian, and derivatives). On most it's 'fd'.")


(defun doom-project-root (&optional dir)
  "Return the project root of DIR (defaults to `default-directory').
Returns nil if not in a project."
  (let ((projectile-project-root
         (unless dir (bound-and-true-p projectile-project-root)))
        projectile-require-project-root)
    (projectile-project-root dir)))

(defmacro pushnew! (place &rest values)
  "Push VALUES sequentially into PLACE, if they aren't already present.
This is a variadic `cl-pushnew'."
  (let ((var (make-symbol "result")))
    `(dolist (,var (list ,@values) (with-no-warnings ,place))
       (cl-pushnew ,var ,place :test #'equal))))

(defun doom-region-active-p ()
  "Return non-nil if selection is active.
Detects evil visual mode as well."
  (declare (side-effect-free t))
  (or (use-region-p)
      (and (bound-and-true-p evil-local-mode)
           (evil-visual-state-p))))

(defun doom-call-process (command &rest args)
  "Execute COMMAND with ARGS synchronously.

Returns (STATUS . OUTPUT) when it is done, where STATUS is the returned error
code of the process and OUTPUT is its stdout output."
  (with-temp-buffer
    (cons (or (apply #'call-process command nil t nil (remq nil args))
              -1)
          (string-trim (buffer-string)))))

(defvar-local doom-inhibit-indent-detection nil
  "A buffer-local flag that indicates whether `dtrt-indent' should try to detect
indentation settings or not. This should be set by editorconfig if it
successfully sets indent_style/indent_size.")

(defvar doom-inhibit-large-file-detection nil
  "If non-nil, inhibit large/long file detection when opening files.")

(defvar doom-large-file-p nil)
(put 'doom-large-file-p 'permanent-local t)

(defvar doom-large-file-size-alist '(("." . 1.0))
  "An alist mapping regexps (like `auto-mode-alist') to filesize thresholds.

If a file is opened and discovered to be larger than the threshold, Doom
performs emergency optimizations to prevent Emacs from hanging, crashing or
becoming unusably slow.

These thresholds are in MB, and is used by `doom--optimize-for-large-files-a'.")

(defvar doom-large-file-excluded-modes
  '(so-long-mode special-mode archive-mode tar-mode jka-compr
    git-commit-mode image-mode doc-view-mode doc-view-mode-maybe
    ebrowse-tree-mode pdf-view-mode tags-table-mode)
  "Major modes that `doom-check-large-file-h' will ignore.")

(defadvice! doom--prepare-for-large-files-a (size _ filename &rest _)
  "Sets `doom-large-file-p' if the file is considered large.

Uses `doom-large-file-size-alist' to determine when a file is too large. When
`doom-large-file-p' is set, other plugins can detect this and reduce their
runtime costs (or disable themselves) to ensure the buffer is as fast as
possible."
  :before #'abort-if-file-too-large
  (and (numberp size)
       (null doom-inhibit-large-file-detection)
       (ignore-errors
         (> size
            (* 1024 1024
               (assoc-default filename doom-large-file-size-alist
                              #'string-match-p))))
       (setq-local doom-large-file-p size)))

(add-hook 'find-file-hook
  (defun doom-optimize-for-large-files-h ()
    "Trigger `so-long-minor-mode' if the file is large."
    (when (and doom-large-file-p buffer-file-name)
      (if (or doom-inhibit-large-file-detection
              (memq major-mode doom-large-file-excluded-modes))
          (kill-local-variable 'doom-large-file-p)
        (when (fboundp 'so-long-minor-mode) ; in case the user disabled it
          (so-long-minor-mode +1))
        (message "Large file detected! Cutting a few corners to improve performance...")))))
