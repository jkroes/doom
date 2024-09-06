;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun my/ediff-mode-bindings ()
  "Bindings to copy the contents of one or both diff buffers when
using `my/ediff-doom-private-module', usually for pasting into
some other buffer. Bsaed on `ediff-util'."
  (map! :map ediff-mode-map
        "ca" #'ediff-copy-A
        "cb" #'ediff-copy-B))

;;;###autoload
(defun ediff-copy-A (arg)
  (interactive "P")
  (my/ediff-diff-to-diff arg "a"))

;;;###autoload
(defun ediff-copy-B (arg)
  (interactive "P")
  (my/ediff-diff-to-diff arg "b"))

(defun my/ediff-diff-to-diff (arg &optional keys)
  (interactive "P")
  (ediff-barf-if-not-control-buffer)
  (or keys (setq keys (this-command-keys)))
  (if (eq arg '-) (setq arg -1)) ; translate neg arg to -1
  (if (numberp arg) (ediff-jump-to-difference arg))

  (let* ((char1 (aref keys 0)))
    (my/ediff-copy-diff ediff-current-difference
                        (ediff-char-to-buftype char1))))

(defun my/ediff-copy-diff (n from-buf-type)
  (let* ((reg-to-copy (ediff-get-region-contents n from-buf-type ediff-control-buffer)))
    (kill-new reg-to-copy)))

;;;###autoload
(defun doom/copy-module-to-private (category module)
  "Copy the Doom module corresponding to the strings category and module as a
private module."
  (interactive
   (mapcar #'intern
           (split-string
            (completing-read "Copy module:"
                             (doom-private-modules-list)
                             nil t)
            " " t)))
  (let* ((path (doom-module-locate-path category module))
         (newpath (replace-regexp-in-string (expand-file-name (cadr doom-module-load-path))
                                            (car doom-module-load-path) path)))
    (copy-directory path newpath nil t t)))

(autoload #'ediff-read-file-name "ediff")
(autoload #'ediff-files-internal "ediff")
;;;###autoload
(defun my/ediff-doom-private-module (file-A file-B &optional skip-prompt)
  "Prompt for a private Doom module, then a module file with differences
from the corresponding non-private file. If the current buffer is
a private module file, use its module and filename to select the
default candidate for the two prompts."
  (interactive
   (let* ((mod (current-doom-module-string))
          (file-B
           ;; Per https://howardism.org/Technical/Emacs/alt-completing-read.html,
           ;; completing-read accepts an alist but only uses the car of each alist as
           ;; candidates; however, we can access the cadr within the PREDICATE func
           (completing-read
            "Private module file: "
            (module-files-with-diffs
             ;; TODO This returns nil if the module is not activated in user's init.el
             (apply #'doom-module-get
                    (append
                     (mapcar #'intern
                             (split-string
                              ;; Private modules with default of the current module
                              (completing-read
                               "Private module: "
                               (doom-private-modules-list
                                (directory-files
                                 (car doom-module-load-path) t))
                               nil t nil nil mod)))
                     (list :path))))
            ;; If DEF is non-nil but not one of the candidates, the first
            ;; candidate will not be selected. If `mod' is nil,
            ;; then `buffer-file-name' will not be a candidate.
            nil t nil nil (when mod buffer-file-name)))
          (file-A (replace-regexp-in-string
                   (car doom-module-load-path)
                   (expand-file-name (cadr doom-module-load-path))
                   file-B)))
     (list file-A file-B)))
  (ediff-files-internal file-A file-B nil nil 'ediff-files))

(defun current-doom-module-string ()
  (when-let (mod (current-doom-module))
    (format "%s %s" (car mod) (cdr mod))))

(defun current-doom-module ()
  (when buffer-file-name
    (when-let (mod (doom-module-from-path buffer-file-name))
      (unless (memq (car mod) '(:core :user))
        mod))))

(defun doom-private-modules-list (&optional paths-or-all)
  (cl-loop for (cat . mod) in
           (doom-module-list paths-or-all)
           for format = (format "%s %s" cat mod)
           if mod ; Exclude (:core) and (:user)
           collect format))

(defun module-files-with-diffs (module-path)
  (-filter #'diff-file-between-modules
           (directory-files-recursively module-path "")))

(defun diff-file-between-modules (private-file)
  (let* ((private-file (expand-file-name private-file))
         (doom-file (replace-regexp-in-string
                     (car doom-module-load-path)
                     (expand-file-name (cadr doom-module-load-path))
                     private-file)))
    (diff2 doom-file private-file)))

(defun diff2 (old new &optional switches)
  (interactive
   (let* ((newf (if (and buffer-file-name (file-exists-p buffer-file-name))
                    (read-file-name
                     (concat "Diff new file (default "
                             (file-name-nondirectory buffer-file-name) "): ")
                     nil buffer-file-name t)
                  (read-file-name "Diff new file: " nil nil t)))
          (oldf (file-newest-backup newf)))
     (setq oldf (if (and oldf (file-exists-p oldf))
                    (read-file-name
                     (concat "Diff original file (default "
                             (file-name-nondirectory oldf) "): ")
                     (file-name-directory oldf) oldf t)
                  (read-file-name "Diff original file: "
                                  (file-name-directory newf) nil t)))
     (list oldf newf (diff-switches))))
  (diff-no-select2 old new switches))

(autoload #'diff-check-labels "diff")
(defun diff-no-select2 (old new &optional switches)
  (unless (bufferp new) (setq new (expand-file-name new)))
  (unless (bufferp old) (setq old (expand-file-name old)))
  (or switches (setq switches diff-switches)) ; If not specified, use default.
  (unless (listp switches) (setq switches (list switches)))
  (diff-check-labels)
  (let* ((old-alt (diff-file-local-copy old))
         (new-alt (diff-file-local-copy new))
         (command
          (mapconcat #'identity
                     `(,diff-command
                       ;; Use explicitly specified switches
                       ,@switches
                       ,@(mapcar #'shell-quote-argument
                                 (nconc
                                  (and (or old-alt new-alt)
                                       (eq diff-use-labels t)
                                       (list "--label"
                                             (if (stringp old) old
                                               (prin1-to-string old))
                                             "--label"
                                             (if (stringp new) new
                                               (prin1-to-string new))))
                                  (list (or old-alt old)
                                        (or new-alt new)))))
                     " ")))
    (diff-sentinel2
     (call-process shell-file-name nil nil nil
                   shell-command-switch command)
     old-alt new-alt)))

(defun diff-sentinel2 (code &optional old-temp-file new-temp-file)
  (if old-temp-file (delete-file old-temp-file))
  (if new-temp-file (delete-file new-temp-file))
  (cond ((equal 0 code) nil)
        ((equal 1 code) t)
        ;; TODO Handle errors reported by the diff binary
        ((equal 2 code) nil)))
