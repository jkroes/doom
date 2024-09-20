;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun describe-face-under-hl-line ()
  "The `hl-line' face obscures underlying faces and must be
disabled to get the underlying face. When mixing fixed- and
variable-pitch fonts within a buffer, this function will not
correctly display variable-pitch fonts. Instead use
`describe-char'."
  (interactive)
  (if hl-line-mode
      (unwind-protect
          (progn
            (hl-line-mode -1)
            (call-interactively #'describe-face))
        (hl-line-mode))
    (call-interactively #'describe-face)))

(defconst embark--verbose-indicator-buffer " *Embark Actions*")
;;;###autoload
(defun jkroes/embark-actions-buffer-visible ()
  (get-buffer-window embark--verbose-indicator-buffer 'visible))

;;;###autoload
(defun comment-pad-with-dashes-right ()
  "Fill a three-semicolon lisp comment with dash characters from
the end of the line to `fill-column'."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (save-match-data (looking-at (string-join `("^" ,comment-start))))
      (end-of-line)
      (just-one-space)
      (insert (make-string (- fill-column (current-column)) ?-)))))

;;;###autoload
(defun quit-window-kill-buffer ()
  (interactive)
  (quit-window t))

;;;###autoload
(defun file-bookmark ()
  (interactive)
  (my/bookmark-set
   (car (find-file-read-args
         "Find file: "
         (confirm-nonexistent-file-or-buffer)))))

;; TODO Modify this so that it works without visiting file via find-file
;;;###autoload
(defun my/bookmark-set (file)
  "For use with embark-file-map. Bookmark the selected file and
prompt for a name, using filename as default input"
  (let ((curbuf (current-buffer)))
    ;; NOTE bookmark-set uses the current buffer. find-file opens directories
    ;; in dired. dired with e.g. /mnt/c/ throws permission errors that prevent
    ;; a bookmark's creation, but that's fine.
    (find-file file)
    (bookmark-set (read-from-minibuffer
                   "Bookmark name: "
                   (file-name-nondirectory
                    (if (f-dir-p file)
                        (directory-file-name file)
                      file))))
    (kill-buffer (current-buffer))
    (switch-to-buffer curbuf)))


;;;###autoload
(defun my/helpful--version-info (sym)
  "If SYM has version information, format and return it.
Return nil otherwise."
  (when (symbolp sym)
    (let ((package-version
           (get sym 'custom-package-version))
          (emacs-version
           (get sym 'custom-version)))
      (cond
       ((listp package-version)
        (format
         "This variable was added, or its default value changed, in %s version %s."
         (car package-version)
         (cdr package-version)))
       (emacs-version
        (format
         "This variable was added, or its default value changed, in Emacs %s."
         emacs-version))))))

;;;###autoload
(defun corfu-popupinfo-scroll-up-5 ()
  (interactive)
  (corfu-popupinfo-scroll-up 5))

;;;###autoload
(defun corfu-popupinfo-scroll-down-5 ()
  (interactive)
  (corfu-popupinfo-scroll-down 5))

(defvar browse-url-generic-program)
(defvar browse-url-generic-args)
;;;###autoload
(defun open-in-windows (path &rest _)
  (let ((browse-url-generic-program "/mnt/c/Windows/System32/cmd.exe")
        (browse-url-generic-args '("/c" "start" "")))
    (browse-url-generic
     (substring
      (shell-command-to-string
       (format "wslpath -w '%s'" path))
      0 -1))))

(defvar corfu--frame)
;;;###autoload
(defun jkroes/corfu-visible-p ()
  (and (frame-live-p corfu--frame) (frame-visible-p corfu--frame)))

;; NOTE Relative line numbers are most useful for org body text, while visual
;; is most useful for collapsed org headings, assuming
;; `evil-respect-visual-line-mode' was set to nil before evil loaded. Below we
;; default to visual instead of relative lines. There does not seem to be an
;; easy solution without being able to set `evil-respect-visual-line-mode'
;; independently for different modes.

;; HACK `doom/toggle-line-numbers' toggles absolute line numbers the first time
;; it is invoked, when display-line-numbers-type is nil or relative. It sets
;; display-line-numbers, which is a buffer local variable, but tracks state
;; between calls using a global variable. This makes the effect of toggling in
;; a given buffer unpredictable when you have toggled other buffers previously.

;; TODO Add more commands below as you discover more commands that open links
;; within org-mode
(defvar jkroes/org-open-file-link-commands
  '(+org/dwim-at-point
    org-open-at-mouse
    org-open-at-point
    org-open-at-point-global))

;;;###autoload
(defun insert-org-entity ()
  "A dumb replacement for counsel-org-entity. See `org-pretty-entities'."
  (interactive)
  (let* ((str (completing-read
               "Entity: "
               (cl-loop for element in (append org-entities org-entities-user)
                        unless (or (stringp element)
                                   (string-prefix-p "_" (car element))) ; some hspace elements
                        collect (cons
                                 (format "%s | %s"
                                         (cl-first element)    ; name
                                         (cl-seventh element)) ; utf-8
                                 element))))
         (latex (concat "\\" (nth 0 (split-string str "|" t " ")))))
    (insert latex)))
