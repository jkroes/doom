;;; lang/org/autoload/org-link.el -*- lexical-binding: t; -*-

(defun +org--relative-path (path root)
  (if (and buffer-file-name (file-in-directory-p buffer-file-name root))
      (file-relative-name path)
    path))

(defun +org--read-link-path (key dir &optional fn)
  (let ((file (funcall (or fn #'read-file-name) (format "%s: " (capitalize key)) dir)))
    (format "%s:%s" key (file-relative-name file dir))))

;;;###autoload
(defun +org-read-link-description-at-point (&optional default context)
  "TODO"
  (if (and (stringp default) (not (string-empty-p default)))
      (string-trim default)
    (if-let* ((context (or context (org-element-context)))
              (context (org-element-lineage context '(link) t))
              (beg (org-element-property :contents-begin context))
              (end (org-element-property :contents-end context)))
        (unless (= beg end)
          (replace-regexp-in-string
           "[ \n]+" " " (string-trim (buffer-substring-no-properties beg end)))))))

;;;###autoload
(defun +org-define-basic-link (key dir-var &rest plist)
  "Define a link with some basic completion & fontification.

KEY is the name of the link type. DIR-VAR is the directory variable to resolve
links relative to. PLIST is passed to `org-link-set-parameters' verbatim.

Links defined with this will be rendered in the `error' face if the file doesn't
exist, and `org-link' otherwise."
  (declare (indent 2))
  (let ((requires (plist-get plist :requires))
        (dir-fn (if (functionp dir-var)
                    dir-var
                  (lambda () (symbol-value dir-var)))))
    (apply #'org-link-set-parameters
           key
           :complete (lambda ()
                       (if requires (mapc #'require (doom-enlist requires)))
                       (+org--relative-path (+org--read-link-path key (funcall dir-fn))
                                            (funcall dir-fn)))
           :follow   (lambda (link)
                       (org-link-open-as-file (expand-file-name link (funcall dir-fn)) nil))
           :face     (lambda (link)
                       (let* ((path (expand-file-name link (funcall dir-fn)))
                              (option-index (string-match-p "::\\(.*\\)\\'" path))
                              (file-name (substring path 0 option-index)))
                         (if (file-exists-p file-name)
                             'org-link
                           'error)))
           (plist-put plist :requires nil))))


;;
;;; Image data functions (for custom inline images)

;;;###autoload
(defun +org-image-file-data-fn (protocol link _description)
  "Intepret LINK as an image file path and return its data."
  (setq
   link (expand-file-name
         link (pcase protocol
                ("download"
                 (or (if (require 'org-download nil t) org-download-image-dir)
                     (if (require 'org-attach)         org-attach-id-dir)
                     default-directory))
                ("attachment"
                 (require 'org-attach)
                 org-attach-id-dir)
                (_ default-directory))))
  (when (and (file-exists-p link)
             (image-type-from-file-name link))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (setq buffer-file-coding-system 'binary)
      (insert-file-contents-literally link)
      (buffer-substring-no-properties (point-min) (point-max)))))

;;;###autoload
(defun +org-inline-image-data-fn (_protocol link _description)
  "Interpret LINK as base64-encoded image data."
  (base64-decode-string link))

;;;###autoload
(defun +org-http-image-data-fn (protocol link _description)
  "Interpret LINK as an URL to an image file."
  (when (and (image-type-from-file-name link)
             (not (eq org-display-remote-inline-images 'skip)))
    (if-let (buf (url-retrieve-synchronously (concat protocol ":" link)))
        (with-current-buffer buf
          (goto-char (point-min))
          (re-search-forward "\r?\n\r?\n" nil t)
          (buffer-substring-no-properties (point) (point-max)))
      (message "Download of image \"%s\" failed" link)
      nil)))

(defvar +org--gif-timers nil)
;;;###autoload
(defun +org-play-gif-at-point-h ()
  "Play the gif at point, while the cursor remains there (looping)."
  (dolist (timer +org--gif-timers (setq +org--gif-timers nil))
    (when (timerp (cdr timer))
      (cancel-timer (cdr timer)))
    (image-animate (car timer) nil 0))
  (when-let* ((ov (cl-find-if
                   (lambda (it) (overlay-get it 'org-image-overlay))
                   (overlays-at (point))))
              (dov (overlay-get ov 'display))
              (pt  (point)))
    (when (image-animated-p dov)
      (push (cons
             dov (run-with-idle-timer
                  0.5 nil
                  (lambda (dov)
                    (when (equal
                           ov (cl-find-if
                               (lambda (it) (overlay-get it 'org-image-overlay))
                               (overlays-at (point))))
                      (message "playing gif")
                      (image-animate dov nil t)))
                  dov))
            +org--gif-timers))))

;;;###autoload
(defun +org-play-all-gifs-h ()
  "Continuously play all gifs in the visible buffer."
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when-let* (((overlay-get ov 'org-image-overlay))
                (dov (overlay-get ov 'display))
                ((image-animated-p dov))
                (w (selected-window)))
      (while-no-input
        (run-with-idle-timer
         0.3 nil
         (lambda (dov)
           (when (pos-visible-in-window-p (overlay-start ov) w nil)
             (unless (plist-get (cdr dov) :animate-buffer)
               (image-animate dov))))
         dov)))))


;;
;;; Commands

;;;###autoload
(defun +org/remove-link ()
  "Unlink the text at point."
  (interactive)
  (unless (org-in-regexp org-link-bracket-re 1)
    (user-error "No link at point"))
  (save-excursion
    (let ((label (if (match-end 2)
                     (match-string-no-properties 2)
                   (org-link-unescape (match-string-no-properties 1)))))
      (delete-region (match-beginning 0) (match-end 0))
      (insert label))))

;;;###autoload
(defun +org/play-gif-at-point ()
  "TODO"
  (interactive)
  (unless (eq 'org-mode major-mode)
    (user-error "Not in org-mode"))
  (or (+org-play-gif-at-point-h)
      (user-error "No gif at point")))


;;
;;; Org-link parameters

;;; doom-module:
(defun +org-link--doom-module--read-link (link)
  (cl-destructuring-bind (category &optional module flag)
      (let ((desc (+org-read-link-description-at-point link)))
        (if (string-prefix-p "+" (string-trim-left desc))
            (list nil nil (intern desc))
          (mapcar #'intern (split-string desc " " nil))))
    (list :category category
          :module module
          :flag flag)))

;;;###autoload
(defun +org-link--doom-module-follow-fn (link)
  (cl-destructuring-bind (&key category module flag)
      (+org-link--doom-module--read-link link)
    (when category
      (let ((doom-modules-dirs (list doom-modules-dir)))
        (if-let* ((path (doom-module-locate-path category module))
                  (path (or (car (doom-glob path "README.org"))
                            path)))
            (find-file path)
          (user-error "Can't find Doom module '%s'" link))))
    (when flag
      (goto-char (point-min))
      (and (re-search-forward "^\\*+ \\(?:TODO \\)?Module Flags")
           (re-search-forward (format "^\\s-*- %s :: "
                                      (regexp-quote (symbol-name flag)))
                              (save-excursion (org-get-next-sibling)
                                              (point)))
           (recenter)))))

;;;###autoload
(defun +org-link--doom-module-face-fn (link)
  (cl-destructuring-bind (&key category module flag)
      (+org-link--doom-module--read-link link)
    (if (doom-module-locate-path category module)
        `(:inherit org-priority
          :weight bold)
      'error)))


;;; doom-package:
;;;###autoload
(defun +org-link--doom-package-follow-fn (link)
  "TODO"
  (doom/describe-package
   (intern-soft
    (+org-read-link-description-at-point link))))


;;; kbd:

(defun +org--describe-kbd (keystr)
  (dolist (key `(("<leader>" . ,doom-leader-key)
                 ("<localleader>" . ,doom-localleader-key)
                 ("<prefix>" . ,(if (bound-and-true-p evil-mode)
                                    (concat doom-leader-key " u")
                                  "C-u"))
                 ("<help>" . ,(if (bound-and-true-p evil-mode)
                                  (concat doom-leader-key " h")
                                "C-h"))
                 ("\\<M-" . "alt-")
                 ("\\<S-" . "shift-")
                 ("\\<s-" . "super-")
                 ("\\<C-" . "ctrl-")))
    (setq keystr
          (replace-regexp-in-string (car key) (cdr key)
                                    keystr t t)))
  keystr)

;;;###autoload
(defun +org-read-kbd-at-point (&optional default context)
  "TODO"
  (+org--describe-kbd
   (+org-read-link-description-at-point default context)))


;;;###autoload
(defun org-store-link-to-filepath (arg)
  "I use this to grab the filepath of org files without context about the line
where this is called in the link and without an ID (being created). Also grabs
other files without context."
  (interactive "P")
  (let ((org-link-context-for-files nil)
        major-mode)
    ;; No way to store a link to an org-mode file without an ID, either
    ;; preexisting or created anew. We only want the filepath in the link, which
    ;; we can get if we mask the `major-mode' from `derived-mode-p' within
    ;; `org-store-link'.
    (and (derived-mode-p 'org-mode)
         (setq major-mode 'text-mode))
    ;; Need to call with `interactive?' set to `t' to store link for
    ;; `org-insert-link'
    (org-store-link nil t))
  ;; Without this, the description portion of the newly created element of
  ;; org-stored-links will be the same as the filepath and avoids the
  ;; org-insert-link prompt to enter or accept the description text. Instead,
  ;; the link is inserted without a description. (Some users may prefer this.)
  ;; Any non-nil description value that doesn't match the link portion will pull
  ;; up the prompt with the value as a suggestion.
  (let ((desc (cdr (nth 0 org-stored-links))))
    (setcar desc (if (equal arg '(4))
                     nil
                   (file-name-nondirectory (car desc))))))

;;;###autoload
(defun org-compress-link (arg)
  "Convert URL to a compressed org link. Useful for shortening links you don't
want to add a descrption to"
  (interactive "P")
  (let ((url (thing-at-point 'url))
    (bounds (bounds-of-thing-at-point 'url)))
    (kill-region (car bounds) (cdr bounds))
    (insert (format "[[%s][%s]]" url (truncate-string-to-width url (if arg (prefix-numeric-value arg) 40) nil nil "...")))))
