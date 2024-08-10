;;; lang/org/autoload/org-attach.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +org-attach-icon-for (path)
  (char-to-string
   (pcase (downcase (file-name-extension path))
     ((or "jpg" "jpeg" "png" "gif") ?)
     ("pdf" ?)
     ((or "ppt" "pptx") ?)
     ((or "xls" "xlsx") ?)
     ((or "doc" "docx") ?)
     ((or "ogg" "mp3" "wav" "aiff" "flac") ?)
     ((or "mp4" "mov" "avi") ?)
     ((or "zip" "gz" "tar" "7z" "rar") ?)
     (_ ?))))

;;;###autoload
(defun +org/open-gallery-from-attachments ()
  "TODO"
  (interactive)
  (require 'org-attach)
  (if-let (dir (org-attach-dir))
      (pop-to-buffer
       ;; Rather than opening dired *and* image-dired windows, suppress them
       ;; both and open only the image-dired window.
       (save-window-excursion
         (image-dired dir)
         (current-buffer)))
    (user-error "No attachments for this node")))

;;;###autoload
(defun +org/find-file-in-attachments ()
  "Open a file from `org-attach-id-dir'."
  (interactive)
  (doom-project-browse org-attach-id-dir))

;;;###autoload
(defun +org/attach-file-and-insert-link (path)
  "Downloads the file at PATH and insert an org link at point.
PATH (a string) can be an url, a local file path, or a base64 encoded datauri."
  (interactive "sUri/file: ")
  (unless (eq major-mode 'org-mode)
    (user-error "Not in an org buffer"))
  (require 'org-download)
  (condition-case-unless-debug e
      (let ((raw-uri (url-unhex-string path)))
        (cond ((string-match-p "^data:image/png;base64," path)
               (org-download-dnd-base64 path nil))
              ((image-type-from-file-name raw-uri)
               (org-download-image raw-uri))
              ((let ((new-path (expand-file-name (org-download--fullname raw-uri))))
                 ;; Download the file
                 (if (string-match-p (concat "^" (regexp-opt '("http" "https" "nfs" "ftp" "file")) ":/") path)
                     (url-copy-file raw-uri new-path)
                   (copy-file path new-path))
                 ;; insert the link
                 (org-download-insert-link raw-uri new-path)))))
    (error
     (user-error "Failed to attach file: %s" (error-message-string e)))))

;; ;; HACK The docstring for org-attach-use-inheritance claims that "[when enabled]
;; ;; attachment links will look through all parent headings until it finds the
;; ;; linked attachment." This is desirable but incorrect. The first heading with
;; ;; an attachment is found, and if the link doesn't resolve there a user-error is
;; ;; thrown. Second, "running org-attach inside a node will make org-attach
;; ;; operate on the first parent heading it finds with an attachment." I want to
;; ;; be able to add attachments to any heading, not only one where no ancestors
;; ;; have attachments. The code below implements both objectives. It rewrites
;; ;; org-attach-follow, which is only ever called when resolving attachment
;; ;; links (the use case I am modifying here); adds an optional `where' arg to
;; ;; the end of the signatures for org-attach-expand and org-attach-dir, to avoid
;; ;; altering calls anywhere else to these functions; and sets a new
;; ;; namespaced point variable (`my/where') in org-entry-get-with-inheritance.
;; ;; Thus, no other code should be affected by these changes!
;; ;;
;; ;; Call stack:
;; ;; org-open-at-point -> org-link-open -> org-attach-follow -> org-attach-expand ->
;; ;; org-attach-dir -> org-entry-get -> org-entry-get-with-inheritance
;; (defvar my/where nil)

;; ;; TODO This doesn't search the tree recursively. It searches every heading above
;; ;; the one where the link lives.
;; ;;;###autoload
;; (defun my/org-attach-follow (file arg &optional where)
;;   "Open FILE attachment.
;; See `org-open-file' for details about ARG."
;;   (let ((org-attach-use-inheritance t))
;;     (condition-case err
;;         (org-link-open-as-file (org-attach-expand file where) arg)
;;       (user-error
;;        (org-attach-follow file arg (- my/where 1))))
;;     (setq my/where nil)))

;; ;;;###autoload
;; (defun my/org-attach-expand (file &optional where)
;;   "Return the full path to the current entry's attachment file FILE.
;; Basically, this adds the path to the attachment directory."
;;   (expand-file-name file (org-attach-dir nil nil where)))

;; ;;;###autoload
;; (defun my/org-attach-dir (&optional create-if-not-exists-p no-fs-check where)
;;   "Return the directory associated with the current outline node.
;; First check for DIR property, then ID property.
;; `org-attach-use-inheritance' determines whether inherited
;; properties also will be considered.

;; If an ID property is found the default mechanism using that ID
;; will be invoked to access the directory for the current entry.
;; Note that this method returns the directory as declared by ID or
;; DIR even if the directory doesn't exist in the filesystem.

;; If CREATE-IF-NOT-EXISTS-P is non-nil, `org-attach-dir-get-create'
;; is run.  If NO-FS-CHECK is non-nil, the function returns the path
;; to the attachment even if it has not yet been initialized in the
;; filesystem.

;; If no attachment directory can be derived, return nil."
;;   (let (attach-dir id)
;;     (cond
;;      (create-if-not-exists-p
;;       (setq attach-dir (org-attach-dir-get-create)))
;;      ((setq attach-dir (org-entry-get where "DIR" org-attach-use-inheritance))
;;       (org-attach-check-absolute-path attach-dir))
;;      ;; Deprecated and removed from documentation, but still
;;      ;; works. FIXME: Remove after major nr change.
;;      ((setq attach-dir (org-entry-get where "ATTACH_DIR" org-attach-use-inheritance))
;;       (org-attach-check-absolute-path attach-dir))
;;      ((setq id (org-entry-get where "ID" org-attach-use-inheritance))
;;       (org-attach-check-absolute-path nil)
;;       (setq attach-dir (org-attach-dir-from-id id 'try-all))))
;;     (if no-fs-check
;; 	attach-dir
;;       (when (and attach-dir (file-directory-p attach-dir))
;; 	attach-dir))))

;; ;;;###autoload
;; (defun my/org-entry-get-with-inheritance (property &optional literal-nil element)
;;   "Get PROPERTY of entry or content at point, search higher levels if needed.
;; The search will stop at the first ancestor which has the property defined.
;; If the value found is \"nil\", return nil to show that the property
;; should be considered as undefined (this is the meaning of nil here).
;; However, if LITERAL-NIL is set, return the string value \"nil\" instead."
;;   (move-marker org-entry-property-inherited-from nil)
;;   (org-with-wide-buffer
;;    (let (value at-bob-no-heading)
;;      (catch 'exit
;;        (let ((element (or element
;;                           (and (org-element--cache-active-p)
;;                                (org-element-at-point nil 'cached))))
;;              (separator (org--property-get-separator property)))
;;          (if element
;;              (let ((element (org-element-lineage element '(headline org-data inlinetask) 'with-self)))
;;                (while t
;;                  (let* ((v (org--property-local-values property literal-nil element))
;;                         (v (if (listp v) v (list v))))
;;                    (when v
;;                      (setq value
;;                            (concat (mapconcat #'identity (delq nil v) separator)
;;                                    (and value separator)
;;                                    value)))
;;                    (cond
;; 	            ((car v)
;; 	             (move-marker org-entry-property-inherited-from (org-element-property :begin element))
;; 	             (throw 'exit nil))
;; 	            ((org-element-property :parent element)
;;                      (setq element (org-element-property :parent element)))
;; 	            (t
;; 	             (let ((global (org--property-global-or-keyword-value property literal-nil)))
;; 	               (cond ((not global))
;; 		             (value (setq value (concat global separator value)))
;; 		             (t (setq value global))))
;; 	             (throw 'exit nil))))))
;;            (while t
;; 	     (let ((v (org--property-local-values property literal-nil)))
;; 	       (when v
;; 	         (setq value
;; 		       (concat (mapconcat #'identity (delq nil v) separator)
;; 			       (and value separator)
;; 			       value)))
;; 	       (cond
;; 	        ((car v)
;; 	         (org-back-to-heading-or-point-min t)
;; 	         (move-marker org-entry-property-inherited-from (point))
;;                  (setq my/where (point))
;; 	         (throw 'exit nil))
;; 	        ((or (org-up-heading-safe)
;;                      (and (not (bobp))
;;                           (goto-char (point-min))
;;                           nil)
;;                      ;; `org-up-heading-safe' returned nil.  We are at low
;;                      ;; level heading or bob.  If there is headline
;;                      ;; there, do not try to fetch its properties.
;;                      (and (bobp)
;;                           (not at-bob-no-heading)
;;                           (not (org-at-heading-p))
;;                           (setq at-bob-no-heading t))))
;; 	        (t
;; 	         (let ((global (org--property-global-or-keyword-value property literal-nil)))
;; 	           (cond ((not global))
;; 		         (value (setq value (concat global separator value)))
;; 		         (t (setq value global))))
;; 	         (throw 'exit nil))))))))
;;      (if literal-nil value (org-not-nil value)))))
