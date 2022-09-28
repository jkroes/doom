;;; tools/biblio/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun citar-org-format-note-no-bib (key entry)
  "`citar-org-format-note-default' without
#+print_bibliography"
  (let* ((template (citar--get-template 'note))
         (note-meta (when template
                      (citar-format--entry template entry)))
         (filepath (expand-file-name
                    (concat key ".org")
                    (car citar-notes-paths)))
         (buffer (find-file filepath)))
    (with-current-buffer buffer
      ;; This just overrides other template insertion.
      (erase-buffer)
      (citar-org-roam-make-preamble key)
      (insert "#+title: ")
      (when template (insert note-meta))
      (insert "\n\n")
      (when (fboundp 'evil-insert)
        (evil-insert 1)))))

;;;###autoload
(defun aj/citar-org-key-at-point ()
  "Return key at point for org-cite citation-reference or citekey."
  (or (citar-org-key-at-point)
      (when (org-in-regexp org-element-citation-key-re)
        (cons (substring (match-string 0) 1)
              (cons (match-beginning 0)
                    (match-end 0))))))

;; citar-open first prompts for a key, then for a resource (file, URL, or note).
;; Here we skip the key-prompt by using the key stored in ROAM_REFS of the
;; current note
;;;###autoload
(defun org-roam-open-refs ()
  "Open resources associated with citation key, or open URL, from ROAM_REFS
of current note"
  (interactive)
  (save-excursion
    (goto-char (org-roam-node-point (org-roam-node-at-point 'assert)))
    (when-let* ((p (org-entry-get (point) "ROAM_REFS"))
                (refs (when p (split-string-and-unquote p)))
                (user-error "No ROAM_REFS found"))
      ;; Open ref citation keys
      (when-let ((oc-cites
                  (seq-map
                   (lambda (ref) (substring ref 1))
                   (seq-filter (apply-partially #'string-prefix-p "@") refs))))
        (citar-open-from-note oc-cites))
      ;; Open ref URLs
      (dolist (ref refs)
        (unless (string-prefix-p "@" ref)
          (browse-url ref))))))

(defun citar-open-from-note (keys)
  "Like citar-open but excludes notes from candidates."
  (interactive (list (citar-select-refs)))
  (if-let ((selected (let* ((actions (bound-and-true-p embark-default-action-overrides))
                            (embark-default-action-overrides `((t . ,#'citar--open-resource) . ,actions)))
                       (citar--select-resource keys :files t :links t
                                               :always-prompt citar-open-prompt))))
      (citar--open-resource (cdr selected) (car selected))
    (error "No associated resources: %s" keys)))

;;;###autoload
(defun open-in-zotero (file)
  "Open file resources in Zotero PDF viewer."
  (string-match ".*/storage/\\(.*\\)/.*\\.pdf" file)
  (browse-url
   ;; NOTE You can also use select instead of open-pdf to see the
   ;; attachment item in the item pane
   (replace-match "zotero://open-pdf/library/items/\\1" nil nil file)))

;;;###autoload
(defun citar-file--parser-default-wsl (file-field)
  "Split FILE-FIELD by `;'."
  (mapcar
   #'wslify-bib-path
   (seq-remove
    #'string-empty-p
    (mapcar
     #'string-trim
     (citar-file--split-escaped-string file-field ?\;)))))

(defun wslify-bib-path (file)
  "For WSL, convert paths assumed to be Windows files to WSL paths. Otherwise,
return the path"
  (if (eq system-type 'gnu/linux)
      (substring
       (shell-command-to-string
        (format
         "wslpath '%s'"
         (replace-regexp-in-string
          "\\\\\\\\"
          "/"
          (replace-regexp-in-string "\\\\:" ":" file))))
       0 -1)
    file))
