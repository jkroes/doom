;;; libraries/dendroam.el -*- lexical-binding: t; -*-

;; This code is based on
;; https://github.com/vicrdguez/dendroam/blob/main/dendroam.el

;; For more on how structures like org-roam-node are related to
;; cl-defmethod, see https://nullprogram.com/blog/2018/02/14/ and
;; https://www.orgroam.com/manual.html#Accessing-and-Modifying-Nodes

(require 'org-roam)
(require 'citar)

;; NODE CAPTURE --------------------------------------------------------------------

(setq org-roam-capture-templates
      '(("d" "dendroam" plain "%?"
         :target (file+head
                  "${dendroam-slug}.org" "#+title: ${dendroam-title}")
         :immediate-finish t)))

(cl-defmethod org-roam-node-dendroam-slug ((node org-roam-node))
  "Return the input with non-alphanumeric characters replaced with underscores,
except for periods and dashes."
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
                      (" " . "_")     ; replace spaces
                      ("__*" . "_")   ; remove sequential underscore
                      ("^_*" . "")    ; remove starting underscore
                      ("_*$" . "")    ; remove ending underscore
                      ))
             (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
        (downcase slug)))))

(cl-defmethod org-roam-node-dendroam-title ((node org-roam-node))
  "Node title is the final period-separated component of the dendroam hierarchy."
  (car (last (split-string (org-roam-node-dendroam-hierarchy node) "\\."))))

;; (cl-defmethod org-roam-node-dendroam-hierarchy ((node org-roam-node))
;;   "Node hierarchy is the filename without directory or extension."
;;   (file-name-base (org-roam-node-file node)))

(defun dendroam-find-master-scratch ()
  (interactive)
  (org-roam-capture-
   :node (org-roam-node-create :title (completing-read "Title: " nil))
   :templates '(("s" "scratch" entry "* %<%Y%m%d%H%M%S>.${title} %?"
                 :target (file+head "scratch.org" "#+title: scratch")
                 :jump-to-captured t
                 :immediate-finish t
                 :empty-lines 2))
   :props '(:finalize find-file)))

(defun dendroam-find-scratch ()
  (interactive)
  (dendroam--find
   "scratch"
   '(("s" "scratch" entry "* %<%Y%m%d%H%M%S>.${title} %?"
      :target (file+head "${dendroam-hierarchy}.scratch.org" "#+title: scratch")
      :immediate-finish t))))

(defun dendroam-find-meeting ()
  (interactive)
  (dendroam--find
   (format-time-string "%Y%m%d")
   '(("m" "meeting" plain "%?"
      :target (file+head "${dendroam-hierarchy}.%<%Y%m%d>.org" "#+title: ${title}")
      :immediate-finish t))))

;; NOTE For best results, input should be formatted like the candidates
;; displayed according to org-roam-node-display-template. Since we are
;; excluding citar nodes, we can use the function for formatting non-citar
;; notes.
(defun dendroam--find (suffix template)
  (let* ((parent (org-roam-node-at-point))
         (input (if parent (org-roam-node-dendroam-full-hierarchy parent)))
         ;; Find a node located outside of `citar-org-roam-subdir'. If the
         ;; current file is a node, use it's hierarchy as initial input
         (node (org-roam-node-read
                input
                (lambda (f)
                  (not (string=
                        (directory-file-name (file-name-directory (org-roam-node-file f)))
                        (concat-path org-roam-directory citar-org-roam-subdir))))
                nil t)))
    (org-roam-capture-
     :node (org-roam-node-create
            :title (completing-read "Title: " nil)
            :file (concat-path
                   org-roam-directory
                   (concat (org-roam-node-dendroam-hierarchy node) "." suffix ".org")))
     :templates template
     :props '(:finalize find-file))))

;; NODE TREE-STYLE NAVIGATION --------------------------------------------------------------------

;; NOTE Compare code to org-roam-node-find
(defun dendroam-find-parent ()
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
         :props '(:finalize find-file))
        ))))

(defun dendroam-find-siblings ()
  "Find sibling nodes at the same hierarchical level as input, excluding the current node."
  (interactive)
  (dendroam--find-siblings (or (and (eq major-mode 'org-mode) (org-roam-node-at-point)) "")))

(cl-defmethod dendroam--find-siblings ((node org-roam-node))
  (org-roam-node-find nil
                      (org-roam-node-dendroam-hierarchy-no-title node)
                      ;; TODO Why does comparing nodes fail when comparing
                      ;; files works?
                      ;;(lambda (f) (not (equal node f)))))
                      (lambda (f) (not (string= (org-roam-node-file node)
                                                (org-roam-node-file f))))))

(cl-defmethod org-roam-node-dendroam-hierarchy-no-title ((node org-roam-node))
  "Node hierarchy, minus the last period-separated component."
  (dendroam-up-hierarchy (org-roam-node-dendroam-hierarchy node)))

(defun dendroam-up-hierarchy (hierarchy)
  (string-join (butlast (split-string hierarchy "\\.")) "."))

(cl-defmethod dendroam--find-siblings ((str string))
  (org-roam-node-find nil str))

(defun dendroam-find-children ()
  "Find children nodes one hierarchical level beneath input, excluding the current node."
  (interactive)
  (dendroam--find-children (or (and (eq major-mode 'org-mode) (org-roam-node-at-point)) "")))

(cl-defmethod dendroam--find-children ((node org-roam-node))
  (org-roam-node-find nil
                      (org-roam-node-dendroam-hierarchy node)
                      (lambda (f) (not (string= (org-roam-node-file node)
                                                (org-roam-node-file f))))))

(cl-defmethod dendroam--find-children ((str string))
  (org-roam-node-find nil str))

;;; NODE REFACTOR --------------------------------------------------------------------

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

;; TODO Keep testing this. I would prefer to do simple file matching in case
;; the database is out of sync. I pulled this from the original dendroam github
;; repository
(defun dendroam-sibling-files (hierarchy)
  "Gets all the nodes that share the same HIERARCHY"
  (mapcar #'car (org-roam-db-query [:select [file]
                                    :from nodes
                                    :where (like file $r1)]
                                   (concat "%" hierarchy "%"))))

;; TODO Add a warning when the node to rename is a parent node, in case we want
;; to use refactor instead.
(defun dendroam-rename-note ()
  "Rename current note only (i.e., preserve hierarchy)."
  (interactive)
  (dendroam--rename-note (org-roam-node-at-point)))

(cl-defmethod dendroam--rename-note ((node org-roam-node))
  (unless (or (dendroam--meeting-note-p node)
              (dendroam--scratch-note-p node)
              (dendroam--citar-note-p node))
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
      (org-roam-set-keyword "title" new-title)
      (save-buffer))))

(cl-defmethod dendroam--scratch-note-p ((node org-roam-node))
  "Return t if org-roam note is a dendroam datetime note"
  (string= "scratch" (org-roam-node-title node)))

(cl-defmethod dendroam--meeting-note-p ((node org-roam-node))
  "Return t if org-roam note is a dendroam meeting note"
  (require 'dash)
  (let* ((title (org-roam-node-dendroam-title node))
         (dmy (butlast (nthcdr 3 (parse-time-string title)) 3)))
    (not (-any 'null dmy))))

(cl-defmethod dendroam--citar-note-p ((node org-roam-node))
  (string=
   (directory-file-name (file-name-directory (org-roam-node-file node)))
   (concat-path org-roam-directory citar-org-roam-subdir)))

;;; NODE DISPLAY --------------------------------------------------------------------

;; https://github.com/org-roam/org-roam/issues/2066
;; BUG org-roam candidates are too big. Completing them mvoes the cursor down
;; into the candidates displayed by vertico. Doom Emacs fix does not work,
;; so this advice could be used to disable it:
;; (advice-remove '+org--roam-fix-completion-width-for-vertico-a #'org-roam-node-read--to-candidate)
;; The only current fix is to omit length specs below.
(setq org-roam-node-display-template "${display-text}")
;; NOTE Zero-length tags can still be searched; they are just invisible
;; (setq org-roam-node-display-template
;;       (format "${display-text:*} %s"
;;               (propertize "${doom-tags2:5}" 'face 'org-tag)))

;; NOTE zero-length components can still be searched
(setq org-roam-node-display-template
      (format "${display-text:*} %s"
              (propertize "${doom-tags2:5}" 'face 'org-tag)))

(cl-defmethod org-roam-node-display-text ((node org-roam-node))
  (if (string= (concat-path org-roam-directory citar-org-roam-subdir)
               (directory-file-name (file-name-directory (org-roam-node-file node))))
      ;; citar reference notes
      (org-roam-node-title node)
    ;; other org-roam notes
    (org-roam-node-dendroam-full-hierarchy node)))

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


;;; MINIBUFFER COMPLETION ----------------------------------------------------

;; HACK When calling org-roam-node-find or any other function that calls
;; org-roam-node-read, `DEL' deletes a character, or if the preceding character
;; is a period and not the first component deletes to the nearest period. In
;; other words, it does like `vertico-directory-delete-char' but with `.'
;; instead of `/'.
;; NOTE You can always just use vertico-directory-delete-word if you always
;; want to delete back to `.'
;; (defun dendroam-up (&optional n)
;;   "Delete N directories before point."
;;   (interactive "p")
;;   (when (and (> (point) (minibuffer-prompt-end))
;;              (eq (char-before) ?.)
;;              (eq 'org-roam-node (vertico--metadata-get 'category)))
;;     (let ((path (buffer-substring (minibuffer-prompt-end) (point))) found)
;;       (dotimes (_ (or n 1) found)
;;         (save-excursion
;;           (let ((end (point)))
;;             (goto-char (1- end))
;;             (when (search-backward "." (minibuffer-prompt-end) t)
;;               (delete-region (1+ (point)) end)
;;               (setq found t))))))))
;;
;; (defun delete-dendroam (&optional n)
;;   "Delete N directories or chars before point."
;;   (interactive "p")
;;   (unless (dendroam-up n)
;;     (backward-delete-char n)))
;;
;; (defadvice! my/delete--dendroam (fn &rest args)
;;   :around #'org-roam-node-read
;;   (cl-letf (((symbol-function  'vertico-directory-delete-char) #'delete-dendroam))
;;     (apply fn args)))



;;; MISCELLANEOUS ------------------------------------------------------------------------------

;; TODO Test this to make sure it works with dendroam, keeping the title and
;; filename properly synced
;; NOTE org-roam v2 got rid of features to auto-update link descriptions when
;; a file's title changes. See `git log -G org-roam--setup-title-auto-update'
;; and https://org-roam.discourse.group/t/org-roam-v2-rename-file-or-title/1691/11
;; (defun azr/org-roam-modify-title ()
;;   "Modify title of current node and update the description portion of all ID
;; links to this node (i.e., of all backlinks) to the new title"
;;   (interactive)
;;   (unless (org-roam-buffer-p) (error "Not in an org-roam buffer."))
;;   ;; Save all buffers without prompt.
;;   (save-some-buffers t)
;;   ;; Update the title
;;   (let* ((old-title (org-roam-get-keyword "title"))
;;          (ID (org-entry-get (point) "ID"))
;;          (new-title (read-string "Enter new title: " old-title)))
;;     (org-roam-set-keyword "title" new-title)
;;     (save-buffer)
;;     ;; Update the buffer name and filename
;;     (let* ((new-slug (org-roam-node-slug (org-roam-node-at-point)))
;;            (new-file-name (replace-regexp-in-string "-.*\\.org" (format "-%s.org" new-slug) (buffer-file-name)))
;;            (new-buffer-name (file-name-nondirectory new-file-name)))
;;       (rename-buffer new-buffer-name)
;;       (rename-file (buffer-file-name) new-file-name 1)
;;       ;; OG Author: I don't know why this last command is necessary. Getting it from here:
;;       ;; https://stackoverflow.com/a/384346/2422698
;;       (set-visited-file-name new-file-name))
;;     (save-buffer)
;;     ;; Rename backlinks in the rest of the Org-roam database.
;;     (let* ((search (format "[[id:%s][%s]]" ID old-title))
;;            (replace (format "[[id:%s][%s]]" ID new-title))
;;            (rg-command (format "rg -t org -lF '%s' '%s'" search org-roam-directory))
;;            (file-list (split-string (shell-command-to-string rg-command))))
;;       (dolist (file file-list)
;;         (let ((file-open (get-file-buffer file)))
;; 	  (find-file file)
;;           (beginning-of-buffer)
;;           (while (search-forward search nil t)
;;             (replace-match replace))
;;           (save-buffer)
;;           (unless file-open
;;             (kill-buffer)))))))

(provide 'dendroam)
