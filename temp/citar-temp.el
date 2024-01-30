;;; citar-temp.el -*- lexical-binding: t; -*-

;; TODO :NOTER_DOCUMENT: contains the absolute path to file. A function needs
;; to be created to convert the parent path to allow for Zotero libraries to
;; be moved around without breaking notes. Or org-noter needs to be overhauled
;; to work with relative paths. As far as I know, it does not. This latter
;; solution would be way more work.

;; TODO If you want org-noter to open the citar note from within a PDF, the note
;; needs the same name as the file. Either you need to name the note after the
;; file, or you need to use Zotfile to rename PDF attachments after the
;; citation key, which is what citar uses to name notes.

;; CSL JSON settings
;; (setq org-cite-csl-styles-dir "~/Zotero/styles"
;;       ;; TODO: citar-library-paths allows you to omit the file field from
;;       ;; bib entries. as long as you name files after the citation key.
;;       ;; This is useful for formats like Zotero BetterBibTeX's version of CSL
;;       ;; JSON. Unfortunately, this is slow with Zotero's default storage of
;;       ;; one directory per item. To speed it up, you would need to use a
;;       ;; single linked attachment dir for PDFs.
;;       citar-library-paths (thread-last
;;                               (directory-files "/Users/jkroes/Zotero/storage")
;;                             (remove ".")
;;                             (remove "..")
;;                             (remove ".DS_Store")
;;                             (mapcar
;;                              (lambda (x)
;;                                (concat "/Users/jkroes/Zotero/storage/" x)))))

(use-package! citar
  :when (featurep! :completion vertico)
  :no-require
  :config
  ;; NOTE: If no longer using org-noter, uncomment this
  ;; (setq citar-file-open-function
  ;;       ;; Need to mount the A: drive each session to access Zotero storage
  ;;       ;; (shell-command
  ;;       ;;  (concat "echo "
  ;;       ;;          (shell-quote-argument (read-passwd "Password: "))
  ;;       ;;          " | sudo -S mount -t drvfs A: /mnt/a"))
  ;;       (cond ((eq system-type 'gnu/linux) 'open-in-windows)
  ;;             ((eq system-type 'darwin) 'citar-file-open-external))

  ;; Used by citar-open-notes
  (setq citar-open-note-function 'citar--open-noter)

  ;; HACK From within an org-roam note (see ESM 203), citar-open-notes will
  ;; use the filetag as the initial input when completing bib entry candidates
  ;; The filetag should be a Zotero tag used in the org-cite folder.
  (cl-defun citar-select-ref (&optional &key rebuild-cache multiple filter)
    "Select bibliographic references.

A wrapper around 'completing-read' that returns (KEY . ENTRY),
where ENTRY is a field-value alist.  Therefore 'car' of the
return value is the cite key, and 'cdr' is an alist of structured
data.

Takes the following optional keyword arguments:

REBUILD-CACHE: if t, forces rebuilding the cache before offering
  the selection candidates.

MULTIPLE: if t, calls `completing-read-multiple` and returns an
  alist of (KEY . ENTRY) pairs.

FILTER: if non-nil, should be a predicate function taking
  arguments KEY and ENTRY.  Only candidates for which this
  function returns non-nil will be offered for completion.  For
  example:

  (citar-select-ref :filter (citar-has-file))

  (citar-select-ref :filter (citar-has-note))

  (citar-select-ref
   :filter (lambda (_key entry)
             (when-let ((keywords (assoc-default \"keywords\" entry)))
               (string-match-p \"foo\" keywords))))"
    (require 'org) ; HACK
    (let* ((candidates (citar--get-candidates rebuild-cache))
           (completions (citar--completion-table candidates filter))
           (embark-transformer-alist (citar--embark-transformer-alist candidates))
           (crm-separator "\\s-*&\\s-*")
           (chosen (if (and multiple citar-select-multiple)
                       (completing-read-multiple "References: " completions nil nil nil
                                                 'citar-history citar-presets nil)
                     (completing-read "Reference: " completions nil nil
                                      ;; HACK "Filter" candidates by using the
                                      ;; first non-project filetag as initial
                                      ;; input
                                      (concat (car (remove "project" org-file-tags)) " ")
                                      'citar-history citar-presets nil)))
           (notfound nil)
           (keyentries
            (seq-mapcat
             ;; Find citation key-entry of selected candidate.
             ;; CHOICE is either the formatted candidate string, or the citation
             ;; key when called through `embark-act`.  To handle both cases, test
             ;; CHOICE against the first two elements of the entries of
             ;; CANDIDATES.  See
             ;; https://github.com/bdarcus/citar/issues/233#issuecomment-901536901
             (lambda (choice)
               (if-let ((cand (seq-find
                               (lambda (cand) (member choice (seq-take cand 2)))
                               candidates)))
                   (list (cdr cand))
                 ;; If not found, add CHOICE to NOTFOUND and return nil
                 (push choice notfound)
                 nil))
             (if (listp chosen) chosen (list chosen)))))
      (when notfound
        (message "Keys not found: %s" (mapconcat #'identity notfound "; ")))
      (if multiple keyentries (car keyentries)))))


;;; tools/biblio/autoload.el -*- lexical-binding: t; -*-

;; Like citar--open-note, it creates a new note, but it uses org-noter
;; to display existing notes. It has the advantage that it doesn't
;; pollute the current frame with note buffers from which you have to
;; call org-noter.
;; TODO Add an embark-act option to open a note with the default
;; citar-open-note-function. Or create a parallel function to citar-open-notes
;; TODO Open newly created notes in a new frame
;;;###autoload
(defun citar--open-noter (key entry)
  (if-let* ((file (citar-file--get-note-filename key
                                                 citar-notes-paths
                                                 citar-file-note-extensions))
            (file-exists (file-exists-p file)))
      ;; Existing note
      (progn
        (let ((org-noter-always-create-frame nil)
              (org-noter-kill-frame-at-session-end t))
          ;; Open all notes in a new maximized frame
          (find-file-other-frame file)
          (toggle-frame-maximized)
          ;; If bib entry for note has a file, start org-noter. We could also
          ;; use citar-has-file (any performance benefit?) or query the
          ;; noter_document property added by citar-format-note-function for
          ;; new notes
          (if (assoc "file" entry)
              (progn
                ;; Hack to make sure we are inside the "Notes" heading before
                ;; calling org-noter. Shouldn't rely on evil, but whatever.
                ;; TODO Navigate to the "Notes" heading explicitly. Better yet,
                ;; navigate to the heading with property noter_document.
                (evil-window-bottom)
                (org-noter)))))
    ;; Create a new note
    (funcall citar-format-note-function key entry file)))


;; Like citar-org-format-note-default without #+print_bibliography and
;; integrated with org-noter
(defun citar-org-format-note-no-bib (key entry filepath)
  (let* ((template (citar-get-template 'note))
         (note-meta
          (when template
            (citar--format-entry-no-widths
             entry
             template)))
         (buffer (find-file filepath)))
    (with-current-buffer buffer
      ;; This just overrides other template insertion.
      (erase-buffer)
      (citar-org-roam-make-preamble key)
      (insert "#+title: ")
      (when template (insert note-meta))
      (insert "\n\n* Notes")
      (when (assoc "file" entry)
        ;; TODO This assumes a single file, but the field may have multiple
        ;; NOTE: Can't use org-roam-property-add because it enquotes paths
        ;; with spaces in them, which makes org-noter fail
        (org-set-property "NOTER_DOCUMENT" (wslify-bib-path (cdr (assoc "file" entry))))))))


;; This works with point anywhere in a roam note
;;;###autoload
(defun org-roam-open-refs ()
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
        (let ((citar-default-action 'citar-open-from-note))
          (citar-run-default-action oc-cites)))
      ;; Open ref URLs
      (dolist (ref refs)
        (unless (string-prefix-p "@" ref)
          (browse-url ref))))))


;; Like citar-open but excludes notes from candidates. For use with
;; org-roam-open-refs from within roam notes
(defun citar-open-from-note (keys-entries)
  (interactive (list
                (list (citar-select-ref
                       :rebuild-cache current-prefix-arg))))
  (when (and citar-library-paths
             (stringp citar-library-paths))
    (message "Make sure 'citar-library-paths' is a list of paths"))
  (let* ((embark-default-action-overrides
          '((multi-category . citar-open-multi)
            (file . citar-file-open)
            (url . browse-url)))
         (key-entry-alist (citar--ensure-entries keys-entries))
         (files
          (citar-file--files-for-multiple-entries
           key-entry-alist
           ;;(append citar-library-paths citar-notes-paths)
           citar-library-paths
           ;; find files with any extension:
           nil))
         (links
          (seq-map
           (lambda (key-entry)
             (citar-get-link (cdr key-entry)))
           key-entry-alist))
         (resource-candidates (delete-dups (append files (remq nil links))))
         (resource
          (if resource-candidates
              (citar-select-resource files links)
            (error "No associated resources"))))
    (citar-open-multi resource)))

;; To use org-roam-open-refs within WSL on a bib file generated by Windows
;; Zotero, we need to convert Windows to WSl paths. Note the quadrupled
;; backslashes in the regex strings.
;;;###autoload
(defun citar-file-parser-wsl (dirs file-field)
  ;; To avoid errors with e.g. citar-refresh when wslpath is not a real command.
  ;; TODO This is hacky and needs to be fixed eventually
  (when (eq system-type 'gnu/linux)
    (let* ((files (split-string file-field ";"))
           (wsl-files (mapcar #'wslify-bib-path files)))
      (delete-dups
       (seq-mapcat
        (lambda (dir)
          (mapcar
           (lambda (file)
             (expand-file-name file dir)) wsl-files))
        dirs)))))



;; NOTE Unlike import-zotero-annotations-from-note, which have Zotero open-pdf
;; links, this will insert an absolute filepath. The absolute filepath is useful
;; if we need to export the org-mode file to another format. We can copy that
;; file to a common directory then search-replace the links in the exported
;; file, to preserve access for those who can't access Emacs or my Zotero
;; library (i.e., everyone who is not me)
;; (defun citar-insert-file-link ()
;;   "Insert an org-mode link to a selected file attachment with the parent item title as the description"
;;   (interactive)
;;   (require 'citar)
;;   (let* ((key (citar-select-ref))
;;          (file (cdr (citar--select-resource key :files t)))
;;          (title (cdr (assoc "title" (citar-get-entry key)))))
;;     (org-insert-link nil file title)))

;;; NOTE This is possibly a more recent version of the functions above.

;; TODO Fix this function
;; citar-open first prompts for a key, then for a resource (file, URL, or note).
;; Here we skip the key-prompt by using the key stored in ROAM_REFS of the
;; current note
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

;; NOTE This doesn't seem to work when using citar-org-roam. See the :create
;; key of citar-org-roam-notes-config.
(setq citar-note-format-function #'citar-org-format-note-no-bib)
