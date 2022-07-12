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
  (setq citar-bibliography
        (cond ((eq system-type 'gnu/linux)
               '("/mnt/c/Users/jkroes/Documents/org-cite.bib"))
              ((eq system-type 'darwin)
               '("~/.doom.d/org/org-cite.bib")))
        ;; 1. Doom doesn't set org-cite-global-bibliography properly. When
        ;; :tools magit is enabled, org is loaded early, and oc loads before
        ;; citar. B/c of this, citar-bibliography needs to be reliably set
        ;; 2. citar only seems to use org-cite-global-bibliography for
        ;; functionality related to local bib files specified by the org-cite
        ;; #+bibliography keyword, but the README recommends setting it
        org-cite-global-bibliography citar-bibliography
        org-cite-insert-processor 'citar
        org-cite-follow-processor 'citar
        org-cite-activate-processor 'citar
        ;; Used by e.g. citar-open-notes to display bib entry candidates
        citar-templates
        '((main . "${title:140}")
          (suffix . " ${=key= id:15} ${tags keywords:*}")
          ;; Used by citar-insert-reference
          (preview . "${author editor} (${year issued date}) ${title}, ${journal publisher container-title collection-title}.\n")
          ;; Used by citar-open-notes to create new note
          (note . "${title}"))
        ;; Use icons to indicate resources associated with a bib entry
        citar-symbols
        `((file ,(all-the-icons-faicon "file-o" :face 'all-the-icons-green :v-adjust -0.1) . " ")
          (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) . " ")
          (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01) . " "))
        ;; Padding between resource indicators (icons)
        citar-symbol-separator "  "
        ;; citar-default-action 'embark-act
        ;; Location of notes associated with bib entries
        citar-notes-paths '("~/.doom.d/org/cite/")
        ;; Used by citar-open-notes
        citar-open-note-function 'citar--open-noter
        citar-format-note-function 'citar-org-format-note-no-bib))

(with-eval-after-load 'citar-file
  ;; Convert Windows to WSL paths when opening PDFs
  (add-to-list 'citar-file-parser-functions 'citar-file-parser-wsl))

;; BUG Why do these fail when used in :config rather than after!? E.g.,
;; void-variable citar-major-mode-function, or failing to redefine a function
;; defined in citar? See https://github.com/doomemacs/doomemacs/issues/6367
(after! citar
  ;; Let embark-act work with org-cite citation keys in roam_refs at point
  (setf (alist-get
         'key-at-point
         (alist-get '(org-mode) citar-major-mode-functions nil nil #'equal))
        #'aj/citar-org-key-at-point)

  ;; NOTE Makes embark-act>citar-run-default-action (where default action is
  ;; citar-open) work the same as +org/dwim-at-point (RET) or embark-dwim
  ;; (where default-action is citar-open, at least) with point on a citation.
  ;; BUG citar-run-default-action expects its argument to be a list, but when
  ;; called from embark-act receives a string.
  ;; HACK Borrow code from embark-dwim to check if keys-entries is a string and
  ;; if so convert it to a list
  (defun citar-run-default-action (keys-entries)
    "Run the default action `citar-default-action' on KEYS-ENTRIES."
    (funcall citar-default-action
             (if (listp keys-entries) keys-entries (list keys-entries))))

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

;;;###autoload
  (defun aj/citar-org-key-at-point ()
    "Return key at point for org-cite citation-reference or citekey."
    (or (citar-org-key-at-point)
        (when (org-in-regexp org-element-citation-key-re)
          (cons (substring (match-string 0) 1)
                (cons (match-beginning 0)
                      (match-end 0))))))


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

;; Backgrond reading:
;; https://orgmode.org/manual/Citation-handling.html
;; https://blog.tecosaur.com/tmio/2021-07-31-citations.html#fn.3
