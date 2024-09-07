;; Shrink tables on startup and show shrunk text in the echo area automatically
;; when cursor is over the ellipses that represent the shrunk text
(setq org-startup-shrink-all-tables t
      help-at-pt-display-when-idle t
      help-at-pt-timer-delay 0.25)
(help-at-pt-set-timer)

(when doom-variable-pitch-font
  (add-hook 'org-mode-hook 'variable-pitch-mode)
  ;; NOTE Unlike the `line-spcing' text property, this works with visual lines
  (setq-hook! 'org-mode-hook line-spacing 0.5))

;; TODO It is still a pain in the ass to define headings this way, as opposed
;; to just using a spreadsheet. There should be some way to interface between
;; the two. Spreadsheets are easier to edit, because you enter the headers
;; exactly once and don't need SPC-m-o to properly enter data.
(defun org-property-from-columns ()
  "Add properties that start with a lowercase character and are
specified in a file-level COLUMNS property to the heading at
point."
  (interactive)
  (let ((columns (split-string (org-entry-get 0 "COLUMNS") " ")))
    (dolist (col columns)
      (setq col (replace-regexp-in-string "^%" "" col))
      (when (and (s-lowercase-p (substring col 0 1))
                 (not (org-entry-get nil col)))
        (org-set-property col "")))))

;;;; org-element --------------------------------------------------------------

;; TODO Prior to org v9.6, org-element threw a lot of errors about invalidated
;; cache. Disable the cache if you are still having issues. org v9.6 refactored
;; the cache and encouraged users to test it out. See
;; https://orgmode.org/Changes.html.

;; (setq org-element-use-cache nil
;;       org-element--cache-self-verify 'backtrace)

;;;; org-attach ---------------------------------------------------------------

(after! org
  (when IS-WSL
    ;; Windows-based attachment directory for WSL Emacs
    (setq org-attach-id-dir
          "/mnt/c/Users/jkroes/OneDrive - California Department of Pesticide Regulation (1)/org-attach"))

  ;; When enabled, org-attach will add attachments to the nearest node with an
  ;; attachment directory, so you can e.g. add an attachment to an org-roam
  ;; file from anywhere in the file if none of the above headings have an
  ;; attachment dir. To create an attachment dir on the current heading, first
  ;; run `org-id-get-create'.
  (setq org-attach-use-inheritance t)

  ;; When running org-attach, store a file link with absolute path to the
  ;; attached file. You can also store a link via embark-act, embark-copy-as-kill
  ;; (@w), then yank.
  (setq org-attach-store-link-p 'attached))

;; Stack trace when following attachment links:
;; org-open-at-point
;; org-link-open
;; org-attach-follow
;; org-link-open-as-file(org-attach-expand)
;; org-open-file
;; (user-error "No such file: %s" file))
(defadvice! +org-attach-expand-a (file)
  "HACK A version of org-attach-expand that actually will look through all
parent headings until it finds the linked attachment, to quote the docs for
`org-attach-use-inheritance'. Normally the search stops at the first heading for
which there is an attachment directory"
  :override #'org-attach-expand
  (let ((filepath (expand-file-name file (org-attach-dir))))
    (if (and (org-attach-dir)
             (file-exists-p filepath))
        filepath
      (if (= (point) (point-min))
          ;; Don't pass back control to org-attach-follow,
          ;; then org-link-open-as-file, then org-open-file.
          ;; If no file is found, exit immediately.
          (user-error "No such file: %s" file)
        (org-roam-up-heading-or-point-min)
        (org-attach-expand file)))))


(defvar org-attach-ignore-regexp-list (list "." ".." ".DS_STORE")
  "A list of filenames for org-attach to ignore")


(defadvice! +org-attach-file-list-a (directory)
  "Return a list of files in the attachment DIRECTORY.
This ignores \".\", \"..\", \".DS_STORE\", and files ending in \"~\"."
  :override #'org-attach-file-list
  (delq nil
        (mapcar (lambda (x)
                  (if (string-match
                       (concat "^"
                               (regexp-opt
                                org-attach-ignore-regexp-list)
                               "\\'")
                       x) nil x))
                (directory-files directory nil "[^~]\\'"))))


(defun delete-empty-org-attach-id-dirs ()
  "Delete empty directories within org-attach-id-dir."
  (interactive)
  (require 'dash)
  ;; Delete org-attach-id-dir sub-sub folders
  (-each
      (-filter
       (lambda (file) (directory-empty-p file))
       (directory-files-recursively org-attach-id-dir "" t))
    #'delete-directory)
  ;; Delete org-attach-id-dir sub-folders. Some will be newly empty after the
  ;; last deletion.
  (-each
      (-filter
       (lambda (file) (directory-empty-p file))
       (directory-files org-attach-id-dir t))
    #'delete-directory))

;;;; org-roam --------------------------------------------------

;; BUG If `org-roam-node-find' returns no nodes, it is probably an issue with
;; this file.
;; NOTE This must be a relative path. org-roam initializes this with
;; `org-attach-id-dir' but doesn't check whether it's a relative path!
;; See `org-roam-file-p'.
(after! org-roam
  (setq org-roam-file-exclude-regexp (if (not IS-WSL)
                                         (list (file-name-nondirectory (directory-file-name org-attach-id-dir)))
                                       (list)))
  ;; TODO Remove this once you finish processing these files.
  (push "reorg/" org-roam-file-exclude-regexp))

(after! org-roam
  ;; Only complete roam node titles within link brackets; otherwise, completion
  ;; interferes with normal typing
  (setq org-roam-completion-everywhere nil)

  ;; NOTE If this isn't working, try running `org-roam-db-clear-all',then
  ;; `org-roam-db-sync'
  (defvar org-roam-excluded-tags
     (list (bound-and-true-p org-archive-tag)
           (bound-and-true-p org-attach-auto-tag)
           ;; Omit vulpea tag. TODO Update this when you create a variable
           ;; to customize the vulpea tag
           "project"))

  (setq org-roam-db-node-include-function
        (lambda ()
          (not (-any (lambda (tag)
                       (member tag (org-get-tags nil)))
                     org-roam-excluded-tags)))))


;; Replace all roam: links generated by org-roam completion with id: links, on
;; file save.
(add-hook 'org-mode-hook
          (lambda () (add-hook 'after-save-hook #'org-roam-link-replace-all nil t)))

;;;; consult-org-roam

(use-package! consult-org-roam
  :after org-roam
  :config
  ;; Annotate tags rather than including them in
  ;; org-roam-node-display-template. In the latter case, they are part of the
  ;; vertico candidate string, and vertico-insert (tab) inserts the tags as
  ;; well as the org-roam node string. Plus there's this unresolved bug:
  ;; https://github.com/org-roam/org-roam/issues/2066. On the flip side, you
  ;; can't search annotations.
  ;; TODO my/org-roam-node-read--annotation isn't working as of 2/4/24. I
  ;; (after! dendroam (setq org-roam-node-annotation-function #'my/org-roam-node-read--annotation))
  ;; Faster live preview
  (setq consult-org-roam-grep-func #'consult-ripgrep)
  ;; Advise org-roam-node-read to use consult--read. This package uses
  ;; live previews by default (consult-org-roam--node-preview), but you
  ;; can suppress them via consult-customize.
  (consult-org-roam-mode)
  (consult-customize org-roam-node-find :preview-key "C-SPC"))

(autoload #'org-roam-node-dendroam-tags "dendroam")
(defun my/org-roam-node-read--annotation (node)
  "Replaces org-roam's dummy annotation function for org-roam-node-read"
  (concat (make-string 5 ?\s)
          (mapconcat #'identity (org-roam-node-dendroam-tags node) " ")))


;;;; biblio / citar  ----------------------------------------------------------

;; List of commands:
;;
;; citar-open
;;
;; org-cite-insert / citar-insert-citation
;;
;; citar-insert-reference (for a references section; unused currently)
;;
;; org-roam-ref-add (for non-Zotero URLs and wayback URLs)
;;
;; org-roam-ref-find (for finding a node by URL)
;;
;; citar-org-roam-ref-add: Add an additional citation key to roam refs of the
;; node at point.
;;
;; org-roam-cited
;; - TODO This throws an error "Wrong type argument: org-roam-node, nil" if
;; there are no citations. The function is fine, but it needs to be rewritten
;; to return nil rather than attempting to visit a nonexistent node.

;; TODO +org/dwim erases citations when this is the default action (on WSL,
;; untested on MacOS).
;; (setq citar-default-action #'citar-insert-edit)

;; Location of citar notes (what I call "reference" notes). NOTE citar-open can
;; find files outside of citar-org-roam-subdir.
(setq citar-notes-paths (list org-directory))

;; NOTE Windows Zotero can't write to org-directory (TODO OneDrive or WSL?)
(setq citar-bibliography
      (list (expand-file-name "org-cite.bib"
                              (cond (IS-WSL "/mnt/d")
                                    (t org-directory)))))

;; NOTE modules/tools/biblio/config.el does not set this correctly
(setq org-cite-global-bibliography citar-bibliography)

;; Whether to use multiple selection
(setq citar-select-multiple nil)

;; Complete citations keys
(add-hook 'org-mode-hook #'citar-capf-setup)

(setq citar-templates
      ;; `main' and `suffix' are used by `citar-open' and other functions to
      ;; display existing notes
      '((main . "${date:10} ${title:120}")
        ;; NOTE Zotero tags are in the better-biblatex field "keywords" within
        ;; `citar-bibliography'
        (suffix . " ${tags keywords:*}")
        ;; The format for reference text inserted by `citar-insert-reference'
        (preview . "${author editor} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
        ;; The format for new notes created by `citar-open-notes'
        (note . "${title}")))

;; Open files (as opposed to notes or URLs) in Zotero
(setq citar-file-open-functions (list (cons "pdf" #'open-in-zotero)
                                      (cons "html" #'citar-file-open-external)
                                      (cons t (cond (IS-WSL #'open-in-windows)
                                                    (t #'find-file)))))

;; Convert Windows to WSL paths when opening PDF files
(after! citar-file
  (add-to-list 'citar-file-parser-functions 'citar-file--parser-default-wsl))


(defun open-in-zotero (file)
  "Open file resources in Zotero PDF viewer."
  (string-match ".*/storage/\\(.*\\)/.*\\.pdf" file)
  (browse-url
   ;; NOTE You can also use select instead of open-pdf to see the
   ;; attachment item in the item pane
   (replace-match "zotero://open-pdf/library/items/\\1" nil nil file)))


;; TODO This will not allow you to open files in the shared Zotero Air Program
;; library (CDPR Master Zotero Collection), but citar highlights any citation
;; keys you insert that have bad filepaths
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


;;;; citar-org-roam ------------------------------------------------------------

(use-package! citar-org-roam
  :after org-roam
  :config
  ;; Subdirectory of `org-roam-directory' for citar notes
  (setq citar-org-roam-subdir "references")
  ;; citar note filetitle, which is the title field from the bibliography
  (setq citar-org-roam-note-title-template "${title}")
  (citar-org-roam-mode))


(defadvice! citar-org-roam--create-capture-note-a (citekey entry)
  "Slightly modified function for citar-reference-note creation"
  :after #'citar-org-roam--create-capture-note
  (when (fboundp 'evil-insert)
    (evil-append-line 1)
    (insert "\n")))


;;;; dendroam --------------------------------------------------------------------

;; TODO Create a command that completes only the unique components at each
;; level of the heirarchy. This makes finding things easier if you don't know
;; what you're looking for.

;; NOTE Nodes can be created by running org-roam-node-find;
;; org-roam-node-insert; or by typing a roam link like
;; [[roam:this is a roam link]], moving the cursor onto it,
;; and running org-open-at-point. The latter behavior is
;; similar to how dendroam works, where following a link
;; creates a new node.

(add-to-list 'load-path (expand-file-name "libraries" doom-private-dir))

;; TODO Is this the best solution?
(autoload #'dendroam-find "dendroam")

;; TODO org-roam links only complete the title, which is a problem for dendroam
;; since it allows for the same title with different hierarchies. See
;; `org-roam-link-auto-replace'
;; TODO org-roam completion only seems to work with trailing brackets, per
;; doom's global smartparens mode
;; TODO org-roam seems capable of completing node headings with trailing
;; brackets, but pcomplete-completions-at-point (the capf provided by org-mode)
;; is incapable of doing so. To complete with org-mode, you need e.g. "[[*XXX",
;; where XXX is the name of a heading in the buffer (see
;; https://orgmode.org/manual/Completion.html).

;;;; zotero --------------------------------------------------------------

;; NOTE This works on MacOS but won't work in WSL
;;(use-package! zotxt)

;; Setup within WSL Ubuntu (based on zotero.org/support/installation
;; and "create a custom url protocol with xdg in ubuntu" and
;; "url protocol handlers in basic ubuntu desktop"
;; 1. Create ~/.local/share/applications/Zotero.desktop
;; 2. Add the following to the file:
;; [Desktop Entry]
;; Name=Zotero
;; Exec="/mnt/c/Users/jkroes/AppData/Local/Zotero/zotero.exe" -url %U
;; Terminal=false
;; Type=Application
;; MimeType=x-scheme-handler/zotero
;; 3. Run (without quotes) "xdg-mime default Zotero.desktop x-scheme-handler/zotero
(after! ol
  (org-link-set-parameters "zotero" :follow
                           (lambda (zpath) (browse-url (format "zotero:%s" zpath)))))

;; Deconstruct annotations imported via import-zotero-annotations-from-note
;; using an evil macro invoked by "@z"
(evil-set-register ?z [?A ?\d ?\d escape ?0 ?d ?f ?\[ ?.])

;; Set extensions.zotero.annotations.noteTemplates.title to "annotations"
;; (without the quotes). Delete the entry for
;; extensions.zotero.annotations.noteTemplates.note. Then only highlight
;; annotations will be exported, which simplifies the regexp. This is fine
;; because only highlight annotations contain a link back to the location in the
;; PDF. Furthermore, the default filename is the title, so you can use that in the
;; code below.
;; In zotero, create annotations in a PDF attachment.
;; Right click one or more items, "Add note from annotations"
;; Right click a single note, "Export note" as markdown including zotero links
;; Export as ~/Downloads/annotations.md (after a couple times, this should be
;; the default, on MacOS at least
;; TODO You can select multiple notes, and they will be separated by "---"
;; https://www.zotero.org/support/note_templates
;; NOTE May have to delete previous annotation file for subsequent export to
;; succeed. If note template contains no title, you need to choose a filename
(defvar zotero-annotations-file
  (cond (IS-WSL "/mnt/d/annotations.md")
        (t "~/Downloads/annotations.md")))
(defun import-zotero-annotations-from-note (buf)
  "Import Zotero annotations from a markdown notes-export file,
convert the annotations to org-mode links with annotation
comments underneath, and display the buffer"
  (interactive
   (list (find-file-noselect (read-file-name
                              "Note file (default Annotations.md): "
                              (file-name-directory zotero-annotations-file)
                              zotero-annotations-file))))
  (with-current-buffer buf
    (beginning-of-buffer)
    (kill-whole-line 2) ; Delete the title and subsequent line
    (while (re-search-forward "(\\[.*?](zotero://select.*?)) " nil t)
      (replace-match ""))
    (beginning-of-buffer)
    (while (re-search-forward "^\u201C\\(.*\\)\u201D (\\[pdf](\\(zotero://open-pdf.*?\\)))[ ]*" nil t)
      (replace-match "[[\\2][\\1]]\n\n"))
    (beginning-of-buffer)
    (while (re-search-forward "\n\n\n" nil t)
      (replace-match "\n"))
    (org-mode))
  (pop-to-buffer buf))

;; Zotero annotations in Emacs:

;; TODO This doesn't work on WSL because firewall rules must be disabled to
;; allow WSL to connect to Windows, and that requires admin approval See
;; https://retorque.re/zotero-better-bibtex/exporting/pull/
;;
;; Right click a Zotero collection and select "Download Betterbibtex export" to
;; get the URL
;; (defun update-bib-file ()
;;   (interactive)
;;   (let ((root_url (shell-quote-argument
;;                    ;; use $(hostname).local in lieu of the ip for localhost on wsl
;;                    "http://127.0.0.1:23119/better-bibtex/export/collection?/1/org-cite.biblatex"))) ; &exportnotes=true
;;     (shell-command (format "wget %s -o %s" root_url citar-bibliography))))

;; TODO drag and drop for text doesn't work with mobaxterm Emacs and Windows.
;; It does work for terminal emacs.
;; (after! dnd
;;   (add-to-list 'dnd-protocol-alist
;;                (cons "zotero://" #'dnd-transform-zotero)))
;; (advice-add 'xterm-paste :override 'my/xterm-paste)
;; (defun my/xterm-paste (event)
;;   (interactive "e")
;;   (unless (eq (car-safe event) 'xterm-paste)
;;     (error "xterm-paste must be found to xterm-paste event"))
;;   (let ((pasted-text (dnd-transform-zotero (nth 1 event) 'return)))
;;     (if xterm-store-paste-on-kill-ring
;;         ;; Put the text onto the kill ring and then insert it into the
;;         ;; buffer.
;;         (let ((interprogram-paste-function (lambda () pasted-text)))
;;           (yank))
;;       ;; Insert the text without putting it onto the kill ring.
;;       (push-mark)
;;       (insert-for-yank pasted-text))))
;; (defun dnd-transform-zotero (url action)
;;   "Transform Zotero highlight annotations that are dragged to Emacs from the PDF
;; viewer into org-mode links. These annotations consist of highlighted text
;; surrounded by Unicode quotes and followed by two links in markdown format:
;; zotero select and zotero open-pdf."
;;   (if (string-match "^\u201C\\(.*\\)\u201D.*(\\[pdf](\\(.*\\))) ?\\(.*\\)" url)
;;       (progn
;;         (let* ((annot-link (replace-match "\n[[\\2][\\1]]\n" nil nil url))
;;                (comment (replace-match "\\3" nil nil url))
;;                (all (if (string-empty-p comment)
;;                         annot-link
;;                       (concat annot-link "\n" comment "\n"))))
;;           (if (eq action 'return)
;;               all
;;             (insert all))))
;;     url))

;;;; stuff from the bottom of config


;; TODO Integrate code block expansion. See
;; https://orgmode.org/manual/Structure-Templates.html and
;; https://emacs.stackexchange.com/questions/12841/quickly-insert-source-blocks-in-org-mode

;; (setq org-structure-template-alist
;;       '(("r" . "src ruby")
;;         ("e" . "src emacs-lisp")))

;; Insert pairs of tildes in org-mode
(after! smartparens (sp-local-pair 'org-mode "~" "~" ))

;; Inspired by
;; https://mbork.pl/2022-01-17_Making_code_snippets_in_Org-mode_easier_to_type.
;;
;; Insert backticks instead of tildes and vice versa in org-mode.
;; Insert and enter a code block by inserting three backticks.
;; The language can be specified with `org-insert-tilde-language'
;;
;; NOTE Set `org-insert-tlde-language' to the language you are
;; currently working with most often, within `org-directory'/.dir-locals.el

(defvar-local org-insert-tilde-language nil
  "Default language name in the current Org file.
If nil, `org-insert-tilde' after 2 tildes inserts an \"example\"
block.  If a string, it inserts a \"src\" block with the given
language name.")

(defun org-insert-tilde ()
  "Insert a tilde using `org-self-insert-command'."
  (interactive)
  (if (string= (buffer-substring-no-properties (- (point) 3) (point))
	       "\n~~")
      (progn (delete-char -2)
	     (if org-insert-tilde-language
		 (insert (format "#+begin_src %s\n#+end_src"
				 org-insert-tilde-language))
	       (insert "#+begin_example\n#+end_example"))
	     (forward-line -1)
	     (if (string= org-insert-tilde-language "")
		 (move-end-of-line nil)
	       (org-edit-special)))
    (setq last-command-event ?~)
    (call-interactively #'org-self-insert-command)))

(defun org-insert-backtick ()
  "Insert a backtick using `org-self-insert-command'."
  (interactive)
  (setq last-command-event ?`)
  (call-interactively #'org-self-insert-command))


(after! org
  (define-key org-mode-map (kbd "`") #'org-insert-tilde)
  (define-key org-mode-map (kbd "~") #'org-insert-backtick))

;; Prefer heading navigation to element navigation
(defun my/evil-org--populate-navigation-bindings ()
  "Configures gj/gk/gh/gl for navigation."
  (let-alist evil-org-movement-bindings
    (evil-define-key 'motion evil-org-mode-map
      (kbd (concat "g" .left)) #'org-backward-heading-same-level
      (kbd (concat "g" .right)) #'org-forward-heading-same-level
      (kbd (concat "g" .up)) #'org-previous-visible-heading
      (kbd (concat "g" .down)) #'org-next-visible-heading
      (kbd (concat "g" (capitalize .left))) #'evil-org-top)))

(after! evil-org (my/evil-org--populate-navigation-bindings))

;; Makes inserting org footnotes easier. Type M-e or M-a to
;; forward- or backward-sentence, then SPC-m-f
(setq-hook! 'org-mode-hook
  sentence-end "[.?!…,;:]")