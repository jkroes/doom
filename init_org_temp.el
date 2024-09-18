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

;;;; biblio / citar  ----------------------------------------------------------




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
