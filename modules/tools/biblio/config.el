;;; tools/biblio/config.el -*- lexical-binding: t; -*-

;; Background reading:
;; https://orgmode.org/manual/Citation-handling.html
;; https://blog.tecosaur.com/tmio/2021-07-31-citations.html#fn.3

;;
;;; `org-cite'

(use-package! oc
  :defer t
  :config
  (setq org-cite-global-bibliography
        (doom-enlist
         (or (bound-and-true-p citar-bibliography)
             (bound-and-true-p bibtex-completion-bibliography)))
        ;; Setup export processor; default csl/citeproc-el, with biblatex for
        ;; latex
        org-cite-export-processors '((latex biblatex) (t csl))
        org-support-shift-select t))


(use-package! citar
  :when (featurep! :completion vertico)
  :no-require
  :init
  ;; Bibliography location
  ;; Without this (taken from the org module) I'm getting a void-variable
  ;; error for org-directory
  (unless (boundp 'org-directory)
    (setq-default org-directory "~/org"))
  (setq citar-bibliography
       (cond (IS-WSL '("/mnt/d/org-cite.bib"))
             (t (list (expand-file-name "org-cite.bib" org-directory))))
       citar-templates
       '((main . "${title:140}")
         (suffix . " ${=key= id:15} ${tags keywords:*}")
         ;; Used by citar-insert-reference
         (preview . "${author editor} (${year issued date}) ${title}, ${journal publisher container-title collection-title}.\n")
         ;; Used by citar-open-notes to create new note
         (note . "${title}"))
       ;; Location of notes associated with bib entries
       citar-notes-paths (list (expand-file-name "cite" org-directory))
       ;; Open files (as opposed to notes or URLs) in Zotero
       citar-file-open-function #'open-in-zotero
       ;; (cond (IS-WSL #'open-in-windows)
       ;;       (t #'citar-file-open-external))
       ;; Function for creating new note (see also citar-templates)
       citar-note-format-function #'citar-org-format-note-no-bib
       ;; Padding between resource indicators (icons)
       citar-symbol-separator "  "
       ;; Whether to use multiple selection
       citar-select-multiple nil)

  :config
  (setq org-cite-insert-processor 'citar
        org-cite-follow-processor 'citar
        org-cite-activate-processor 'citar)

  ;; TODO Why is after! necessary here within :config? Getting a void-variable
  ;; error for citar-major-mode-functions
  (after! citar
    (setf (alist-get
           'key-at-point
           (alist-get '(org-mode) citar-major-mode-functions nil nil #'equal))
          #'aj/citar-org-key-at-point))

  ;; Use icons to indicate resources associated with a bib entry
  (when (display-graphic-p)
    (setq citar-symbols
          `((file ,(all-the-icons-faicon "file-o" :face 'all-the-icons-green :v-adjust -0.1) . " ")
            (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) . " ")
            (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01) . " ")))))


;; Make embark-act recognize org-cite keys at point in roam_refs
;; BUG Why does after! work when :config results in a void-variable error
;; for citar-major-mode-functions
;; My issue about this: https://github.com/doomemacs/doomemacs/issues/6367
(after! citar
  (setf (alist-get
         'key-at-point
         (alist-get '(org-mode) citar-major-mode-functions nil nil #'equal))
        #'aj/citar-org-key-at-point))

;; Convert Windows to WSL paths when opening PDF files
(after! citar-file
  (add-to-list 'citar-file-parser-functions 'citar-file--parser-default-wsl))

;; `org-cite' processors
(use-package! oc-biblatex :after oc)
(use-package! oc-csl :after oc)
(use-package! oc-natbib :after oc)

(use-package! citar-embark
  :after citar embark
  :no-require
  :config (citar-embark-mode))

;;
;;; Third-party

(use-package! bibtex-completion
  :when (or (featurep! :completion ivy)
            (featurep! :completion helm))
  :defer t
  :config
  (setq bibtex-completion-additional-search-fields '(keywords)
        ;; Tell bibtex-completion to look at the File field of the bibtex to
        ;; figure out which pdf to open:
        bibtex-completion-pdf-field "file"))


(use-package! ivy-bibtex
  :when (featurep! :completion ivy)
  :defer t
  :config
  (add-to-list 'ivy-re-builders-alist '(ivy-bibtex . ivy--regex-plus)))

;;
;;; Zotero annotations in Emacs

;; TODO This doesn't work on WSL because firewall rules must be disabled to allow WSL to connect to Windows,
;; and that requires admin approval
;; See https://retorque.re/zotero-better-bibtex/exporting/pull/
;; Right click a Zotero collection and select "Download Betterbibtex export" to get the URL
;; (defun update-bib-file ()
;;   (interactive)
;;   (let ((root_url (shell-quote-argument
;;                    ;; use $(hostname).local in lieu of the ip for localhost on wsl
;;                    "http://127.0.0.1:23119/better-bibtex/export/collection?/1/org-cite.biblatex"))) ; &exportnotes=true
;;     (shell-command (format "wget %s -o %s" root_url citar-bibliography))))

;; TODO drag and drop for text doesn't work with mobaxterm Emacs and Windows. It does work for terminal emacs.
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

(defvar zotero-annotations-file
  (cond (IS-WSL "/mnt/d/annotations.md")
        (t "~/Downloads/annotations.md")))
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
;; NOTE This last step might not be necessary
(after! ol
  (org-link-set-parameters "zotero" :follow
                           (lambda (zpath)
                             (browse-url
                              ;; we get the "zotero:"-less url, so we put it back.
                              (format "zotero:%s" zpath)))))

;; NOTE This works on MacOS but won't work in WSL
;;(use-package! zotxt)

;; NOTE Unlike import-zotero-annotations-from-note, which have Zotero open-pdf links, this will insert
;; an absolute filepath. The absolute filepath is useful if we need to export the org-mode file to
;; another format. We can copy that file to a common directory then search-replace the links in the
;; exported file, to preserve access for those who can't access Emacs or my Zotero library (i.e.,
;; everyone who is not me)
(defun citar-insert-file-link ()
  "Insert an org-mode link to a selected file attachment with the parent item title as the description"
  (interactive)
  (require 'citar)
  (let* ((key (citar-select-ref))
         (file (cdr (citar--select-resource key :files t)))
         (title (cdr (assoc "title" (citar-get-entry key)))))
    (org-insert-link nil file title)))

;; TODO Open note matching file link
;; TODO Export and replace links
;; TODO Search citar PDF text like Zotero (?):
;; https://github.com/emacs-citar/citar/wiki/Example-functions#search-contents-of-pdfs
;; TODO Alternative Zotero integration ideas
;; https://github.com/emacs-citar/citar/issues/685
