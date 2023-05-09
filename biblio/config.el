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
       ;; citar-notes-paths (list (expand-file-name "cite" org-directory))
       citar-notes-paths (list org-directory)
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

;; NOTE Unlike import-zotero-annotations-from-note, which have Zotero open-pdf
;; links, this will insert an absolute filepath. The absolute filepath is useful
;; if we need to export the org-mode file to another format. We can copy that
;; file to a common directory then search-replace the links in the exported
;; file, to preserve access for those who can't access Emacs or my Zotero
;; library (i.e., everyone who is not me)
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
