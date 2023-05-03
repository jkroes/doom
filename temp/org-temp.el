;;; org-temp.el -*- lexical-binding: t; -*-

;;; roam

  ;; Hide attachment headings from roam
  ;; (setq org-roam-db-node-include-function
  ;;       (lambda ()
  ;;         (not (member "ATTACH" (org-get-tags)))))

;;; org-blocks

;; NOTE: If org blocks are malformed (e.g., #+end_src is not on its own line),
;; org-toggle-blocks will fail, because org-hide-block-all will fail with
;; 'user-error: Not at a block' or something similar.

;; TODO Without the after!, the defcustom's initial value for org-cycle-hook is
;; not set. Does add-hook prevent defcustom initial values?
;; (after! org
;;   (add-hook 'org-mode-hook 'org-blocks-hidden-initialize)
;;   (add-hook 'org-cycle-hook 'org-cycle-blocks))

;; Without an initial valuek, helpful-variable won't show this variable at all,
;; even in a buffer with a local binding
;; TODO File a bug report, since describe-variable doesn't have this issue
;; (defvar org-blocks-hidden nil)

;; (defun org-blocks-hidden-initialize ()
;;   (setq-local org-blocks-hidden org-hide-block-startup))

;; (defun org-cycle-blocks (cycle-state)
;;   "Make org-cycle respect the value of org-blocks-hidden (set by
;; org-toggle-blocks)"
;;   (if (memq cycle-state '(all subtree))
;;       (if org-blocks-hidden
;;           (org-hide-block-all)
;;         (org-show-block-all))))

;; TODO Bind this function
;; (defun org-toggle-blocks ()
;;   "Toggle block visibility on or off."
;;   (interactive)
;;   (if org-blocks-hidden
;;       (org-show-block-all)
;;     (org-hide-block-all))
;;   (setq-local org-blocks-hidden (not org-blocks-hidden)))

;;;; vulpea

(setq org-agenda-hide-tags-regexp "project")

;; org-agenda slows in proportion to the number of files it has to read.
;; The code here builds a dynamic agenda from files with todo entries within
;; org-roam-agenda. Pulled from:
;; https://d12frosted.io/posts/2021-01-16-task-management-with-roam-vol5.html

;; Old setting for org-agenda-files
;; (defun dir-and-subdirs (dir)
;;   (require 'dash)
;;   (let ((dirs (-filter (lambda (file) (file-directory-p file))
;;                        (directory-files-recursively dir "" t))))
;;     (push dir dirs)))
;; (setq org-agenda-files (mapcan #'dir-and-subdirs (list org-directory)))

;; TODO There may be more entrypoints in need of advising. Some of them may call
;; the others, so some may be redundant. Investigate.
(advice-add 'org-agenda :before #'vulpea-agenda-files-update)
(advice-add 'org-agenda-list :before #'vulpea-agenda-files-update)
(advice-add 'org-todo-list :before #'vulpea-agenda-files-update)
(advice-add 'consult-org-agenda :before #'vulpea-agenda-files-update)

(defun vulpea-agenda-files-update (&rest _)
  "Update the value of `org-agenda-files'."
  (setq org-agenda-files (vulpea-project-files)))

;; NOTE: Doom advises org-roam-db-query to initialize org-roam. I suspect that
;; without this advice we would need to ensure org-roam is loaded before
;; `vulpea-project-files' runs. I don't think `org-roam-db-query' is
;; normally autoloaded
(defun vulpea-project-files ()
  "Return a list of note files containing 'project' tag." ;
  (seq-uniq
   (seq-map
    #'car
    (org-roam-db-query
     [:select [nodes:file]
      :from tags
      :left-join nodes
      :on (= tags:node-id nodes:id)
      :where (like tag (quote "%\"project\"%"))]))))

(add-hook 'find-file-hook 'vulpea-project-update-tag)
(add-hook 'before-save-hook 'vulpea-project-update-tag)

(defun vulpea-project-update-tag ()
  "Update PROJECT tag in the current buffer."
  (require 'vulpea) ; HACK Since I don't use the rest of the package elsewhere
  (when (and (not (active-minibuffer-window))
             (vulpea-buffer-p))
    (save-excursion
      (goto-char (point-min))
      (let* ((tags (vulpea-buffer-tags-get))
             (original-tags tags))
        (if (vulpea-project-p)
            (setq tags (cons "project" tags))
          (setq tags (remove "project" tags)))

        ;; cleanup duplicates
        (setq tags (seq-uniq tags))

        ;; update tags if changed
        (when (or (seq-difference tags original-tags)
                  (seq-difference original-tags tags))
          (apply 'vulpea-buffer-tags-set tags))))))

(defun vulpea-buffer-p ()
  "Return non-nil if the currently visited buffer is a note."
  (and buffer-file-name
       (string-prefix-p
        (expand-file-name (file-name-as-directory org-roam-directory))
        (file-name-directory buffer-file-name))))

;; TODO: Update this to also include timestamps, schedules, and deadlines--
;; anything that agenda should show by default
(defun vulpea-project-p ()
  "Return non-nil if current buffer has any todo entry.

todo entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks."
  (org-element-map
      (org-element-parse-buffer 'headline)
      'headline
    (lambda (h)
      (eq (org-element-property :todo-type h)
          'todo))
    nil 'first-match))

  ;; NOTE: For now, this cannot be set via file-local variable list:
  ;; # Local Variables:
  ;; # eval: (make-local-variable 'org-todo-keywords)
  ;; # eval: (add-to-list 'org-todo-keywords '(sequence "NEW" "EXEMPT" "HOLD" "REVIEWED" "BRENNA" "SIGN" "MANAGE" "|" "ROUTED") t)
  ;; # End:
  ;; This evaluates correctly, but does not work with the way keywords are set
  ;; (see org-todo-keywords-1)
  ;; Unfortunately, "[ ]" cannot be set using the existing file-local keywords
  ;; method offered by org; see
  ;; https://orgmode.org/manual/Per_002dfile-keywords.html
  ;; NOTE: For fast selection based on a letter, use org-todo. S-left/right
  ;; cycles within a sequence, then within the next, and so on. C-S-left/right
  ;; cycles between the first item of each sequence. As long as one keyword
  ;; has a parenthesized letter at the end, fast selection is enabled and will
  ;; auto-assign letters.


;;; org-attach

;; org-attach-use-inheritance:
;; Contrary to the docs, org-insert-link and org-open-at-point do not
;; resolve attachment links by searching all parent headings. The
;; nearest heading with an ID is found, and the link is resolved there,
;; creating a new file if the link can't be resolved. In other words,
;; attachment links only work if the link is to an attachment of the
;; nearest parent with an ID. The best alternative to attachment links
;; when these conditions are not met is to open an attachment,
;; call org-store-link, then call org-insert-link.
;; This also means attachment links could be broken if a child heading
;; is given an ID after an attachment link for a parent heading
;; attachment is created under the child heading.
;; The enabled option also adds attachments to the nearest heading with
;; an ID. When disabled, it creates an ID on the current heading.

(defun my/org-mark-ring-goto ()
  "Inspired loosely by
https://emacs.stackexchange.com/questions/31908/remove-mark-from-ring-in-org-mode"
  (interactive)
  (setq marker (car org-mark-ring))
  (if (buffer-live-p (marker-buffer marker))
      (progn
        (pop-to-buffer-same-window (marker-buffer marker))
        (goto-char marker)
        (when (or (org-invisible-p)
                  (org-invisible-p2))
          (org-show-context 'mark-goto))
        ;; Replace CAR with empty marker
        (setf (car org-mark-ring) (make-marker))
        ;; Appears to pop the CAR, then rotate the CAR to the CAR of the CDR.
        ;; I wonder if this is a feature of circular lists that popped elements
        ;; are moved instead of removed
        (pop org-mark-ring)
        marker)
    (message "No previous org-mark.")))


;; TODO Don't treat visibility cycling (?) as undo step
;; TODO M-RET makes it impossible to tab to show drawers with tab
;; (can still use org-show-subtree). Need to undo outline-flag-region
;; by org-cycle-hide-drawers within relevant portion of org-insert-heading
;; TODO Can't tab to show drawer if heading has no children
;; https://stackoverflow.com/questions/17478260/completely-hide-the-properties-drawer-in-org-mode
;; Override org-cycle-hide-drawers to completely hide property drawers on S-TAB.
;; And override org-cycle-internal-local so that TAB has a fourth state that
;; shows property drawer for the current heading
;; NOTE Re-hides drawers for children and subtree states and for global all
;; state
;;(after! org
  ;;   (defun org-cycle-hide-drawers (state)
  ;;     "Re-hide all drawers (not just their contents) after a visibility state
  ;; change."
  ;;     (when (and (derived-mode-p 'org-mode)
  ;;                (not (memq state '(overview folded contents))))
  ;;       (save-excursion
  ;;         (let* ((globalp (memq state '(all)))
  ;;                (beg (if globalp
  ;;                         ;;(point-min)
  ;;                         ;; Don't hide org-roam property before first heading.
  ;;                         ;; I'm not opposed to it, but this function only hides
  ;;                         ;; it in certain states, and tab reveal wouldn't be

  ;;                         (save-excursion
  ;;                           (goto-char (point-min))
  ;;                           (outline-next-heading)
  ;;                           (point))
  ;;                       (point)))
  ;;                (end (if globalp
  ;;                         (point-max)
  ;;                       (if (eq state 'children)
  ;;                           (org-entry-end-position)
  ;;                         (org-end-of-subtree t)))))
  ;;           (goto-char beg)
  ;;           (while (re-search-forward org-drawer-regexp end t)
  ;;             (save-excursion
  ;;               (beginning-of-line 1)
  ;;               (when (looking-at org-drawer-regexp)
  ;;                 (let* ((start (1- (match-beginning 0)))
  ;;                        (limit
  ;;                         (save-excursion
  ;;                           (outline-next-heading)
  ;;                           (point)))
  ;;                        (msg (format
  ;;                              (concat
  ;;                               "org-cycle-hide-drawers:  "
  ;;                               "`:END:`"
  ;;                               " line missing at position %s")
  ;;                              (1+ start))))
  ;;                   (if (re-search-forward "^[ \t]*:END:" limit t)
  ;;                       (outline-flag-region start (point-at-eol) t)
  ;;                     (user-error msg))))))))))

  ;; NOTE This is incompatible with +org-cycle-only-current-subtree-h
  ;; (defun org-cycle-internal-local ()
  ;;   "Do the local cycling action."
  ;;   (let ((goal-column 0) eoh eol eos has-children children-skipped struct)
  ;;     ;; First, determine end of headline (EOH), end of subtree or item
  ;;     ;; (EOS), and if item or heading has children (HAS-CHILDREN).
  ;;     (save-excursion
  ;;       (if (org-at-item-p)
  ;; 	  (progn
  ;; 	    (beginning-of-line)
  ;; 	    (setq struct (org-list-struct))
  ;; 	    (setq eoh (point-at-eol))
  ;; 	    (setq eos (org-list-get-item-end-before-blank (point) struct))
  ;; 	    (setq has-children (org-list-has-child-p (point) struct)))
  ;; 	(org-back-to-heading)
  ;; 	(setq eoh (save-excursion (outline-end-of-heading) (point)))
  ;; 	(setq eos (save-excursion
  ;; 		    (org-end-of-subtree t t)
  ;; 		    (unless (eobp) (forward-char -1))
  ;; 		    (point)))
  ;; 	(setq has-children
  ;; 	      (or
  ;; 	       (save-excursion
  ;; 		 (let ((level (funcall outline-level)))
  ;; 		   (outline-next-heading)
  ;; 		   (and (org-at-heading-p t)
  ;; 			(> (funcall outline-level) level))))
  ;; 	       (and (eq org-cycle-include-plain-lists 'integrate)
  ;; 		    (save-excursion
  ;; 		      (org-list-search-forward (org-item-beginning-re) eos t))))))
  ;;       ;; Determine end invisible part of buffer (EOL)
  ;;       (beginning-of-line 2)
  ;;       (while (and (not (eobp))		;this is like `next-line'
  ;; 		  (get-char-property (1- (point)) 'invisible))
  ;; 	(goto-char (next-single-char-property-change (point) 'invisible))
  ;; 	(and (eolp) (beginning-of-line 2)))
  ;;       (setq eol (point)))
  ;;     ;; Find out what to do next and set `this-command'
  ;;     (cond
  ;;      ((= eos eoh)
  ;;       ;; Nothing is hidden behind this heading
  ;;       (unless (org-before-first-heading-p)
  ;; 	(run-hook-with-args 'org-pre-cycle-hook 'empty))
  ;;       (org-unlogged-message "EMPTY ENTRY")
  ;;       (setq org-cycle-subtree-status nil)
  ;;       (save-excursion
  ;; 	(goto-char eos)
  ;; 	(outline-next-heading)
  ;; 	(when (org-invisible-p) (org-flag-heading nil))))
  ;;      ((and (or (>= eol eos)
  ;; 	       (not (string-match "\\S-" (buffer-substring eol eos))))
  ;; 	   (or has-children
  ;; 	       (not (setq children-skipped
  ;; 			  org-cycle-skip-children-state-if-no-children))))
  ;;       ;; Entire subtree is hidden in one line: children view
  ;;       (unless (org-before-first-heading-p)
  ;; 	(run-hook-with-args 'org-pre-cycle-hook 'children))
  ;;       (if (org-at-item-p)
  ;; 	  (org-list-set-item-visibility (point-at-bol) struct 'children)
  ;; 	(org-show-entry)
  ;; 	(org-with-limited-levels (org-show-children))
  ;; 	(org-show-set-visibility 'tree)
  ;; 	;; Fold every list in subtree to top-level items.
  ;; 	(when (eq org-cycle-include-plain-lists 'integrate)
  ;; 	  (save-excursion
  ;; 	    (org-back-to-heading)
  ;; 	    (while (org-list-search-forward (org-item-beginning-re) eos t)
  ;; 	      (beginning-of-line 1)
  ;; 	      (let* ((struct (org-list-struct))
  ;; 		     (prevs (org-list-prevs-alist struct))
  ;; 		     (end (org-list-get-bottom-point struct)))
  ;; 		(dolist (e (org-list-get-all-items (point) struct prevs))
  ;; 		  (org-list-set-item-visibility e struct 'folded))
  ;; 		(goto-char (if (< end eos) end eos)))))))
  ;;       (org-unlogged-message "CHILDREN")
  ;;       (save-excursion
  ;; 	(goto-char eos)
  ;; 	(outline-next-heading)
  ;; 	(when (org-invisible-p) (org-flag-heading nil)))
  ;;       (setq org-cycle-subtree-status 'children)
  ;;       (unless (org-before-first-heading-p)
  ;; 	(run-hook-with-args 'org-cycle-hook 'children)))
  ;;      ((or children-skipped
  ;; 	  (and (eq last-command this-command)
  ;; 	       (eq org-cycle-subtree-status 'children)))
  ;;       ;; We just showed the children, or no children are there,
  ;;       ;; now show everything.
  ;;       (unless (org-before-first-heading-p)
  ;; 	(run-hook-with-args 'org-pre-cycle-hook 'subtree))
  ;;       (org-flag-region eoh eos nil 'outline)
  ;;       (org-unlogged-message
  ;;        (if children-skipped "SUBTREE (NO CHILDREN)" "SUBTREE"))
  ;;       (setq org-cycle-subtree-status 'subtree)
  ;;       (unless (org-before-first-heading-p)
  ;; 	(run-hook-with-args 'org-cycle-hook 'subtree)))
  ;;      ;; --------- Show hidden property drawers
  ;;      ;; ((eq org-cycle-subtree-status 'subtree)
  ;;      ;;  (org-show-subtree)
  ;;      ;;  (org-unlogged-message "ALL")
  ;;      ;;  (setq org-cycle-subtree-status 'all))
  ;;      ;; ---------
  ;;      (t
  ;;       ;; Default action: hide the subtree.
  ;;       (run-hook-with-args 'org-pre-cycle-hook 'folded)
  ;;       (org-flag-region eoh eos t 'outline)
  ;;       (org-unlogged-message "FOLDED")
  ;;       (setq org-cycle-subtree-status 'folded)
  ;;       (unless (org-before-first-heading-p)
  ;;  (run-hook-with-args 'org-cycle-hook 'folded))))))


 ;; )
