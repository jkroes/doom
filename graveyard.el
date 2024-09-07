;; NOTE These aren't strictly necessary

(defun jkroes/org-statistics-count-checkbox ()
  "Force the current heading to count checkboxes rather than child headings"
  (interactive)
  (org-set-property "COOKIE_DATA" "checkbox")
  (save-excursion
    (org-back-to-heading t)
    (org-update-statistics-cookies nil)))

(defun jkroes/org-statistics-count-todo ()
  "Force the current heading to count child headings rather than checkboxes"
  (interactive)
  (org-set-property "COOKIE_DATA" "todo")
  (save-excursion
    (org-back-to-heading t)
    (org-update-statistics-cookies nil)))

;; NOTE Think twice before moving headings in and out of subtrees, because
;; doing so may change the todo state of the subtrees. If you disable the
;; advice below, you will need to manually trigger a todo state change (even
;; to the same current state) to force the subtree to update itself. I think
;; this is the safer option.

(advice-add #'org-promote :after #'jkroes/org-promote-update-statistics)
(advice-add #'org-demote :after #'jkroes/org-demote-update-statistics)

(defun jkroes/org-promote-update-statistics ()
  (org-update-statistics-cookies nil)
  (org-backward-heading-same-level 1)
  (jkroes/org-demote-update-statistics))

(defun jkroes/org-demote-update-statistics ()
  (save-excursion
    (let ((continue? t))
      (while continue?
        (org-update-statistics-cookies nil)
        (setq continue? (org-up-heading-safe))))))


;; TODO Has the org-attach window always been scrolled to the bottom? File a bug report so
;; you can remove this hack eventually.
(advice-add 'org-attach :around #'test)

(defun test (fn &rest _)
  (interactive)
  (cl-letf (((symbol-function 'org-fit-window-to-buffer)
             (lambda (&optional window max-height min-height shrink-only)
               (cond ((not (window-full-width-p window))
                      ;; Do nothing if another window would suffer.
                      )
                     ((not shrink-only)
                      (fit-window-to-buffer window max-height min-height))
                     (t (shrink-window-if-larger-than-buffer window)))
               (or window (selected-window))
               ;; HACK Display top of *Org-attach* buffers
               (prin1 "hello")
               (goto-line 1 (get-buffer "*Org Attach*")))))
    (call-interactively fn)))


;; Insert one blank line with a heading, and fold one blank line when cycling.
(setq org-blank-before-new-entry '((heading . 1) (plain-list-item))
      org-cycle-separator-lines 2)

;; Enter insert state after inserting a heading. See evil-org-mode bindings
;; to heading insertion commands
(defadvice! jkroes/org-meta-return-insert-state-a (&rest _)
  :after (list #'org-meta-return #'org-insert-subheading)
  (when (and (bound-and-true-p evil-local-mode)
             (not (evil-emacs-state-p)))
    (evil-insert 1)))

(map! :map evil-org-mode-map
      ;; Insert heading at end of current subtree. Appears to be the same
      ;; as org-insert-heading-respect-content and
      org-insert-heading-after-current.
      :ni "C-RET"    (cmd! (let ((current-prefix-arg '(4))) (call-interactively #'org-meta-return)))
      :ni [C-return] (cmd! (let ((current-prefix-arg '(4))) (call-interactively #'org-meta-return)))
      ;; Insert heading at end of parent subtree.
      :ni "C-S-RET"    (cmd! (let ((current-prefix-arg '(16))) (call-interactively #'org-meta-return)))
      :ni [C-S-return] (cmd! (let ((current-prefix-arg '(16))) (call-interactively #'org-meta-return)))
      ;; Insert heading, list item, or table row directly above or below
      ;; current heading.
      :ni "M-RET"        #'jkroes/insert-item-below
      :ni [M-return]     #'jkroes/insert-item-below
      :ni "M-S-RET"      #'jkroes/insert-item-above
      :ni [M-S-return]   #'jkroes/insert-item-above
      ;; Insert subheading
      :ni [C-M-return] #'jkroes/org-insert-subheading)

;;;###autoload
(defun jkroes/insert-item-below ()
  "Inserts a new heading, table cell or item below the current one."
  (interactive)
  (move-end-of-line nil)
  (let (org-insert-heading-respect-content)
    (org-meta-return)))

;;;###autoload
(defun jkroes/insert-item-above ()
  "Inserts a new heading, table cell or item above the current one.
If on text outside of a heading or list item, convert the text into a heading."
  (interactive)
  (move-beginning-of-line nil)
  ;; Don't insert a line above the new heading if it is the first child and is
  ;; not preceded by content
  (let ((org-blank-before-new-entry
         (unless (jkroes/org-first-child-p)
           org-blank-before-new-entry))
         org-insert-heading-respect-content)
    (org-meta-return)
    ;; HACK When `org-blank-before-new-entry' is disabled, it removes the line
    ;; before the heading, but it also removes a line after the heading. We
    ;; still want a line after the first heading
    (when (jkroes/org-first-child-p)
      (let ((cur-pos (point)))
        ;; For some reason, we need a character here in order to go back to
        ;; point later
        (insert "_")
        (newline-and-indent)
        (goto-char cur-pos)
        (delete-forward-char 1)))))

;;;###autoload
(defun jkroes/org-insert-subheading (arg)
  (interactive "P")
    (let ((org-blank-before-new-entry
           (unless (org-at-heading-p)
             org-blank-before-new-entry)))
      (org-insert-subheading arg)))

(defun jkroes/org-first-child-p ()
  "Return non-nil if the current heading is the first child heading
and is not preceded by content."
  ;; Check that a heading was inserted, not e.g. a list item or table row
  (when (org-at-heading-p)
    (save-excursion
      (org-back-to-heading t)
      (let ((cur-pos (point)))
        ;; Current heading is first child of parent
        (org-backward-heading-same-level 1)
        (when (= (point) cur-pos)
          (previous-line)
          ;; And is not separated by content, including blank lines
          (when (org-at-heading-p) t))))))
