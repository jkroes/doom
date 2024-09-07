;; NOTE Execute org-priority or press S-<up> and S-<down> to assign a priority
;; between 1 and 5
(setq org-priority-lowest 5
      org-priority-highest 1
      org-priority-default 3)

;; NOTE Per-keyword logging behavior is specified in `org-todo-keywords'. "!"
;; indicates a timestamp, "@" a timestamped note, and "/" permits different
;; behavior for state entry (LHS) and exit (RHS). The exit behavior only
;; applies when entering a state with no logging behavior. When the *Org Note*
;; buffer appears C-c C-k skips logging (but permits the state change), while
;; C-c C-c records a note only text has been inserted and a timestamp otherwise.
(setq org-log-done nil)

;; Use the LOGBOOK drawer for logging
(setq org-log-into-drawer "LOGBOOK")

;; Keywords should be reserved for task states that you want to count for
;; statistics cookies. Metadata should be implemented as tags. (If, however,
;; you want keywords that don't count for staistics, see
;; org-provide-todo-statistics, but note that you would need to dig into the
;; internals of org-not-done-keywords or
;; org-block-todo-from-children-or-siblings-or-parent to avoid blocking changes
;; in todo state for these kewords).
(setq org-todo-keywords
        '((sequence
           "TODO(t)"     ; A task that is ready to start
           "NOW(n!)"     ; An active task
           "CHOOSE(c)"
           "WAIT(w@/!)"  ; A suspended task
           "PROJ(p)"
           "IDEA(i)"
           "|"
           "CHOSEN"
           "DONE(d!/@)"    ; Task successfully completed
           "KILL(k@/@)"))) ; Task was cancelled, aborted, or is no longer applicable

(setq org-provide-todo-statistics
        '((list
           "TODO"
           "NOW"
           "CHOOSE"
           "WAIT")
           (list
            "CHOSEN"
            "DONE"
            "KILL")))

;; A list of non-done todo states excluding CHOOSE and WAIT.
(defvar jkroes/active-todo-states '("TODO" "NOW"))

;; Block switching of parent state to done until child headings or checkboxes
;; are done. This does not prevent switching parent state from done to
;; todo regardless of children state
(setq org-enforce-todo-dependencies t
      org-enforce-todo-checkbox-dependencies t)

;; When updating statistics cookies, either count the number of direct child
;; headings or the number of checkboxes recursively
(setq org-hierarchical-todo-statistics t
      org-checkbox-hierarchical-statistics nil)

;; This affects org-insert-todo-heading, org-todo, and
;; org-update-statistics-cookies. Because Doom's +org--insert-item calls
;; org-todo, it also affects +org/insert-item-above and +org/insert-item-below.
;; When `org-provide-todo-statistics' is enabled and a function that calls
;; org-todo is hooked onto org-after-todo-statistics-hook, state can be changed
;; recursively for a subtree until a parent is encountered without a statistics
;; cookie or that is not eligible to change state. The call/hook/variable
;; sequence looks like:

;; org-todo -> jkroes/toggle-statistics-cookie ->
;; org-update-parent-todo-statistics -> cookie-present ->
;; org-after-todo-statistics-hook -> org-toggle-todo -> org-todo
(advice-add #'org-update-parent-todo-statistics
            :before #'jkroes/insert-statistics-cookie)

(defun jkroes/insert-statistics-cookie (&rest _)
  (let ((state (org-get-todo-state)))
    (save-excursion
      ;; Reset checkbox state
      (cond ((equal state "CHOOSE")
             ;; (org-set-property "NOBLOCKING" "t")
             (jkroes/org-toggle-radio-keyword 'on)
             (let (org-checkbox-statistics-hook)
               (org-reset-checkbox-state-subtree)))
            ((not (member state '("CHOOSE" "CHOSEN")))
             ;; (org-delete-property "NOBLOCKING")
             (jkroes/org-toggle-radio-keyword 'off)))
      ;; Ensure a cookie is inserted so that `org-toggle-todo' can trigger
      ;; recursive state change acrosss the entire subtree.
      (when (> (org-current-level) 1)
        (org-up-heading-safe)
        ;; Don't insert a cookie if one already exists
        (let* ((cookie-re "\\[\\([0-9]*\\)/\\([0-9]*\\)\\]")
               (cookie-end (re-search-forward cookie-re (line-end-position) t)))
          (unless cookie-end
            (org-end-of-line)
            (insert " [/]")))))))

(defun jkroes/org-toggle-radio-keyword (state)
  (let ((case-fold-search t)
        (radio_keyword "#+attr_org: :radio t")
        (end (org-entry-end-position))
        (continue? t)
        line-beg line-end)
    (save-excursion
      (org-back-to-heading t)
      ;; Skip all drawers (PROPERTIES, LOGBOOK, etc.)
      (while continue?
        (unless (re-search-forward "^[ \t]*:END:[ \t]*$" end t)
          (setq continue? nil)))
      ;; Search for the first list item within the body of the current
      ;; heading. If one is not found, insert a radio keyword before the next
      ;; heading or end of the buffer.
      (unless (re-search-forward org-list-full-item-re end t)
        (outline-next-heading))
      (forward-line -1)
      (setq line-beg (line-beginning-position)
            line-end (line-end-position))
      (cond ((and (eq state 'on)
                  (not (equal (buffer-substring line-beg line-end)
                              radio_keyword)))
             (forward-line)
             (insert (string-join (list radio_keyword "\n"))))
            ((and (eq state 'off)
                  (equal (buffer-substring line-beg line-end)
                         radio_keyword))
             (delete-region line-beg line-end)
             (delete-char 1))))))

;; BUG For CHOOSE and CHOSEN headings, org-toggle-checkbox calls
;; org-toggle-radio-button, which both call org-update-checkbox-count-maybe.
;; This calls org-toggle-todo-checkboxes twice, which can lead to unexpected
;; results. This should be fixed upstream.
(advice-add #'org-toggle-radio-button :around
            (lambda (orig-fun &rest args)
              (advice-add 'org-update-checkbox-count-maybe :override #'ignore)
              (apply orig-fun args)
              (advice-remove 'org-update-checkbox-count-maybe #'ignore)))

(add-hook 'org-after-todo-statistics-hook #'jkroes/org-toggle-todo)

(defun jkroes/org-toggle-todo (n-done n-not-done)
  (let ((state (org-get-todo-state))
        ;; Only log for the subentries. Note that without this, only the
        ;; topmost heading with a state change may be logged.
        org-log-done org-todo-log-states)
    ;; TODO, NOW -> DONE
    (cond ((and (member state jkroes/active-todo-states) (= n-not-done 0))
           (org-todo "DONE"))
          ;; DONE -> TODO
          ((and (equal state "DONE") (> n-not-done 0))
           (org-todo "TODO")))))

;; Affects org-toggle-checkbox, org-toggle-radio-button, org-insert-item,
;; org-ctrl-c-ctrl-c, and org-reset-checkbox-state-subtree. NOTE
;; `org-insert-item' does not insert an item if no list item is already
;; present. A checkbox item can be created from simple text by C-c - C-u SPC m x.

(add-hook 'org-checkbox-statistics-hook #'org-toggle-todo-checkboxes)

(defun org-toggle-todo-checkboxes (&rest _)
  ;; Count must be updated before regexp matching occurs
  (org-update-checkbox-count)
  (save-excursion
    (org-back-to-heading t)
    (let* ((cookie-re "\\[\\([0-9]*\\)/\\([0-9]*\\)\\]")
           (cookie-end (re-search-forward cookie-re (line-end-position) t))
           (cookie-beginning (when cookie-end (match-beginning 0)))
           (numerator (when cookie-end (string-to-number (match-string 1))))
           (denominator (when cookie-end (string-to-number (match-string 2))))
           (state (org-get-todo-state)))
      (cond ((not cookie-end)
             (org-end-of-line)
             (insert " [/]")
             (org-toggle-todo-checkboxes))
            ;; CHOOSE -> CHOSEN
            ((and (equal state "CHOOSE")
                  (= numerator 1))
             ;; See the definition of `org-enforce-todo-checkbox-dependencies'.
             ;; This is like setting the property NOBLOCKING for the current
             ;; heading.
             (let ((org-blocker-hook
                    (remove #'org-block-todo-from-checkboxes
                            org-blocker-hook)))
               (org-todo "CHOSEN")))
            ;; CHOSEN -> CHOOSE
            ((and (equal state "CHOSEN")
                  (= numerator 0)
                  (eq this-command #'org-toggle-checkbox))
             (org-todo "CHOOSE"))
            ;; TODO, NOW -> DONE
            ((and (member state jkroes/active-todo-states)
                  (= numerator denominator))
             (org-todo "DONE"))
            ;; DONE -> TODO
            ((and (equal state "DONE")
                  (not (= numerator denominator)))
             (org-todo "TODO"))))))

;; I already call org-update-checkbox-count within `org-toggle-todo-checkboxes'
(after! org-list (setcdr (assoc 'checkbox org-list-automatic-rules) nil))
