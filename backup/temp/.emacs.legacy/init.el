;; NOTE This is more or less equivalent to use-package-with-elapsed-timer's
;; reporting of how long a use-package body takes to complete. Wrap any lisp
;; code with it to profile that code
(defmacro with-timer (name &rest body)
  "Measure the runtime of a code block"
  `(let ((time (current-time)))
     ,@body
     ;; This is undocumented, but you can use nil instead of calling
     ;; `current-time' as the first arg to `time-subtract'
     (message "%s: %g" ,name (float-time (time-subtract nil time)))))
;; Example usage:
;; (with-timer "mode-line-setup"
;;   (set-up-mode-line))

(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(add-to-list 'load-path (concat user-emacs-directory "lisp/functions"))

(set-face-background 'glyphless-char "red")
(set-char-table-range glyphless-char-display
                      (char-from-name "ZERO WIDTH SPACE") 'thin-space)

;; TODO Configure projectile
;; TODO Switch away from doom-project-root. It doesn't work with recent
;; consult-project-function, so I am already using it selectively

(setq projectile-project-search-path
      (list (concat user-emacs-directory "straight/repos"))
      projectile-switch-project-action #'projectile-dired)
(straight-use-package 'projectile)
(projectile-mode)


(straight-use-package 'vundo)
;; TODO Some undo steps per e.g. vundo appear to do nothing to the buffer. I suspect this has to do with differences
;; in fontification only. It might be worth stripping text properties from text within buffer-undo-list and seeing
;; whether this fixes the issue. org-appear, org-src, and org-visual-outline might be relevant sources for such
;; changes. Or it might have to do with position when editing with org-src. evil state changes? Who really knows?
;; TODO On the other hand, I do want undo boundaries between changes in distant locations, so that I can see all
;; the changes as I undo them.
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Undo.html
;; (defadvice! no-undo-composition (func &rest args)
;;   :around #'compose-region
;;   (let ((buffer-undo-list nil))
;;     (apply func args)))

;; Use daily note as default capture target (see +org-capture-notes-file, but set dynamically each day.
;; +org-capture-journal-file?)
;; TODO Modify org-return to follow links, insert heading (when no other org object is present), insert list items,
;; org-edit-src-code, etc. C-j can be used for return in normal state.

  ;;; general

(setq
      ;; TODO This is an ad-hoc list I created and should be modified with time
      org-structure-template-alist '(("e" . "src emacs-lisp ")
                                     ("r" . "src R :tangle no")
                                     ("p" . "src python :tangle no")
                                     ("q" . "quote")
                                     ("x" . "example"))
      org-list-allow-alphabetical t
      org-M-RET-may-split-line nil
      ;; See org-yank and org-paste
      org-yank-adjusted-subtrees t
      org-yank-folded-subtrees t)

;; Some uses of `org-fix-tags-on-the-fly' occur without a check on
;; `org-auto-align-tags', such as in `org-self-insert-command' and
;; `org-delete-backward-char'.
;; TODO Should be reported/PR'ed upstream
(defadvice! +org--respect-org-auto-align-tags-a (&rest _)
  :before-while #'org-fix-tags-on-the-fly
  org-auto-align-tags)

;; The org-transclude manual recommends disabling this
;; (defadvice! +org--recenter-after-follow-link-a (&rest _args)
;;   "Recenter after following a link, but only internal or file links."
;;   :after '(org-footnote-action
;;            org-follow-timestamp-link
;;            org-link-open-as-file
;;            org-link-search)
;;   (when (get-buffer-window)
;;     (recenter)))

;; HACK With https://code.orgmode.org/bzg/org-mode/commit/48da60f4, inline
;;      image previews broke for users with imagemagick support built in. This
;;      reverses the problem, but should be removed once it is addressed
;;      upstream (if ever).
;; (defadvice! +org--fix-inline-images-for-imagemagick-users-a (fn &rest args)
;;   :around #'org-display-inline-images
;;   (letf! (defun create-image (file-or-data &optional type data-p &rest props)
;;            (let ((type (if (plist-get props :width) type)))
;;              (apply create-image file-or-data type data-p props)))
;;          (apply fn args)))

;; (defadvice! +org--fix-inconsistent-uuidgen-case-a (uuid)
;;   "Ensure uuidgen always produces lowercase output regardless of system."
;;   :filter-return #'org-id-new
;;   (if (eq org-id-method 'uuid)
;;       (downcase uuid)
;;     uuid))

  ;;; agenda

(setq org-agenda-files (list org-directory)
      ;; Faces for approaching and past-due deadlines in the agenda
      org-agenda-deadline-faces '((1.001 . error)
                                  (1.0 . org-warning)
                                  (0.5 . org-upcoming-deadline)
                                  (0.0 . org-upcoming-distant-deadline))
      org-agenda-tags-column 'auto
      org-agenda-restore-windows-after-quit t
      org-agenda-window-setup 'current-window
      org-agenda-skip-unavailable-files t
      ;; Show the previous 3 and next 7 days
      org-agenda-start-on-weekday nil
      org-agenda-start-day "-3d"
      org-agenda-span 10
      ;; Optimize `org-agenda' by inhibiting extra work while opening agenda
      ;; buffers in the background. They'll be "restarted" if the user switches to
      ;; them anyway (see `+org-exclude-agenda-buffers-from-workspace-h')
      org-agenda-inhibit-startup t)

(add-hook 'org-agenda-finalize-hook
          (defun +org-defer-mode-in-agenda-buffers-h ()
            "`org-agenda' opens temporary, incomplete org-mode buffers.  I've
    disabled a lot of org-mode's startup processes for these invisible buffers to
    speed them up (in `+org--optimize-backgrounded-agenda-buffers-a'). However, if
    the user tries to visit one of these buffers they'll see a gimped, half-broken
    org buffer. To avoid that, restart `org-mode' when they're switched to so they
    can grow up to be fully-fledged org-mode buffers."
            (dolist (buffer org-agenda-new-buffers)
              (when (buffer-live-p buffer)      ; Ensure buffer is not killed
                (with-current-buffer buffer
                  (add-hook 'doom-switch-buffer-hook #'+org--restart-mode-h
                            nil 'local))))))

(defun +org--restart-mode-h ()
  "Restart `org-mode', but only once."
  (quiet! (org-mode-restart))
  (delq! (current-buffer) org-agenda-new-buffers)
  (remove-hook 'doom-switch-buffer-hook #'+org--restart-mode-h
               'local)
  (run-hooks 'find-file-hook))

(defvar recentf-exclude)
(defadvice! +org--optimize-backgrounded-agenda-buffers-a (fn file)
  "Prevent temporarily opened agenda buffers from polluting recentf."
  :around #'org-get-agenda-file-buffer
  (let ((recentf-exclude (list (lambda (_file) t)))
        (doom-inhibit-large-file-detection t)
        org-startup-indented
        org-startup-folded
        vc-handled-backends
        org-mode-hook
        find-file-hook)
    (funcall fn file)))

  ;;; appearance

(setq org-hide-block-startup t
      ;; org-blank-before-new-entry '((heading) (plain-list-item))
      org-indirect-buffer-display 'current-window
      org-eldoc-breadcrumb-separator " → "
      org-fontify-done-headline t
      org-fontify-quote-and-verse-blocks t
      org-fontify-whole-heading-line t
      ;; org-indent-mode-turn-off-org-adapt-indentation does not nullify this
      ;; option when its value is 'headline-data. In this case, org property
      ;; drawers are overindented
      org-adapt-indentation nil
      org-tags-column 0
      ;; `showeverything' is org's default, but it doesn't respect
      ;; `org-hide-block-startup' (#+startup: hideblocks), archive trees,
      ;; hidden drawers, or VISIBILITY properties. `nil' is equivalent, but
      ;; respects these settings.
      org-startup-folded nil
      org-hide-emphasis-markers t
      org-link-descriptive t
      org-pretty-entities t
      org-use-sub-superscripts '{}
      org-pretty-entities-include-sub-superscripts t)

;; (defadvice! +org--strip-properties-from-outline-a (fn &rest args)
;;   "Fix variable height faces in eldoc breadcrumbs."
;;   :around #'org-format-outline-path
;;   (let ((org-level-faces
;;          (cl-loop for face in org-level-faces
;;                   collect `(:foreground ,(face-foreground face nil t)
;;                                         :weight bold))))
;;     (apply fn args)))

;; (with-eval-after-load 'org-eldoc
;;   ;; HACK Fix #2972: infinite recursion when eldoc kicks in in 'org' or
;;   ;;      'python' src blocks.
;;   ;; TODO Should be reported upstream!
;;   (puthash "org" #'ignore org-eldoc-local-functions-cache)
;;   (puthash "plantuml" #'ignore org-eldoc-local-functions-cache)
;;   (puthash "python" #'python-eldoc-function org-eldoc-local-functions-cache))

  ;;; refile

(setq org-refile-targets '((nil :maxlevel . 3)
                           (org-agenda-files :maxlevel . 3))
      ;; See the vertico README
      org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil)

  ;;; log

(setq org-log-into-drawer nil
      org-log-done 'time
      org-log-done-with-time t
      org-log-redeadline 'note
      org-log-reschedule 'note)

  ;;; todo

;; HACK Face specs fed directly to `org-todo-keyword-faces' don't respect
;;      underlying faces like the `org-todo' face does, so we define our own
;;      intermediary faces that extend from org-todo.
(with-no-warnings
  (custom-declare-face
   '+org-todo-active
   '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
  (custom-declare-face
   '+org-todo-project
   '((t (:inherit (bold font-lock-doc-face org-todo)))) "")
  (custom-declare-face
   '+org-todo-onhold
   '((t (:inherit (bold warning org-todo)))) "")
  (custom-declare-face
   '+org-todo-cancel
   '((t (:inherit (bold error org-todo)))) ""))

(setq org-enforce-todo-dependencies t
      org-use-fast-todo-selection 'auto
      org-todo-keywords '((sequence
                           "TODO(t)"  ; A task that needs doing & is ready to do
                           "PROJ(p)"  ; A project, which usually contains other tasks
                           "LOOP(r)"  ; A recurring task
                           "STRT(s)"  ; A task that is in progress
                           "WAIT(w)"  ; Something external is holding up this task
                           "HOLD(h)"  ; This task is paused/on hold because of me
                           "IDEA(i)"  ; An unconfirmed and unapproved task or notion
                           "|"
                           "DONE(d)"  ; Task successfully completed
                           "KILL(k)") ; Task was cancelled, aborted or is no longer applicable
                          (sequence
                           "[ ](T)"   ; A task that needs doing
                           "[-](S)"   ; Task is in progress
                           "[?](W)"   ; Task is being held up or paused
                           "|"
                           "[X](D)")  ; Task was completed
                          (sequence
                           "|"
                           "OKAY(o)"
                           "YES(y)"
                           "NO(n)"))
      org-todo-keyword-faces '(("[-]"  . +org-todo-active)
                               ("STRT" . +org-todo-active)
                               ("[?]"  . +org-todo-onhold)
                               ("WAIT" . +org-todo-onhold)
                               ("HOLD" . +org-todo-onhold)
                               ("PROJ" . +org-todo-project)
                               ("NO"   . +org-todo-cancel)
                               ("KILL" . +org-todo-cancel))
      org-priority-faces '((?A . error)
                           (?B . warning)
                           (?C . success)))


  ;;; links

;; See also org-link-elisp-skip-confirm-regexp and
;; https://www.reddit.com/r/emacs/comments/uenjjs/link_to_a_git_commit_from_org_mode_using_magit/
(setq org-link-elisp-confirm-function nil
      org-return-follows-link t
      ;; If needed, change this. It should allow you to resolve links within
      ;; ~/ across operating systems though...
      org-link-file-path-type 'absolute)

;; Open help:* links with helpful-* instead of describe-*
(advice-add #'org-link--open-help :around #'doom-use-helpful-a)

(with-eval-after-load "ol"
  (setf (alist-get 'file org-link-frame-setup) #'find-file)

  ;; Modify default file: links to colorize broken file links red
  (org-link-set-parameters
   "file"
   :face (lambda (path)
           (if (or (file-remote-p path)
                   ;; filter out network shares on windows (slow)
                   (and IS-WINDOWS (string-prefix-p "\\\\" path))
                   (file-exists-p path))
               'org-link
             '(error org-link))))

  ;; Add "lookup" links for keystrings
  (org-link-set-parameters
   "kbd"
   :follow (lambda (_) (minibuffer-message "%s" (+org-display-link-in-eldoc-a)))
   :help-echo #'+org-read-kbd-at-point
   :face 'help-key-binding)

  ;; Allow inline image previews of http(s) urls to image files and
  ;; attachment links
  ;; `+org-http-image-data-fn' will respect `org-display-remote-inline-images'.
  (setq org-display-remote-inline-images 'download) ; TRAMP urls
  (org-link-set-parameters "http"  :image-data-fun #'+org-http-image-data-fn)
  (org-link-set-parameters "https" :image-data-fun #'+org-http-image-data-fn)
  (org-link-set-parameters "attachment" :image-data-fun #'+org-inline-image-data-fn)

  ;; Shouldn't be necessary b/c we can use org-roam completion and
  ;; org-roam-directory should be equiv. to org-roam-directory.
  ;; But we could do this for other directories as a shorthand
  ;; form of `file:' link for commonly used dirs
  ;;(+org-define-basic-link "org" 'org-directory)
  )

(defadvice! +org-display-link-in-eldoc-a (&rest _)
  "Display full link in minibuffer when cursor/mouse is over it."
  :before-until #'org-eldoc-documentation-function
  (when-let* ((context (org-element-context))
              (path (org-element-property :path context)))
    (pcase (org-element-property :type context)
      ("kbd"
       (format "%s %s"
               (propertize "Key sequence:" 'face 'bold)
               (propertize (+org-read-kbd-at-point path context)
                           'face 'help-key-binding)))
      (type (format "Link: %s" (org-element-property :raw-link context))))))


;; TODO Use this as a model for how to alter org-git-link.el
;; to allow for line-search syntax (since it's search syntax
;; is used for commits). Alternatively, use github links if you
;; think the repo won't be available locally.
;; OR YOU CAN USE SOMETHING LIKE THIS:
;; [[elisp:(magit-find-file "b15c81f7766a89" "/Users/jkroes/.emacs.legacy/straight/repos/consult/consult.el")]]
;; with more details at https://www.reddit.com/r/emacs/comments/uenjjs/link_to_a_git_commit_from_org_mode_using_magit/
(defadvice! +org--follow-search-string-a (fn link &optional arg)
  "Support ::SEARCH syntax for id: links."
  :around #'org-id-open
  :around #'org-roam-id-open
  (save-match-data
    (cl-destructuring-bind (id &optional search)
        (split-string link "::")
      (prog1 (funcall fn id arg)
        (cond ((null search))
              ((string-match-p "\\`[0-9]+\\'" search)
               ;; Move N lines after the ID (in case it's a heading), instead
               ;; of the start of the buffer.
               (forward-line (string-to-number option)))
              ((string-match "^/\\([^/]+\\)/$" search)
               (let ((match (match-string 1 search)))
                 (save-excursion (org-link-search search))
                 ;; `org-link-search' only reveals matches. Moving the point
                 ;; to the first match after point is a sensible change.
                 (when (re-search-forward match)
                   (goto-char (match-beginning 0)))))
              ((org-link-search search)))))))

(defun org-store-link-to-filepath (arg)
  "I use this to grab the filepath of org files without context about the line
  where this is called in the link and without an ID (being created). Also grabs
  other files without context."
  (interactive "P")
  (let ((org-link-context-for-files nil)
        major-mode)
    ;; No way to store a link to an org-mode file without an ID, either
    ;; preexisting or created anew. We only want the filepath in the link, which
    ;; we can get if we mask the `major-mode' from `derived-mode-p' within
    ;; `org-store-link'.
    (and (derived-mode-p 'org-mode)
         (setq major-mode 'text-mode))
    ;; Need to call with `interactive?' set to `t' to store link for
    ;; `org-insert-link'
    (org-store-link nil t))
  ;; Without this, the description portion of the newly created element of
  ;; org-stored-links will be the same as the filepath and avoids the
  ;; org-insert-link prompt to enter or accept the description text. Instead,
  ;; the link is inserted without a description. (Some users may prefer this.)
  ;; Any non-nil description value that doesn't match the link portion will pull
  ;; up the prompt with the value as a suggestion.
  (let ((desc (cdr (nth 0 org-stored-links))))
    (setcar desc (if (equal arg '(4))
                     nil
                   (file-name-nondirectory (car desc))))))

(defun compress-org-link (arg)
  (interactive "P")
  (let ((url (thing-at-point 'url))
        (bounds (bounds-of-thing-at-point 'url)))
    (kill-region (car bounds) (cdr bounds))
    (insert
     (format "[[%s][%s]]"
             url
             (truncate-string-to-width url
                                       (if arg (prefix-numeric-value arg) 40)
                                       nil nil "...")))))

  ;;; images

(setq org-image-actual-width nil)

;; Refresh inline images after executing src blocks (useful for plantuml or
;; ipython, where the result could be an image)
;; (add-hook 'org-babel-after-execute-hook
;;            (defun +org-redisplay-inline-images-in-babel-result-h ()
;;              (unless (or
;;                       ;; ...but not while Emacs is exporting an org buffer (where
;;                       ;; `org-display-inline-images' can be awfully slow).
;;                       (bound-and-true-p org-export-current-backend)
;;                       ;; ...and not while tangling org buffers (which happens in a temp
;;                       ;; buffer where `buffer-file-name' is nil).
;;                       (string-match-p "^ \\*temp" (buffer-name)))
;;                (save-excursion
;;                  (when-let ((beg (org-babel-where-is-src-block-result))
;;                             (end (progn (goto-char beg) (forward-line) (org-babel-result-end))))
;;                    (org-display-inline-images nil nil (min beg end) (max beg end)))))))

  ;;; src / babel

;; TODO Add babel- and export-related code from Doom's lang/org module if you
;; begin executing or exporting src code
(setq org-src-preserve-indentation t
      ;; org-src-tab-acts-natively t
      org-src-window-setup 'current-window
      org-src-ask-before-returning-to-edit-buffer nil)

(defun my/org-edit-src-save-and-exit ()
  (interactive)
  (org-edit-src-save)
  (org-edit-src-exit)
  ;; Prevents accidental text insertion
  (evil-normal-state))

  ;;; capture

;; Kill capture buffers by default (unless they've been visited)
;; (with-eval-after-load 'org-capture (org-capture-put :kill-buffer t))

;; Fix #462: when refiling from org-capture, Emacs prompts to kill the
;; underlying, modified buffer. This fixes that.
;;(add-hook 'org-after-refile-insert-hook #'save-buffer)

;; (defadvice! +org--capture-expand-variable-file-a (file)
;;   "If a variable is used for a file path in `org-capture-template', it is used
;; as is, and expanded relative to `default-directory'. This changes it to be
;; relative to `org-directory', unless it is an absolute path."
;;   :filter-args #'org-capture-expand-file
;;   (if (and (symbolp file) (boundp file))
;;       (expand-file-name (symbol-value file) org-directory)
;;     file))
;; (add-hook 'org-capture-mode-hook
;;            (defun +org-show-target-in-capture-header-h ()
;;              (setq header-line-format
;;                    (format "%s%s%s"
;;                            (propertize (abbreviate-file-name (buffer-file-name (buffer-base-buffer)))
;;                                        'face 'font-lock-string-face)
;;                            org-eldoc-breadcrumb-separator
;;                            header-line-format))))

  ;;; attachments

;; TODO Bind org-attach-dired-to-subtree and other attachment functions. See
;; https://orgmode.org/manual/Attach-from-Dired.html
;; TODO Alter inheritance so it only affects link resolution and allows nested
;; headings with attachments. Possibly advice around org-attach-attach (and
;; other functions?) that uses a let binding setting org-attach-use-inheritance
;; to nil? Will inheritance only search for the nearest parent, or will it try
;; to resolve links through all parents? THe latter behavior is what we want...
;; TODO Searching attachments in all org files? You could probably just use
;; vertico to display all files within org-attach-id-dir. Alternatively org-ql?
;; This might also provide a mechanism for inserting absolute-path attachment
;; links anywhere you want, avoiding the need for recursion. You could also
;; design a function that limits candidates to the current file or current
;; subtree, avoiding inheritance and relative links altogether.  ALternatively,
;; you could use org-link-insert, attachment, then insert absolute paths
;; TODO Slim down org-attach-commands
;; TODO Configure attachment link completion
;; TODO org-attach.el only autoloads org-attach and org-attach-dired-to-subtree,
;; so add the rest of its commands

(setq org-attach-store-link-p t     ; store link after attaching files
      org-attach-use-inheritance t ; inherit properties from parent nodes
      org-attach-id-dir (expand-file-name ".attach/" org-directory))

(with-eval-after-load 'projectile
  (add-to-list 'projectile-globally-ignored-directories org-attach-id-dir))

  ;;; blocks

(add-hook 'org-mode-hook 'org-blocks-hidden-initialize)
(add-hook 'org-cycle-hook 'org-cycle-blocks)

(defun org-blocks-hidden-initialize ()
  (setq-local org-blocks-hidden org-hide-block-startup))

(defun org-cycle-blocks (cycle-state)
  "Make org-cycle respect the value of org-blocks-hidden (set by
  org-toggle-blocks)"
  (if (memq cycle-state '(all subtree))
      (if org-blocks-hidden
          (org-hide-block-all)
        (org-show-block-all))))

(defun org-toggle-blocks ()
  "Toggle block visibility on or off."
  (interactive)
  (if org-blocks-hidden
      (org-show-block-all)
    (org-hide-block-all))
  (setq org-blocks-hidden (not org-blocks-hidden)))

  ;;; org

(straight-use-package
 '(org :host github :repo "bzg/org-mode" :local-repo "org" :depth full
       :pre-build (straight-recipes-org-elpa--build) :build (:not autoloads)
       :files (:defaults "lisp/*.el" ("etc/styles/" "etc/styles/*"))))

  ;;; archived code

;; (defun my/org-open-at-point (&optional arg)
;;   "Modified to distinguish between headings with attached directories and
;;   files, and to open directories as normal for the original
;;   function--org-attach-reveal or org-attach-reveal-in-emacs--but
;;   to use org-attach-open or org-attach-open-in-emacs for attached files (i.e.,
;;   attachments with the ID property)."
;;   (interactive "P")
;;   (org-load-modules-maybe)
;;   (setq org-window-config-before-follow-link (current-window-configuration))
;;   (org-remove-occur-highlights nil nil t)
;;   (unless (run-hook-with-args-until-success 'org-open-at-point-functions)
;;     (let* ((context
;;             ;; Only consider supported types, even if they are not the
;;             ;; closest one.
;;             (org-element-lineage
;;              (org-element-context)
;;              '(clock comment comment-block footnote-definition
;;                footnote-reference headline inline-src-block inlinetask
;;                keyword link node-property planning src-block timestamp)
;;              t))
;;            (type (org-element-type context))
;;            (value (org-element-property :value context))
;;            (my/id (org-element-property :ID context)))
;;       (cond
;;        ((not type) (user-error "No link found"))
;;        ;; No valid link at point.  For convenience, look if something
;;        ;; looks like a link under point in some specific places.
;;        ((memq type '(comment comment-block node-property keyword))
;;         (call-interactively #'org-open-at-point-global))
;;        ;; On a headline or an inlinetask, but not on a timestamp,
;;        ;; a link, a footnote reference.
;;        ((memq type '(headline inlinetask))
;;         (org-match-line org-complex-heading-regexp)
;;         (if (and (match-beginning 5)
;;                  (>= (point) (match-beginning 5))
;;                  (< (point) (match-end 5)))
;;             ;; On tags.
;;             (org-tags-view
;;              arg
;;              (save-excursion
;;                (let* ((beg (match-beginning 5))
;;                       (end (match-end 5))
;;                       (beg-tag (or (search-backward ":" beg 'at-limit) (point)))
;;                       (end-tag (search-forward ":" end nil 2)))
;;                  (buffer-substring (1+ beg-tag) (1- end-tag)))))
;;           ;; Not on tags.
;;           (pcase (org-offer-links-in-entry (current-buffer) (point) arg)
;;             (`(nil . ,_)
;;              (require 'org-attach)
;;              (if my/id
;;                  (progn
;;                    (message "Opening attachment-file")
;;                    (if (equal arg '(4))
;;                        (org-attach-open-in-emacs)
;;                      (org-attach-open)))
;;                (message "Opening attachment-dir")
;;                (if (equal arg '(4))
;;                    (my/org-attach-reveal-in-emacs)
;;                  (org-attach-reveal))))
;;             (`(,links . ,links-end)
;;              (dolist (link (if (stringp links) (list links) links))
;;                (search-forward link nil links-end)
;;                (goto-char (match-beginning 0))
;;                (org-open-at-point arg))))))
;;        ;; On a footnote reference or at definition's label.
;;        ((or (eq type 'footnote-reference)
;;             (and (eq type 'footnote-definition)
;;                  (save-excursion
;;                    ;; Do not validate action when point is on the
;;                    ;; spaces right after the footnote label, in order
;;                    ;; to be on par with behavior on links.
;;                    (skip-chars-forward " \t")
;;                    (let ((begin
;;                           (org-element-property :contents-begin context)))
;;                      (if begin (< (point) begin)
;;                        (= (org-element-property :post-affiliated context)
;;                           (line-beginning-position)))))))
;;         (org-footnote-action))
;;        ;; On a planning line.  Check if we are really on a timestamp.
;;        ((and (eq type 'planning)
;;              (org-in-regexp org-ts-regexp-both nil t))
;;         (org-follow-timestamp-link))
;;        ;; On a clock line, make sure point is on the timestamp
;;        ;; before opening it.
;;        ((and (eq type 'clock)
;;              value
;;              (>= (point) (org-element-property :begin value))
;;              (<= (point) (org-element-property :end value)))
;;         (org-follow-timestamp-link))
;;        ((eq type 'src-block) (org-babel-open-src-block-result))
;;        ;; Do nothing on white spaces after an object.
;;        ((>= (point)
;;             (save-excursion
;;               (goto-char (org-element-property :end context))
;;               (skip-chars-backward " \t")
;;               (point)))
;;         (user-error "No link found"))
;;        ((eq type 'inline-src-block) (org-babel-open-src-block-result))
;;        ((eq type 'timestamp) (org-follow-timestamp-link))
;;        ((eq type 'link) (org-link-open context arg))
;;        (t (user-error "No link found")))))
;;   (run-hook-with-args 'org-follow-link-hook))
;;
;; ;; TODO make this exit even if you C-g and abort the command
;; (defun my/org-attach-reveal-in-emacs ()
;;   "Show the attachment directory of the current outline node in deer.
;;   Will create an attachment and folder if it doesn't exist yet.
;;   Respects `org-attach-preferred-new-method'."
;;   (interactive)
;;   (deer (org-attach-dir-get-create)))

;; (defun my/org-open-at-point-in-emacs ()
;;   "Make org-open-at-point open attachments in Emacs"
;;   (interactive)
;;   (my/org-open-at-point '(4))) ; C-u org-open-at-point

;; ;; Reverses my/org-open-at-point-in-emacs (linear link traversal)
;; ;; Original inspiration loosely based on
;; ;; https://emacs.stackexchange.com/questions/31908/remove-mark-from-ring-in-org-mode
;; (defun my/org-mark-ring-goto ()
;;   (interactive)
;;   (setq marker (car org-mark-ring))
;;   (if (buffer-live-p (marker-buffer marker))
;;       (progn
;;         (pop-to-buffer-same-window (marker-buffer marker))
;;         (goto-char marker)
;;         (when (or (org-invisible-p)
;;                   (org-invisible-p2))
;;           (org-show-context 'mark-goto))
;;         ;; Replace CAR with empty marker
;;         (setf (car org-mark-ring) (make-marker))
;;         ;; Appears to pop the CAR, then rotate the CAR to the CAR of the CDR.
;;         ;; I wonder if this is a feature of circular lists that popped elements
;;         ;; are moved instead of removed
;;         (pop org-mark-ring)
;;         marker)
;;     (message "No previous org-mark.")))

;; (defun my/counsel-org-agenda-headlines ()
;;   "Like counsel-org-agenda-headlines but additionally exits org-agenda-files buffers
;; opened by this command, to avoid polluting counsel-org-goto-all results."
;;   (interactive)
;;   (counsel-org-agenda-headlines)
;;   (let ((f (buffer-file-name (current-buffer))))
;;     ;; Taken from org-agenda-exit, which can't be called directly
;;     (org-release-buffers org-agenda-new-buffers)
;;     (find-file f)))

;; (defun my-org-custom-sparse-tree (todo-only match &optional
;;                                             match-body parents-body)
;;   "Create a custom sparse tree that only shows matched headings and parents.
;; For TODO-ONLY and MATCH see `org-match-sparse-tree'.
;; If MATCH-BODY is non-nil the bodies of the matches are shown.
;; If PARENTS-BODY is non-nil the bodies of the parents are shown."
;;   ;; Create the sparse tree.
;;   (org-match-sparse-tree todo-only match)
;;   (let ((pt-first (save-excursion
;;                     (org-first-headline-recenter)
;;                     (point)))
;;         (hls org-occur-highlights))
;;     ;; Hide everything.
;;     (outline-flag-region pt-first (point-max) t)
;;     ;; For each occur highlight overlay (the matches).
;;     (dolist (hl hls)
;;       (save-excursion
;;         (goto-char (overlay-start hl))
;;         ;; Unhide match.
;;         (outline-show-heading)
;;         (when match-body (outline-show-entry))
;;         ;; Unhide parents.
;;         (while (org-up-heading-safe)
;;           (outline-show-heading)
;;           (when parents-body (outline-show-entry))))))
;;   ;; Hide all archived subtrees again.
;;   (org-hide-archived-subtrees (point-min) (point-max)))

;; (server-start) ; Route emacsclient calls to this Emacs instance
;; (if (not (and (boundp 'server-process)
;;               (processp 'server-process)
;;               (server-running-p)))
;;     (message "----- Initiating emacs server -----"))

;; (load "server-extensions")

;; (when (featurep 'ns)
;;   (defun ns-raise-emacs ()
;;     "Raise Emacs."
;;     (ns-do-applescript "tell application \"Emacs\" to activate"))

;;   (defun ns-raise-emacs-with-frame (frame)
;;     "Raise Emacs and select the provided frame."
;;     (with-selected-frame frame
;;       (when (display-graphic-p)
;;         (ns-raise-emacs))))

;;   (add-hook 'after-make-frame-functions 'ns-raise-emacs-with-frame)

;;   (when (display-graphic-p)
;;     (ns-raise-emacs)))

(setq-default fill-column 80)

;; Use auto-fill for all major modes.
;; Source: info:efaq#Turning on auto-fill by default
(setq-default auto-fill-function 'do-auto-fill)
;; (add-hook 'prog-mode-hook 'turn-on-auto-fill) ; (derived-mode-p 'prog-mode)

(add-hook 'custom-mode-hook 'no-auto)
(add-hook 'markdown-mode-hook 'no-auto+visual)
;; (add-hook 'term-mode-hook 'no-auto+truncate)
(add-hook 'backtrace-mode-hook 'no-auto+visual)
;; (add-hook 'emacs-lisp-mode-hook 'no-auto+truncate)
(add-hook 'org-mode-hook 'no-auto+visual)
;; Untested
(if (not (memq 'no-auto+visual org-mode-hook))
    (progn
      ;; Emphasis markers can span 10 lines.
      (setcar (nthcdr 4 org-emphasis-regexp-components) 10)
      (org-set-emph-re 'org-emphasis-regexp-components
                       org-emphasis-regexp-components)))

(defun no-auto (&optional wrap-mode)
  "Disable auto fill and indicator for specific modes"
  (turn-off-auto-fill) ; (auto-fill-mode -1)
  (display-fill-column-indicator-mode -1)
  (if wrap-mode
      (funcall wrap-mode)))

(defun no-auto+visual ()
  (no-auto 'visual-line-mode))

(defun no-auto+truncate ()
  (no-auto)
  (toggle-truncate-lines 1))

(straight-use-package 'org-contrib)

(add-hook 'org-mode-hook 'org-eldoc-load)

;; (straight-use-package 'ivy-bibtex) ; Depends on ivy and bibtex-completion
;; (autoload 'ivy-bibtex "ivy-bibtex" "" t)

;; ;; Open note instead of URL or PDF in ivy by default (RET). You can still
;; ;; open PDFs in docview (pdf-tools if available) with SPC o b M-o p
;; (setq ivy-bibtex-default-action 'ivy-bibtex-edit-notes)

;; ;; ivy-bibtex requires ivy's `ivy--regex-ignore-order` regex builder, which
;; ;; ignores the order of regexp tokens when searching for matching candidates.
;; (add-to-list 'ivy-re-builders-alist '(ivy-bibtex . ivy--regex-ignore-order))

;; ;; Depends on bibtex-completion, pdf-tools, ivy, and helm-bibtex
;; (straight-use-package 'org-ref)

;; (setq bibtex-completion-pdf-field "File"
;;       bibtex-completion-bibliography (list (concat user-emacs-directory "Reorganized.bib"))
;;       ;; Only supports a limited subset of fields in a bibtex file for
;;       ;; ivy-bibtex. TODO Modify internals to allow any field in biblatex
;;       ;; to be used. A good candidate is bibtex-completion-format-entry and
;;       ;; ivy-bibtex-display-transformer
;;       bibtex-completion-display-formats
;;       '((t . "${title:*} ${=type=:10} ${=has-pdf=:1} ${=has-note=:1} "))
;;       ;; Needed to avoid warning messages from org-ref when using ivy-bibtex
;;       org-ref-default-bibliography bibtex-completion-bibliography
;;       ;; Needed to avoid warnings (in an *org-ref* buffer) about invalid
;;       ;; bibtex entry types when using ivy-bibtex
;;       bibtex-dialect "biblatex")

;; ;; org-ref has to be loaded or ivy-bibtex with the selected action set to
;; ;; editing the note will simply create a new note every time.
;; (require 'org-ref)

;; ;; Depends on org-ref, bibtex-completion, and org-roam
;; ;; org-ref depends on bibtex-completion, pdf-tools, hydra,ivy, helm-bibtex,
;; ;; and helm (but not ivy and ivy-bibtex built from the helm-bibtex repo). Note
;; ;; that org-ref has its own ivy frontend, org-ref-ivy-cite.
;; (straight-use-package 'org-roam-bibtex)

;; (setq orb-preformat-keywords
;;       '("citekey" "title" "url" "author-or-editor" "keywords" "file")
;;       orb-process-file-keyword t
;;       orb-file-field-extensions '("pdf"))

;; ;; Manage bib notes using org-roam and access the notes in org-roam-directory
;; ;; via ivy-bibtex or by opening org-ref's `cite:' links
;; (org-roam-bibtex-mode)

;; ;; Integrate org-roam-bibtex and org-noter based on modification to the
;; ;; code provided in the orb manual. Per orb--new-note, the only way to avoid
;; ;; template selection is for there to be a single element in
;; ;; org-roam-capture-templates. ivy-bibtex will now only use the template defined
;; ;; here.
;; (defun orb-override-org-roam-capture-templates (old-func &rest args)
;;   (let ((org-roam-capture-templates
;;          '(("b" "bibtex" plain
;;             ;; The template file to use
;;             (file (concat user-emacs-directory "bibtex-template.org"))
;;             ;; Where to create new notes. For bib(la)tex items without a file
;;             ;; field in bibtex-completion-bibliography, create the note in the
;;             ;; bibtex subfolder of the org-roam-directory. Name the note after
;;             ;; the citation key (other names could be used). For items with a
;;             ;; file, create the note within the bibtex/org-noter subdirectory of
;;             ;; org-roam-directory. Name the note the same as the file field (see
;;             ;; orb-process-file-keyword). This ensures running org-noter on
;;             ;; note headlines or from within the PDF associated with the note do
;;             ;; the same thing.
;;             ;; WARNING: Changing the PDF name will break the connection between
;;             ;; note and PDF. Alternatively, you can use a single notes file
;;             ;; for multiple documents. The current zotfile renaming rule is
;;             ;; {%a_}{%y_}{%t}, with option "Replace blanks" enabled.
;;             :target
;;             (file+head "bibtex/%(if (= (length \"${file}\") 0) \"${citekey}\"
;;             (concat \"org-noter/\" (file-name-base \"${file}\"))).org" "#+title: ${title}\n\n")
;;             :unnarrowed t))))
;;     (apply old-func args)))

;; (advice-add 'orb--new-note :around 'orb-override-org-roam-capture-templates)

;; (straight-use-package 'pdf-tools)

;; (customize-package
;;  '(pdf-outline-display-labels nil "Useful when off
;;  because it shows page numbers starting from 1, even for scientific articles
;;  that typically have page numbers starting in the middle of a journal. Only
;;  affects the parenthesized portion of the outline items.")
;;  '(pdf-tools-enabled-modes
;;    '(pdf-isearch-minor-mode pdf-links-minor-mode
;;      pdf-outline-minor-mode pdf-misc-size-indication-minor-mode
;;      pdf-misc-menu-bar-minor-mode pdf-annot-minor-mode
;;      pdf-misc-context-menu-minor-mode pdf-cache-prefetch-minor-mode
;;      pdf-view-auto-slice-minor-mode pdf-occur-global-minor-mode)))

;; (defvar pdf-view-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     ;; (set-keymap-parent map image-mode-map)
;;     (define-key map (kbd "Q")         'kill-this-buffer) ; no effect in
;;                                         ; org-noter?
;;     ;; Navigation in the document
;;     (define-key map "h" 'image-scroll-right)
;;     (define-key map "l" 'image-scroll-left)
;;     (define-key map "j" 'pdf-view-scroll-up-or-next-page)
;;     (define-key map "k" 'pdf-view-scroll-down-or-previous-page)
;;     (define-key map "J" 'pdf-view-next-page-command)
;;     (define-key map "K" 'pdf-view-previous-page-command)
;;     (define-key map (kbd "g g") 'pdf-view-first-page)
;;     (define-key map "G" 'pdf-view-last-page)
;;     (define-key map (kbd "g t") 'pdf-view-goto-page)
;;     ;; Zoom in/out.
;;     (define-key map "+" 'pdf-view-enlarge)
;;     (define-key map "=" 'pdf-view-enlarge)
;;     (define-key map "-" 'pdf-view-shrink)
;;     (define-key map "0" 'pdf-view-scale-reset)
;;     ;; Fit the image to the window
;;     (define-key map (kbd "w w") 'pdf-view-fit-width-to-window)
;;     (define-key map (kbd "w h") 'pdf-view-fit-height-to-window)
;;     (define-key map (kbd "w p") 'pdf-view-fit-page-to-window)
;;     ;; Slicing the image
;;     (define-key map (kbd "s s") 'pdf-view-set-slice-from-bounding-box)
;;     (define-key map (kbd "s r") 'pdf-view-reset-slice)
;;     ;; Region
;;     (define-key map [down-mouse-1] 'pdf-view-mouse-set-region)
;;     (define-key map [C-down-mouse-1] 'pdf-view-mouse-extend-region)
;;     ;; NOTE: Until isearch is evilified, e.g., `n' does nothing but `C-s'
;;     ;; moves to the next match as expected for isearch
;;     (define-key map "/" 'isearch-forward)
;;     (define-key map "?" 'isearch-backward)
;;     map)
;;   "User-modified keymap used by `pdf-view-mode' when displaying a doc as a set
;; of images.")

;; (defvar pdf-outline-buffer-mode-map
;;   (let ((kmap (make-sparse-keymap)))
;;     (dotimes (i 10)
;;       (define-key kmap (vector (+ i ?0)) 'digit-argument))
;;     (define-key kmap "-" 'negative-argument)
;;     (define-key kmap (kbd "j") 'next-line)
;;     (define-key kmap (kbd "k") 'previous-line)
;;     (define-key kmap (kbd "g g") 'beginning-of-buffer)
;;     (define-key kmap "G" 'pdf-outline-end-of-buffer)
;;     ;; Display and move to page
;;     (define-key kmap (kbd "RET") 'pdf-outline-follow-link-and-quit)
;;     ;; Move to the PDF window (move back to outline with the same key)
;;     (define-key kmap (kbd "o") 'pdf-outline-select-pdf-window)
;;     ;; Display page and remain in outline
;;     (define-key kmap (kbd "f") 'pdf-outline-display-link)
;;     ;; PDF follows along as you navigate the outline
;;     (define-key kmap (kbd "F") 'pdf-outline-follow-mode)
;;     ;; Move within outline to the item for the displayed page
;;     (define-key kmap (kbd "'") 'pdf-outline-move-to-current-page)
;;     (define-key kmap (kbd "`") 'pdf-outline-move-to-current-page)
;;     (define-key kmap (kbd "q") 'quit-window)
;;     (define-key kmap (kbd "Q") 'pdf-outline-quit)
;;     (define-key kmap (kbd "C-c C-q") 'pdf-outline-quit-and-kill)
;;     kmap)
;;   "User-modified keymap used in `pdf-outline-buffer-mode'.")

;; ;; See the README for external dependencies that may be required in advance of
;; ;; running this command. Since this loads pdftools, it should come after any
;; ;; defvars redefining maps
;; (pdf-tools-install)

;; ;; In case global-display-line-numbers-mode is set. It is mentioned as an issue
;; ;; in the README and disables horizontal scrolling in PDFs from my own testing
;; (add-hook 'pdf-view-mode-hook (lambda ()(display-line-numbers-mode -1)))

;; ;; Each element of display-buffer-alist is structured like
;; ;; '(CONDITION . (FUNCTIONS . ALIST)). The value of each alist element is known
;; ;; as an action.
;; ;; CONDITION is a regexp matching buffer name, or a function that takes two
;; ;; arguments - a buffer name and the ACTION argument of display-buffer - and
;; ;; returns a boolean
;; ;; FUNCTIONS is a (list of) action function(s), which accept two arguments - a
;; ;; buffer to display and an alist of the same form as ALIST. See display-buffer
;; ;; for a description of available action functions. Also https://www.gnu.org/software/emacs/manual/html_node/elisp/Buffer-Display-Action-Functions.html.
;; ;; ALIST is an action alist. See display-buffer for a description of available
;; ;; action alist elements. Also https://www.gnu.org/software/emacs/manual/html_node/elisp/Buffer-Display-Action-Alists.html.
;; ;; If CONDITION succeeds, display-buffer adds action (FUNCTIONS . ALIST) to a
;; ;; list of actions it will try.
;; ;; Since actions are cons cells, you can omit the period between FUNCTIONS and
;; ;; ALIST if you also omit the outer parentheses around ALIST.

;; (defun pdf-outline-buffer-p (buffer &rest _)
;;   "Buffer's major-mode is pdf-outline-mode"
;;   (with-current-buffer buffer
;;     (derived-mode-p 'pdf-outline-buffer-mode)))

;; (defun pdf-view-p (buffer &rest _)
;;   "Buffer's major-mode is pdf-view-mode"
;;   (with-current-buffer buffer
;;     (derived-mode-p 'pdf-view-mode)))

;; ;; By default, when calling outline from within a window displaying a PDF
;; ;; buffer, the outline opens in the selectd window. pdf-outline-display-link and
;; ;; other commands open the PDF in a second window. In contrast,
;; ;; pdf-outline-follow-link-and-quit re-displays the PDF in its original
;; ;; window. There is a variable (pdf-outline-display-buffer-action) that can be
;; ;; used; however, display-buffer-alist may be more reliable for overcoming
;; ;; hardcoded display issues for all pdf-tools buffers.

;; ;; Reuse outline's previous window or pop one open. Avoid the selected window
;; ;; even if is also the outline's previous window. Windows may or may not be
;; ;; split to pop up a window.
;; (add-to-list 'display-buffer-alist
;;              '(pdf-outline-buffer-p
;;                (display-buffer-in-previous-window display-buffer-pop-up-window)
;;                (inhibit-same-window . t)
;;                (reusable-frames)))

;; ;; Reuse PDF's previous window in current frame
;; (add-to-list 'display-buffer-alist
;;              '(pdf-view-p
;;                display-buffer-in-previous-window
;;                (reusable-frames)))

;; (straight-use-package 'org-noter)

(use-package ace-window
  :custom
  (aw-keys '(97 115 100 102 103 104 106 107 108)))

;; All autoloaded
;; (defhydra+ hydra-window (:color blue)
;;   ("a" ace-window)
;;   ("c" ace-delete-window)
;;   ("s" ace-swap-window))

(use-package command-log-mode
  :custom
  (command-log-mode-auto-show nil)
  (command-log-mode-is-global t)
  (command-log-mode-key-binding-open-log nil)
  (command-log-mode-open-log-turns-on-mode t)
  (command-log-mode-window-size 50))

;; Auto-scroll buffer as commands are logged
(add-hook 'command-log-mode-hook 'auto-scroll)

(defun auto-scroll ()
  (set (make-local-variable 'window-point-insertion-type) t))

(straight-use-package 'evil-nerd-commenter)

(autoload 'evil-nerd-commenter-operator "evil-nerd-commenter-operator")

;; (defhydra+ hydra-buffer ()
;;   ("l" evil-switch-to-windows-last-buffer :color blue))

;; (defhydra+ hydra-window (:color pink)
;;   ("-" evil-window-decrease-height)
;;   ("+" evil-window-increase-height)
;;   ("<" evil-window-decrease-width)
;;   (">" evil-window-increase-width)
;;   ("H" evil-window-move-far-left :color blue)
;;   ("J" evil-window-move-very-bottom :color blue)
;;   ("K" evil-window-move-very-top :color blue)
;;   ("L" evil-window-move-far-right :color blue)
;;   ("r" evil-window-rotate-downwards)
;;   ("R" evil-window-rotate-upwards)
;;   ("v" evil-window-vsplit :color blue)
;;   ("x" evil-window-split :color blue))

(use-package indent-guide
  :hook (prog-mode . indent-guide-mode))

(use-package hideshow
  :straight (:type built-in)
  :hook
  (prog-mode . hs-minor-mode)
  (ess-r-mode . alternate-evil-hs-commands)
  :custom
  (hs-hide-comments-when-hiding-all nil "Due to the implementation of
  hs-hide-all, nested outline headings or headings followed by comments,
  possibly separated by space are hidden. Without a rewrite, this is not usable
  with outline mode.")
  :config
  ;; ess-r-mode
  (load "hideshow-extensions")
  (add-to-list 'hs-special-modes-alist '(ess-r-mode "{" "}" "#" nil nil)))

(use-package company
  :hook (prog-mode . company-mode)
  :config (load "company-extensions")
  :custom
  (company-frontends
   (list 'company-pseudo-tooltip-unless-just-one-frontend
         'company-preview-if-just-one-frontend)
   "Remove company-echo-metadata-frontend to speed up candidate navigation")
  (company-idle-delay 0.0)
  (company-minimum-prefix-length 1)
  (company-require-match nil "Otherwise company may block typing when automatic
  completion is enabled ")
  (company-selection-wrap-around t)
  (company-tooltip-align-annotations t)
  (company-tooltip-limit 20)
  :general
  (:keymaps 'global-map "<tab>" nil "TAB" nil)
  (:keymaps 'company-mode-map
            "<tab>" 'company-indent-or-complete-common
            "TAB" 'company-indent-or-complete-common)
  (:keymaps 'company-active-map
            "<return>" nil
            "RET" nil
            "<tab>" 'company-complete-selection
            "TAB" 'company-complete-selection
            "C-h" 'mode-specific-C-h
            "M-n"  nil
            "M-p"  nil
            "C-n"  'company-select-next
            "C-p"  'company-select-previous))

;; The README is incorrect. See
;; https://github.com/sebastiencs/company-box/issues/143.
;; TODO Compare to eldoc-box and company-quickhelp. Perhaps these have workable
;; company docs for ess-r-mode with lsp-mode
(use-package company-box
  :hook (company-mode . company-box-mode)
  :custom
  (company-box-doc-enable t)
  (company-box-enable-icon t))

;; NOTE: company-capf knows about completion-styles, so lsp-mode does as well

(use-package lsp-mode
  :hook
  ;; sp-command-map is bound to SPC l. If binding is changed, update
  ;; lsp--prepend-prefix in lsp-extensions.el
  (lsp-mode . (lambda ()
                (leader 'local "l" '(:keymap lsp-command-map))))
  (lsp-mode . lsp-enable-which-key-integration)
  (ess-r-mode . ess-r-lsp)
  :config
  (load "lsp-extensions")
  :custom
  (read-process-output-max (* 1024 1024))
  ;; (lsp-log-io t)
  (lsp-headerline-breadcrumb-enable nil "Takes up space without meangingful
  improvement")
  (lsp-enable-folding nil "Uses lsp-origami. Currently origami doesn't work with
  my init.el, so I am using hideshow.")
  (lsp-signature-render-documentation nil "Remove documentation from signature
  when within a function, which leads to signature truncation and is displayed
  elsewhere. See also lsp-signature-auto-activate. Note that this is for the
  signature when typing a function call. lsp-ui-doc or lsp-eldoc-enable-hover
  show a signature on hover over a call.")
  (lsp-eldoc-enable-hover nil "Not needed with lsp-ui-doc-enable to show
  signatures on hover. Furthermore, it doesn't work with ess-r-mode unless
  lsp-eldoc-render-all is non-nil. But this enables displaying documentation.")
  (lsp-eldoc-render-all nil "This also seems to show documentation--all of
  it--similar to lsp-signature-render-documentation, but on hover. So more like
  lsp-ui-doc in the echo area. See lsp-eldoc-enable-hover")
  (lsp-modeline-diagnostics-enable nil "Not configured to work with
  telephone-line, and diagnostic messages are shown with lsp-ui-sideline")
  (lsp-enable-snippet t "Provides parameter completion with names via tab. Does
  not work for ess-r-mode"))

;; TODO Set height and width maxima to different values if
;; (x-display-pixel-width) is greater than 1440 (my laptop screen size)

;; lsp-ui-doc shows docs for objects such as functions, signatures and argument
;; descriptions for (named) function args, and the last assignment statement for
;; variables. It is quite useful when browsing complex code like package
;; functions
(use-package lsp-ui
  :custom
  (lsp-ui-doc-alignment 'frame "Has a bias toward displaying the doc childframe
in a window on the right side in the frame when this option is set to
`frame'. The actual window containing the doc childframe depends on the window
splits and location of lsp-mode buffer. Split vertically, then horizontally in
the right vertical split. Then open the same buffer in each window. The do show
up in different windows depending on which window it is called from.")
  (lsp-ui-doc-position 'bottom "Where in the chosen window to display")
  (lsp-ui-doc-max-height 10)
  (lsp-ui-doc-max-width 80)
  (lsp-ui-doc-delay 1 "At least delay a bit to keep this from popping up all the
time when you don't want it")
  (lsp-ui-doc-show-with-cursor t "Shows docs for objects such as functions,
signatures and argument descriptions for (named?) arguments, and the last
assignment statement for variables. Disabling disables all of these things.")
  (lsp-ui-doc-use-webkit nil "Non-webkit docs are closer to RStudio docs in
appearance. While webkit highlights source code in blocks, it also mislabels
some sections as code. Non-webkit docs also have the advantage that headings are
indented by level, similar to how RStudio uses different font sizes.")
  (lsp-ui-imenu-auto-refresh t)
  (lsp-ui-doc-border "#93a1a1")
  (lsp-ui-sideline-diagnostic-max-lines 10 "Arbitrary value in case more
  diagnostics are available. Tweak as you code with ess-r and learn more about
  lintr and flycheck.")
  (lsp-ui-sideline-show-code-actions nil "Get rid of e.g. the message to disable
  all linters on line")
  :general
  ;; Also available in lsp-command-map
  (:keymaps 'lsp-ui-mode-map
            [remap xref-find-definitions] 'lsp-ui-peek-find-definitions
            [remap xref-find-references] 'lsp-ui-peek-find-references)
  :config
  ;; If the common prefix for company is a complete symbol, lsp-ui-doc will
  ;; show. This is especially a problem for autocompletion where completion will
  ;; display as you type. If you need to navigate the completion menu, you
  ;; likely want to banish the doc for the common prefix symbol. This is a hack
  ;; until I can figure out how to avoid showing the doc at all when the tooltip
  ;; is visible.
  (advice-add 'company-select-next :after 'lsp-ui-doc-hide)
  (advice-add 'company-select-previous :after 'lsp-ui-doc-hide))

;; Doesn't seem to do anything...
;; (lsp-dired-mode)

;; required for lsp-iedit-highlights. Call it at point, make edits, then hit C-g
;; over the symbol to finish editing symbols in parallel. Calling it again
;; before C-g adds additional symbols to edit in parallel. In contrast to
;; lsp-rename, editing is in the buffer rather than the minibuffer and allows
;; for more complex edits like adding a prefix to multiple symbols
;; Note: If a function and one of its arguments ar ethe same symbol, this will
;; not distinguish between the two. On the other hand, lsp-rename seems able to
;; tell the difference. You may have to enter insert mode after the symbol to be
;; renamed because sometimes you get a message about not being able to rename
;; the symbol depending on its context.
;; (use-package iedit)

;; lsp-describe-thing-at-point
;; lsp-ui-doc-show
;; lsp-auto-guess-root (projectile support?)
;; (lsp-ui-doc-focus-frame)
;; (lsp-ui-imenu) -> can be used even outside of lsp-mode buffers! Customize
;; imenu-generic-expression
;; lsp-ui-peek-jump-* (difference from xref-pop-marker-stack? Something about a
;; "window local jump list")
;; download R src code for packages so that *-find-definitions jumps to the
;; original rather than a temp file? See
;; http://applied-r.com/r-source-code/#:~:text=Compiled%20Package%20Code,the%20package%20source%20for%20you.
;; lsp-lens-enable (disabled by default; no support from ess-r)
;; lsp-modeline-* (probably not supported by telephone?)
;; in lsp-mode, a message is sent when scrolling "showing all blocks". This is
;; only defined in hs-show-all, which is called by hs-minor-mode when
;; enabling. Furthermore, when removing hs-minor-mode from prog-mode-hook, the
;; messges stop. This is strong evidence that lsp-mode is messing with this
;; minor mode. Finally, the messages don't show up in emacs lisp files, which do
;; not run lsp. And instrumenting hs-minor-mode drops us into debugging its body
;; when we scroll.

;; ;; Disable underlines in lsp-ui-doc child frames
;; (custom-set-faces '(nobreak-space ((t nil))))

;; (defun scroll-down-lsp-ui ()
;;   "Enable scrolling documentation child frames when using lsp-ui-doc-glance"
;;   (interactive)
;;   (if (lsp-ui-doc--frame-visible-p)
;;       (let ((kmap (make-sparse-keymap)))
;;         (define-key kmap (kbd "q")
;;           '(lambda ()
;;              (interactive)
;;              (lsp-ui-doc-unfocus-frame)
;;              (setq overriding-terminal-local-map nil)
;;              (setq which-key-show-transient-maps t)))
;;         (setq which-key-show-transient-maps nil)
;;         (setq overriding-terminal-local-map kmap)
;;         (lsp-ui-doc-focus-frame)))
;;   (evil-scroll-page-down 1))

(setq ess-nuke-trailing-whitespace-p t
      ;; ess-S-quit-kill-buffers-p 'ask
      inhibit-field-text-motion nil) ; prompt acts as beginning of line if prompt is read-only
(use-package ess
  :custom
  (ess-use-eldoc (if (featurep 'lsp-mode-autoloads) nil t) "In conjunction with
  `lsp-signature-auto-activate', this option leads to two signatures in the echo
  area if an iESS buffer is associated with the current ess buffer. ")
  (ess-use-company (if (featurep 'lsp-mode-autoloads) nil t) "Don't modify
  company-backends by removing company-capf (used by lsp) and adding ess-r
  backends (ignored by lsp)")
  :hook
  (ess-mode . prettify-symbols-mode) ; pretty ligatures
  (ess-r-mode . config-ess-r-mode))


;; As far as I can tell, ess-use-company removes company-capf and adds other
;; backends to company-backends, but the ess completion function doesn't invoke
;; company. It invokes completion-at-point.
(with-eval-after-load "ess-mode"
  (general-unbind ess-mode-map "TAB"))

;; Shadows xref-find-references
(with-eval-after-load "ess-r-mode"
  (general-unbind ess-r-mode-map "M-?"))

(defun config-ess-r-mode ()
  (ess-set-style 'RStudio)
  ;; (setq-local ess-indent-offset 4) ; RStudio style uses a value of 2

  ;; Rely on electric-pair-mode instead of skeleton
  (local-set-key (kbd "{") 'self-insert-command)
  (local-set-key (kbd "}") 'self-insert-command)

  ;; electric-layout-rules interferes with ess-roxy-newline-and-indent
  ;; if electric-layout-mode is enabled (it is not by default)
  (setq-local electric-layout-rules nil))


;; Override Windows' help_type option of "html", to open help in help buffer, not browser (see contents of .Rprofile)
(pcase system-type
  ('windows-nt
   ;; iESS searches the paths listed in the variable exec-path for inferior-ess-r-program
   (add-to-list 'exec-path "c:/Users/jkroes/Documents/R/R-3.6.2/bin")
   ;; Sets R_USER and R_LIBS_USER
   (setenv "R_USER" "c:/Users/jkroes/Documents")
   ;; run-ess-r fails when this is set to Rterm
   (setq inferior-ess-r-program "R")
   (setenv "R_PROFILE_USER" "C:/Users/jkroes/.emacs.d/.Rprofile")
   ;; RStudio downloads pandoc with rmarkdown, but outside of RStudio
   ;; you need to notify R of the executable's directory
   (setenv "RSTUDIO_PANDOC" "C:/Users/jkroes/AppData/Local/Pandoc"))
  ('darwin (setenv "R_PROFILE_USER" (concat user-emacs-directory ".Rprofile"))))

;; Disabling this while I render Word documents from Rmarkdown.
;;(customize-package '(polymode-display-output-file nil))

(straight-use-package 'poly-markdown)

;; NOTE: ess-r configuration and bindings are available inside chunks, where R-mode is active
;; I have bound polymode-export (render) to SPC-m-e-k
(straight-use-package 'poly-R)

;; Prevent window displaying company documentation buffer from vanishing when
;; invoking a binding not in company--electric-commands
;; (defun forget-saved-window-config ()
;;   (setq company--electric-saved-window-configuration nil))
;; (advice-add 'company-pre-command :before 'forget-saved-window-config)

(setq display-buffer-alist
      `(("\\*company-documentation\\*"
         (display-buffer-reuse-mode-window display-buffer-in-side-window)
         (mode. ess-r-help-mode)
         (side . right)
         (slot . 1)
         (window-width . 0.33)
         (reusable-frames . nil))
        ("\\*R Dired"
         (display-buffer-reuse-mode-window display-buffer-in-side-window)
         (side . right)
         (slot . -1)
         (window-width . 0.5)
         (reusable-frames . nil))
        ("\\*R"
         (display-buffer-reuse-mode-window display-buffer-below-selected)
         (window-height . 0.2)
         (reusable-frames . nil))
        ("\\*Help\\[R"
         (display-buffer-reuse-mode-window display-buffer-in-side-window)
         (side . right)
         (slot . 1)
         (window-width . 0.5)
         (reusable-frames . nil))
        ;; ("\\*Help\\*" display-buffer-same-window)
        ;; ("\\*Apropos\\*" display-buffer-same-window)
        )
      )

(defun my/start-r ()
  "Start an R process."
  (interactive)
  (save-selected-window
    (run-ess-r)
    ;;(ess-rdired)
    )
  (ess-force-buffer-current))

(cond ((eq system-type 'gnu/linux)
       (setq bookmark-file (concat user-emacs-directory "bookmarks_gnu")
             projectile-known-projects-file
             (concat user-emacs-directory "projectile-bookmarks_gnu.eld")
             ;; Open webpages in Windows
             browse-url-generic-program "/mnt/c/Windows/System32/cmd.exe"
             browse-url-generic-args '("/c" "start" "")
             browse-url-browser-function 'browse-url-generic)
       )
      ((eq system-type 'darwin)
       (setq mac-pass-command-to-system nil)
       (setq mac-pass-control-to-system nil)
       (setq mac-command-modifier 'control
             ;; E.g., M-RET instead of A-RET
             mac-option-modifier 'meta
             bookmark-file (concat user-emacs-directory "bookmarks_macos")
             projectile-known-projects-file
             (concat user-emacs-directory "projectile-bookmarks_macos.eld")
             )
       (general-define-key :states '(normal insert emacs)
                           ;; HYPER-SPC (hammerspoon) is bound to S-`
                           "C-`" 'other-frame)))

(leader
  ;; "" nil ; Unbind prefix key
  ";" 'execute-extended-command
  "!" 'shell-command
  ;; "u" 'universal-argument
  "a" '(:prefix-command my/apps-map :wk "apps")
  "k" '(:prefix-command my/keymaps-map :wk "keymaps")
  "f" '(:prefix-command my/files-map :wk "files")
  "h" '(:prefix-command my/help-map :wk "help"))

;; (general-def my/keymaps-map
;;   ;; See general bindings all in one buffer
;;   "g" 'general-describe-keybindings)
;; (general-def my/help-map
;;   "c" 'describe-key-briefly
;;   "i" 'info
;;   "I" 'info-display-manual
;;   "K" 'Info-goto-emacs-key-command-node
;;   "l" 'view-lossage
;;   "m" 'describe-mode
;;   "M" 'describe-minor-mode
;;   "p" 'describe-package
;;   "P" 'finder-by-keyword
;;   "s" 'describe-symbol
;;   "S" 'info-lookup-symbol
;;   "w" 'where-is
;;   "X" 'Info-goto-emacs-command-node)
;; (general-def :keymaps 'my/files-map
;;   ;; Allows for consistent wk replacement text during cyclical map navigation
;;   :wk-full-keys nil
;;   "b" '(:prefix-command my/bookmarks-map :wk "bookmarks")
;;   ;; https://beyondgrep.com/feature-comparison/
;;   "d" 'dired-default-directory
;;   "f" 'find-file-at-point
;;   "i" 'insert-file)
;; (general-def my/bookmarks-map
;;   :wk-full-keys nil
;;   "d" 'bookmark-delete
;;   "e" 'edit-bookmarks
;;   "f" '(my/files-map :wk "files")
;;   "l" 'bookmark-bmenu-list
;;   "r" 'bookmark-rename
;;   ;; Can be used within ranger/deer/dired. Previously set bookmarks can be
;;   ;; viewed there with "B"
;;   "s" 'bookmark-set)

(defun my/delete-other-windows-and-buffers ()
  "Delete other windows and buffers."
  (interactive)
  (defun select-kill-window-and-buffer (window)
    (select-window window)
    (kill-buffer-and-window))
  (let ((other-windows
         (delq (selected-window)
               (window-list (window-frame (selected-window)))))
        (kill-buffer-query-functions ;; Disable prompt to end process buffers
         (delq 'process-kill-buffer-query-function
               kill-buffer-query-functions)))
    (mapc 'select-kill-window-and-buffer other-windows)))

;;(leader "w" 'hydra-window/body)
;; (defhydra hydra-window (:color pink)
;;   "Window"
;;   ("=" balance-windows :color blue)
;;   ("b" hydra-buffer/body :color blue)
;;   ("d" delete-other-windows :color blue)
;;   ("D" my/delete-other-windows-and-buffers :color blue)
;;   ("h" windmove-left :color blue)
;;   ("j" windmove-down :color blue)
;;   ("k" windmove-up :color blue)
;;   ("l" windmove-right :color blue)
;;   ("q" nil))

(defun my/switch-to-scratch ()
  "Switch buffer to *Scratch*."
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun my/kill-other-buffers ()
  "Kill other buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (buffer-list))))

;; (defhydra hydra-buffer (:color pink)
;;   "Buffer"
;;   ("b" counsel-switch-buffer :color blue)
;;   ("e" view-echo-area-messages :color blue)
;;   ("k" kill-buffer :color blue)
;;   ("K" my/kill-other-buffers :color blue)
;;   ("p" previous-buffer)
;;   ("r" rename-uniquely :color blue)
;;   ("s" my/switch-to-scratch :color blue)
;;   ("w" hydra-window/body :color blue)
;;   ("q" nil))

;; TODO Feel free to change this binding. I kept is as an example of localleader
;; (general-define-key
;;  :prefix-command 'my/dired-map
;;  "h" #'dired-omit-mode)
;; (leader :keymaps 'dired-mode-map "m" 'my/dired-map)
(local-leader :keymaps 'dired-mode-map "h" #'dired-omit-mode)

;;(leader "p" '(:keymap projectile-command-map :wk "projects"))
;; projectile-dired
;; projectile-find-file-in-known-projects
;; projectile-ibuffer
;; projectile-save-project-buffers
;; projectile-browse-dirty-projects
;; projectile-find-other-file
;; projectile-switch-to-buffer
;; projectile-find-dir
;; projectile-recentf
;; projectile-find-file
;; projectile-kill-buffers
;; projectile-commander
;; projectile-multi-occur
;; projectile-switch-project
;; projectile-switch-open-project
;; projectile-replace
;; projectile-ripgrep (depends on rg or ripgrep emacs package)
;; projectile-vc

;;(leader "P" '(:prefix-command my/packages-map :wk "packages"))
;; (general-def my/packages-map
;;   "d" 'straight-primary-dependencies
;;   "D" 'straight-dependents
;;   "g" 'straight-get-recipe
;;   "f" 'straight-fetch-all
;;   "p" 'straight-pull-all
;;   ;; Clear unused packages from build cache and directory
;;   "P" 'straight-prune-build
;;   "m" 'straight-merge-all
;;   ;; Verify remote URLs are set correctly, no merge in progress, clean worktree,
;;   ;; and primary :branch is checked out.
;;   "n" 'straight-normalize-all
;;   "p" 'straight-push-all
;;   "r" 'straight-rebuild-all
;;   "u" 'straight-visit-package-website
;;   "v" 'straight-freeze-versions
;;   "V" 'straight-thaw-versions
;;   "w" 'straight-watcher-start
;;   "W" 'straight-watcher-stop)
;; "F" 'straight-fetch-package
;; "P" 'straight-pull-package
;; "M" 'straight-merge-package
;; "N" 'straight-normalize-package
;; "P" 'straight-push-package
;; "R" 'straight-rebuild-package

;; (leader "." 'clm/toggle-command-log-buffer)

 ;; Disable accidental q: in normal mode
 (general-define-key :states 'normal "q" nil)
 ;; Don't shadow xref-find-definitions (the other xref commands aren't shadowed)
 (general-unbind evil-normal-state-map "M-.")
 (general-unbind evil-motion-state-map "RET")

  ;; C-a/C-e: beg./end of line
  ;; C-b/C-f: back/forward char
  ;; M-b/M-f: back/forward word
  ;; <backspace> or DEL: back delete char or entire directory (if component is dir)
  ;; C-d or <delete-char> or <kp-delete>: forward delete char
  ;; C-<backspace> or M-<backspace> or M-<DEL>: back delete word or entire directory (if component is dir)
  ;; M-d: forward delete word
  ;; C-k: forward kill line
  (general-def vertico-map
    "DEL" #'vertico-directory-delete-char
    "C-<backspace>" #'vertico-directory-delete-word
    "M-<backspace>" #'vertico-directory-delete-word
    "M-DEL" #'vertico-directory-delete-word
    "C-j" #'ignore) ; avoid vertico-exit when meaning to press C-k; RET is available

  (general-def
    [remap apropos]                       #'consult-apropos
    [remap bookmark-jump]                 #'consult-bookmark
    [remap evil-show-marks]               #'consult-mark
    [remap evil-show-jumps]               #'+vertico/jump-list
    [remap evil-show-registers]           #'consult-register
    [remap goto-line]                     #'consult-goto-line
    [remap imenu]                         #'consult-imenu
    [remap locate]                        #'consult-locate
    [remap load-theme]                    #'consult-theme
    [remap man]                           #'consult-man
    [remap recentf-open-files]            #'consult-recent-file
    [remap switch-to-buffer]              #'consult-buffer
    [remap switch-to-buffer-other-window] #'consult-buffer-other-window
    [remap switch-to-buffer-other-frame]  #'consult-buffer-other-frame
    ;; TODO Why does this only affect insert state binding for M-y? I handle
    ;; this in ~/.doom.d/modules/editor/evil/config.el
    ;;[remap yank-pop]                      #'consult-yank-pop
    [remap persp-switch-to-buffer]        #'+vertico/switch-workspace-buffer)


  (general-def
    [remap list-directory] #'consult-dir)
  (general-def vertico-map
    "C-x C-d" #'consult-dir
    "C-x C-j" #'consult-dir-jump-file)

  (general-def
    [remap describe-bindings] #'embark-bindings
    "C-;" #'embark-act)
  (general-def minibuffer-local-map "C-;" #'embark-act)
  (leader "a" #'embark-act)
  (general-def embark-file-map "s" #'doom/sudo-find-file)


  (general-def minibuffer-local-map "M-A" #'marginalia-cycle)

;; (general-def my/keymaps-map
;;   "t" 'which-key-show-top-level
;;   "k" 'which-key-show-full-keymap
;;   "m" 'which-key-show-full-major-mode
;;   "M" 'which-key-show-full-minor-mode-keymap)


;; (general-define-key
;;  :states '(normal insert)
;;  "C-q" 'server-shutdown)


(general-define-key
 :prefix-command 'my/org-map
 "a" '(:prefix-command my/org-agenda-map :wk "agenda")
 "A" '(:prefix-command my/org-attach-map :wk "attach")
 "c" '(:prefix-command my/org-cycle-map :wk "cycle")
 "i" '(:prefix-command my/org-insert-map :wk "insert")
 "s" '(:prefix-command my/org-subtree-map :wk "subtree")
 "T" '(:prefix-command my/org-toggle-map :wk "toggle")
 "," 'org-insert-structure-template ; E.g. src block
 "@" 'org-mark-subtree
 ;; See also counsel-org-agenda-headlines
 ;; TODO Check out org-rifle
 ;; Alternative: https://emacs.stackexchange.com/questions/32617/how-to-jump-directly-to-an-org-headline
 "e" 'org-expand-emphasize
 "g" 'counsel-org-goto ; headings in current buffer.
 )

(general-define-key
 :prefix-command 'my/org-agenda-map
 ;; See also org-search-view:
 ;; https://orgmode.org/worg/org-tutorials/advanced-searching.html
 ;; TODO Show file each heading belongs to
 "g" 'counsel-org-goto-all ; headings in buffer-list
 "h" 'my/counsel-org-agenda-headlines ; Like counsel-org-goto for all agenda files
 )

(general-define-key
 :prefix-command 'my/org-attach-map
 "f" 'counsel-org-file ; list files in all attachment dirs in current buffer
 )

(general-define-key
 :prefix-command 'my/org-cycle-map
 "a" 'outline-show-all ; all text in buffer, including drawers
 "s" 'outline-show-subtree ; entire subtree (TAB TAB TAB)
 "h" 'outline-show-branches ; subtree headings (like a localized S-TAB S-TAB)
 "H" 'outline-show-children ; subtree child headings
 )

(general-define-key
 :prefix-command 'my/org-insert-map
 "d" 'org-insert-drawer
 "f" 'org-footnote-new
 "h" 'org-insert-heading ; C-u end of subtree; C-u C-u end of parent subtree
 "H" 'org-insert-heading-after-current
 "l" 'org-insert-link
 "p" 'org-set-property
 "s" 'org-insert-subheading
 ;; "y" 'org-rich-yank ; download the package to use this
 )

(general-define-key
 :prefix-command 'my/org-subtree-map
 ;; "a" ; toggle archive tag
 ;; "A" ; archive subtree
 "*" 'org-toggle-heading
 "d" 'org-cut-subtree
 "y" 'org-copy-subtree
 "p" 'org-yank
 "h" 'org-promote-subtree
 "l" 'org-demote-subtree
 "j" 'org-move-subtree-down
 "k" 'org-move-subtree-up
 ;; TODO Compare narrowing and widening to foldout
 "n" 'org-narrow-to-subtree
 "w" 'widen
 "s" 'org-sparse-tree
 "S" 'org-sort-entries
 )

(general-define-key
 :prefix-command 'my/org-toggle-map
 "b" 'org-toggle-blocks
 )

;;(leader :keymaps 'org-mode-map "m" 'my/org-map)

;; M-RET inserts heading before (at beginning of) or after current line
;; C-RET inserts heading at end of subtree
;; C-u C-u M-RET inserts heading at end of parent subtree

;; (general-define-key
;;  :keymaps 'org-src-mode-map
;;  "C-c '" 'my/org-edit-src-save-and-exit)

;; (general-def :keymaps 'org-mode-map
;;   "<C-tab>" 'org-force-cycle-archived
;;   ;; Promote/dedent heading or region (org-do-promote/demote)
;;   "M-h" 'org-metaleft
;;   "M-l" 'org-metaright
;;   ;; Promote/dedent subtree (org-promote/demote-subtree)
;;   "M-H" 'org-shiftmetaleft
;;   "M-L" 'org-shiftmetaright
;;   ;; Move heading up/down
;;   "M-j" 'org-shiftmetadown
;;   "M-k" 'org-shiftmetaup
;;   ;; Move subtree up/down (org-move-subtree-up/down)
;;   "M-J" 'org-metadown
;;   "M-K" 'org-metaup
;;   ;; Respects lists when filling
;;   "M-q" 'org-fill-paragraph)

;; (general-def
;;   :keymaps 'org-mode-map
;;   :states '(normal)              ; Setting only motion state does not work for mapping
;;   "DEL" 'my/org-mark-ring-goto          ; DEL because evil binds it in normal state. RET
;;   "RET" 'org-open-at-point  ; is only bound in motion state, which normal shadows.
;;   "g" '(:ignore t :wk "Entry navigation")
;;   "gh" 'outline-previous-visible-heading
;;   "gH" 'outline-up-heading
;;   "gl" 'outline-next-visible-heading
;;   "gj" 'org-forward-heading-same-level
;;   "gk" 'org-backward-heading-same-level
;;   )

(general-def :keymaps 'my/apps-map
  :wk-full-keys nil
  "o" '(:prefix-command my/apps-org-map :wk "org"))

(general-def :keymaps 'my/apps-org-map
  "a" 'org-agenda
  "e" 'counsel-org-entity ; https://orgmode.org/manual/Special-Symbols.html
  ;; Only used to grab links to files without added context or org IDs. For all
  ;; other uses see the org-roam leader binding for `org-store-link'
  "l" 'org-store-link-to-filepath
  "o" 'org-open-at-point-global)

;; (leader "o" '(:prefix-command my/org-roam-map :wk "org-roam"))
;; (general-def my/org-roam-map
;;   ;; Find a node, or create file node named after #title using org-capture
;;   "f" 'org-roam-node-find
;;   ;; Like `org-roam-node-find' but stays in current buffer after capture
;;   "F" 'org-roam-capture
;;   "c" 'org-roam-db-clear-all
;;   ;; Creates an ID if one is missing from a node when
;;   ;; `org-id-link-to-org-use-id' is `t' or `'create-if-interactive'
;;   "l" 'org-store-link
;;   "i" 'org-roam-node-insert
;;   "B" 'org-roam-buffer-toggle
;;   ;; Add a tag to the nearest node (#+filetags or a heading tag)
;;   "t" 'org-roam-tag-add
;;   ;; Add a ref to the nearest node (:roam_refs: property for file or heading)
;;   "r" 'org-roam-ref-add
;;   "R" 'org-roam-ref-find
;;   ;; Add an alias to :roam_aliases: preoprty of the nearest node, heading or
;;   ;; file-level. Useful for distinguishing headings apart with the same text in
;;   ;; different files. Alternatively, split large notes into smaller notes with
;;   ;; unique titles instead of aliases
;;   "a" 'org-roam-alias-add
;;   "v" 'org-roam-version)

;; (general-def my/org-roam-map
;;   :wk-full-keys nil
;;   "b" '(:prefix-command my/org-roam-bibtex-map :wk "bibtex"))

;; (general-def :keymaps 'my/org-roam-bibtex-map
;;   "b" 'ivy-bibtex
;;   "l" 'orb-insert-link
;;   "a" 'orb-note-actions)

;; (general-def my/help-map
;;   "f" 'helpful-callable
;;   "h" 'helpful-at-point
;;   "v" 'helpful-variable
;;   "k" 'helpful-key
;;   "x" 'helpful-command)

;; Kill helpful buffer instead of "burying" it. This prevents buried buffers
;; from being used to display helpful buffers in their window.
(general-def helpful-mode-map
  "q" 'quit-window-kill-buffer)

;; (general-def '(motion insert emacs)
;;  "C-f" 'scroll-down-lsp-ui)

;;(leader ";" 'evilnc-comment-operator)

;; (leader
;;   "c" '(:ignore t :wk "comments")
;;   "cc" 'evilnc-comment-or-uncomment-lines
;;   "cC" 'evilnc-copy-and-comment-lines
;;   ;; When given C-u <n>, will forward-match <n> against the rightmost
;;   ;; digits of each line. E.g., on line 160, C-u <72> will target lines
;;   ;; 160-172
;;   "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
;;   "cp" 'evilnc-comment-or-uncomment-paragraphs
;;   "cy" 'evilnc-comment-and-kill-ring-save
;;   ;; Whether empty lines can be commented as part of a selection
;;   "ce" 'evilnc-toggle-comment-empty-lines
;;   ;; When toggled off, all lines in a selection are commented if any
;;   ;; uncommented lines are included. Note that blank lines never count
;;   "cv" 'evilnc-toggle-invert-comment-line-by-line
;;   "c," 'evilnc-comment-operator
;;   "c." 'evilnc-copy-and-comment-operator)

  ;; (leader
  ;;   "y" '(:ignore t :wk "snippets")
  ;;   "yn" 'yas-new-snippet
  ;;   "ye" 'yas-visit-snippet-file
  ;;   "ye" 'yas-visit-snippet-file)
  ;; (leader :keymaps 'snippet-mode-map
  ;;   "m" '(:ignore t :wk "mode")
  ;;   "ml" 'yas-load-snippet-buffer
  ;;   "mc" 'yas-load-snippet-buffer-and-close
  ;;   "mt" 'yas-tryout-snippet)

;;(leader :keymaps 'emacs-lisp-mode-map "m" 'my/elisp-map)

(local-leader :keymaps 'emacs-lisp-mode-map
  "c" 'check-parens
  "d" 'eval-defun
  "m" 'pp-eval-expression ; "m" for minibuffer, where exp is evaluated
  "s" 'pp-eval-last-sexp
  "i" 'eval-print-last-sexp ; "i" for inserting result
  "r" 'eval-region)

;;(leader :keymaps 'ess-r-mode-map "m" 'hydra-r/body)

;; (defhydra hydra-r (:color pink)
;;   "R"
;;   ("SPC" ess-mark-function-or-para)
;;   ("a" ess-cycle-assign) ;; See how electric functions work as hydras...
;;   ("d" hydra-r-debug/body :color blue)
;;   ("e" hydra-r-eval/body :color blue)
;;   ("h" hydra-r-help/body :color blue)
;;   ("j" ess-goto-end-of-function-or-para)
;;   ("k" ess-goto-beginning-of-function-or-para)
;;   ("r" my/start-r :color blue)
;;   ("s" ess-switch-to-inferior-or-script-buffer :color blue)
;;   ("z" ess-submit-bug-report :color blue)
;;   ;; prog-indent-sexp
;;   ;; ess-indent-exp
;;   ;; ess-indent-new-comment-line
;;   ;; ess-complete-object-name
;;   ("q" nil))

;; (defhydra hydra-r-help (:color pink) ; ess-doc-map
;;   "R-help"
;;   ("a" ess-display-help-apropos)
;;   ("e" hydra-r-eval/body :color blue)
;;   ("i" ess-display-package-index)
;;   ("m" ess-manual-lookup)
;;   ("o" ess-display-help-on-object)
;;   ("p" ess-describe-object-at-point)
;;   ("r" hydra-r/body :color blue)
;;   ("t" ess-display-demos)
;;   ("v" ess-display-vignettes)
;;   ("w" ess-help-web-search)
;;   ("q" nil))

;; (defhydra hydra-r-eval (:color pink) ; ess-rutils-map and ess-extra-map
;;   "R-eval"
;;   ("<C-return>" ess-eval-region-or-function-or-paragraph-and-step)
;;   ("RET" ess-eval-region-or-line-and-step)
;;   ("b" ess-eval-buffer-from-beg-to-here)
;;   ("e" ess-eval-buffer-from-here-to-end)
;;   ("E" ess-dirs)
;;   ("f" ess-load-file)
;;   ("i" inferior-ess-reload)
;;   ;; ("P" ess-request-a-process) ;; Display selected iESS process and buffer
;;   ("p" ess-switch-process) ;; Switch process attached to script (current process buffer auto-displays if new,
;;   ;; but any script evaluation will auto-display attached process buffer if not already visible
;;   ("s" ess-switch-to-inferior-or-script-buffer)
;;   ("r" hydra-r/body :color blue)
;;   ("R" ess-rdired)
;;   ("u" ess-use-this-dir)
;;   ("w" ess-change-directory)
;;   ("q" nil))

;; (defhydra+ hydra-r-eval()
;;   ("k" polymode-export :color blue))

;; Note that several commands available in the inferior ess R
;; process while debugging are absent:
;; f (finish)
;; s (step)
;; help
;; where
;; <expr>
;; As such, it is best to debug from the inferior process where
;; the additional, built-in functionality is needed
;; TODO: Add commands here to ess-debug-minor-mode-map
;; (defhydra hydra-r-debug (:color pink) ;; ess-debug-minor-mode-map and ess-dev-map
;;   "R-debug"
;;   ("c" ess-debug-command-continue)
;;   ("f" ess-debug-flag-for-debugging) ;; base:::debug()
;;   ("F" ess-debug-unflag-for-debugging) ;; base:::undebug()
;;   ("g" ess-debug-goto-debug-point)
;;   ("n" ess-debug-command-next)
;;   ("N" next-error)
;;   ("p" previous-error)
;;   ("q" ess-debug-command-quit :color blue) ;; Investigate diff b/w this and ess-debug-stop
;;   ("Q" ess-debug-stop :color blue)
;;   ("s" ess-switch-to-ESS :color blue)
;;   ;; ("t" ess-debug-toggle-error-action) ;; Sets value of error option (e.g. options(error=recover)) for active process
;;   ;; ("u" ess-debug-command-up) ;; NOTE: currently broken. Use recover() from within debugging session (i.e. browse())
;;   ;; ess-debug-goto-input-event-marker
;;   ;; ess-debug-insert-in-forward-ring
;;   ("q" nil))
