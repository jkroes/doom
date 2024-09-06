;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; NOTE Current Doom commit is bf330b405d73757b314cc70e16ce991d2bbd9cc5
;; Install Emacs on MacOS with:
;;   brew install emacs-plus@29 --with-ctags --with-debug --with-imagemagick --with-native-comp
;; Place the following in ~/.config/emacs/early-init.el:
;;   (setq force-load-messages t)

;;; Todo List ------------------------------------------------------------------
;; Make consult-yank-pop bound to M-y in normal state
;; Move module configuration into this file and delete private modules
;; Annotate built-in modules with annotate.el
;; Finish processing init_temp.el and init_org_temp.el
;; Move functions out of miscellaneous/autoload.el that are used within this
;; file, because only autoloads should be in that file.
;; Add lexical roes module, then test that it
;; still works
;;
;; Why do README.org files render when viewing the default module but not the
;;   same version of the file in the private modules dir? See
;;   ~/.config/emacs/lisp/lib/docs.el and that project's .dir-locals.el
;; Set up ediff so that it doesn't display a command window at the bottom and
;;   find a way to bind the available commands in that window to localleader
;;   (such that they work regardless of which buffer the cursor is in)
;; Why does typing gd sometimes jump to
;;   ~/.config/emacs/.local/etc/@/init.29.el? E.g. when invoked on
;;   set-popup-rules!
;; Configure how annotation.el displays annotations to make annotation
;; highlighting less intrusive.
;; Open a Doom popup window (e.g., via helpful-variable). Then embark-act. C-g
;;   exits embark-act AND the popup window.
;; Rename user functions/commands
;; my/marginalia-annotate-function affects describe-function but not
;;   helpful-callable. Instead, my/marginalia-annotate-symbol applies.
;;   See also helpful-function. Maybe because callable includes macros?
;; Uncomment and test modifications to embark and marginalia related to
;;   org-attach (see marginalia's use-package form)
;; be easily killable
;; Look into kind-icon for corfu or nerd-icons-corfu
;; Test built-in corfu + lsp integration
;; Syntax highlighting still not rendering when you visit a file? See
;;   https://github.com/minad/org-modern/discussions/218. See if it goes away
;;   when you scroll far enough.
;; Finish configuring evil module, org-modern
;;
;; Add bindings:
;; - doom/copy-module-to-private
;; - my/ediff-doom-private-module
;; - comment-pad-with-dashes-right
;; - quit-window-kill-buffer (in modes like helpful-mode, assuming "q" doesn't
;;   already do something similar e.g. in a popup mode)
;; - org-open-at-point-global (for opening org links in emacs lisp comments)
;; - vertico-repeat and vertico-repeat-select (< and > ?)
;; - pp-macroexpand-last-sexp
;; - annotate-toggle-all-annotations-text and other annotation functions
;; - evil-record-macro (on leader)
;; - commands from titlecase pkg
;; - insert-org-entity
;; - org-toggle-ordered-property
;; - jkroes/org-statistics-count-checkbox
;; - jkroes/org-statistics-count-todo

;;; Miscellaneous -------------------------------------------------------------

;; On startup, set the font to a smaller size if working on the
;; laptop without external monitors
(setq doom-font (font-spec
                 :family "JuliaMono"
                 :size (jkroes/startup-font-size)))

(setq doom-theme 'modus-vivendi)

;; Where my org notes live
(setq org-directory (expand-file-name "~/org"))

;; All of my org files are org-roam files
(setq org-roam-directory org-directory)

;; Center and focus Emacs frame on launch
(select-frame-set-input-focus (selected-frame))

;; Treat command key like control
(setq ns-command-modifier 'control
      mac-command-modifier 'control)

;; Configure display-line-numbers-mode for modes where it is enabled.
;; Individual buffers can toggle between different types of line numbers via
;; `jkroes/toggle-line-numbers'. Commands like `consult-line' always show
;; absolute line numbers regardless of this setting.
(setq display-line-numbers-type 'relative)

;; Disable line numbers for text buffers, since org-mode is derived from it,
;; and navigation works differently for these buffers. E.g., numeric prefixes
;; for movement commands across a collapsed subtree moves by that number of
;; headings regardless of displayed line number. The display of line numbers
;; for collapsed org-mode headings can be fixed by setting
;; display-line-numbers-type to 'visual; however, prefixed motions will not
;; jump to the expected line when `visual-line-mode' is enabled and you are
;; jumping to or across wrapped lines--unless `evil-respect-visual-line-mode'
;; was enabled prior to loading evil. Note that `jkroes/toggle-line-numbers'
;; still seems to work in modes where `display-line-numbers-mode' is disabled.
(remove-hook! 'text-mode-hook #'display-line-numbers-mode)

;; Toggle display line numbers type to match when toggling visual lines
(defadvice! jkroes/match-display-line-to-visual-line-a (&rest _)
  :after #'visual-line-mode
  (when (or (and visual-line-mode (eq display-line-numbers 'relative))
            (and (null visual-line-mode) (eq display-line-numbers 'visual)))
  (jkroes/toggle-line-numbers)))

;; Do not prompt when killing Emacs
(setq confirm-kill-emacs nil)

;; Disable messages about available keybindings when using M-x
(setq suggest-key-bindings nil)

;; Useful in conjunction with `enable-recursive-minibuffers'
(minibuffer-depth-indicate-mode)

;; Scroll screen to right (`scroll-left') automatically when cursor moves off
;; screen. See `hscroll-step' and `hscroll-margin' for details.
(setq auto-hscroll-mode t) ; 'current-line

;; BUG Even if `undo-no-redo' is non-nil, if you `undo' all edits in a buffer,
;; switch to a second window, then switch back, `undo' no longer reports "No
;; further undo information." It redoes the first edit in the buffer, then
;; undoes that redo, then reports the message. `vundo' does not have this same
;; issue.

;; Print full results to the messages buffer when evaluating expressions
(setq eval-expression-print-length nil
      eval-expression-print-level  nil
      edebug-print-length 1000)

(setq undo-no-redo t)

(use-package! titlecase :defer t)

;; Projectile caching is used with e.g. doom-project-find-file (SPC-f-F).
;; It's probably worth enabling for large projects, but for now it's
;; omitting file candidates that have been recently added to e.g. a
;; private module.
(setq projectile-enable-caching nil)

(when IS-WSL
  (setq browse-url-generic-program "/mnt/c/Windows/System32/cmd.exe"
        browse-url-generic-args '("/c" "start" "")
        browse-url-browser-function 'browse-url-generic))

;;; auto-fill -----------------------------------------------------------------

(setq-default fill-column 79)

;; Within a comment, typing a nonspace character followed by a space beyond
;; column will cause Emacs to hard wrap your comment
(add-hook 'prog-mode-hook 'turn-on-auto-fill) ; https://www.gnu.org/software/emacs/manual/html_node/efaq/Turning-on-auto_002dfill-by-default.html
(setq comment-auto-fill-only-comments t)

(defadvice! jkroes/scroll-right-on-auto-fill (fn &rest _)
  "When auto-filling, automatically undo the effects of
 auto-hscroll-mode by scrolling back again to the left."
  :around 'do-auto-fill
  (when (funcall fn) (scroll-right)))

;; TODO Set regexp if you need to inhibit auto-fill in specific places
;; (setq auto-fill-inhibit-regexp "")

;;; which-key -----------------------------------------------------------------

;; See lisp/doom-keybinds.el for additional settings
(setq which-key-idle-delay 0.1)

;; NOTE This masks Doom's description of bindings for remaps only
;; (e.g. "SPC h b b")
(setq which-key-compute-remaps t)

;; Disable Doom's descriptions of bindings. If the user rebinds keys with map!
;; but doesn't specify :desc, the :desc from previous bindings via map! still
;; shows up for some reason. Unfortunately, this also strips some useful
;; descriptions.
;; (setq which-key-replacement-alist nil)

;;; helpful -------------------------------------------------------------------

;; BUG The original function expects a list but does not ensure that it
;; receives a list. Because it shouldn't have to. which-key incorrectly
;; specifies `defcustom' :package-version as a string, in contrast to most
;; other packages. See the documentation for `defcustom', which details the
;; exepctation for :package-version.

;; TODO File an issue with which-key
(advice-add #'helpful--version-info :override #'my/helpful--version-info)

;;; profiler ------------------------------------------------------------------

;; HACK I'm not sure Doom's settings for these variables make sense.
;; They push the functions to the RHS of the screen. I don't
;; understand this variable fully, since there's no docs. I just
;; tried left-aligning, but you may have to tweak this.
(after! profiler
  (setq profiler-report-memory-line-format
        '((20 left
           ((15 left profiler-format-number)
            (5 left)))
          (1 left "%s")
          (0 left)))

  (setq profiler-report-cpu-line-format
        '((20 left
           ((12 left)
            (5 left)))
          (1 left "%s")
          (0 left))))

;;; modules/editor/evil -------------------------------------------------------

;;(set-evil-initial-state! '(dired-mode) 'emacs)

;; Easier scrolling
(map! "M-h" (lambda () (interactive) (evil-scroll-column-left 40))
      "M-l" (lambda () (interactive) (evil-scroll-column-right 40)))

;;; modules/tools/lsp ---------------------------------------------------------

;; TODO Probably safe to delete this. Pulled it from the now-outdated module

;; Format eglot help and doc buffers.
;; (advice-add 'eglot--format-markup :filter-return
;;             ;; TODO Only ruby-mode has been configured so far.
;;             #'format-eglot-help-a)

;; (defun format-eglot-help-a (buffer-text)
;;   "Clean up the format of *eglot-help* and *eglot-doc* buffer text.
;; Accepts unformatted help text. Returns pre-formatted text."
;;   (pcase major-mode
;;     ('ruby-mode
;;      (replace-regexp-in-string "  \n" " " buffer-text))))


;;;###autoload
;; (defun format-eglot-help-a (buffer-text)
;;   "Clean up the format of *eglot-help* and *eglot-doc* buffer text.
;; Accepts unformatted help text. Returns pre-formatted text."
;;   (pcase major-mode
;;     ('ruby-mode
;;      (replace-regexp-in-string "  \n" " " buffer-text))))

;;;###autoload
;; (defun wrap-corfu-eglot-doc-buffer-a (window)
;;   "Apply `visual-line-mode' to *eglot doc* buffer created by
;; invoking corfu-info-documentation. Use to advise
;; `corfu-info--display-buffer'. Note that eglot help buffers can
;; achieve the same effect by simply using `help-mode-hook'."
;;   (with-current-buffer (window-buffer window)
;;     (visual-line-mode))
;;   window)

;;; modules/ui/indent-guides

;;; modules/ui/modeline -------------------------------------------------------

;; Increase the visibility of the evil state indicator
(setq doom-modeline-modal-icon nil)

;;; modules/ui/popup ----------------------------------------------------------

;; TODO Copied from previous doom configuration. Haven't tested.

;;(advice-remove #'org-edit-src-exit #'+popup--org-edit-src-exit-a)

;; TODO The first info buffer shows the modeline, but successive buffers do not.
;; Investigate the modeline rules for popups. In the meantime, disable modeline
;; hiding for popups.
(remove-hook '+popup-buffer-mode-hook #'+popup-set-modeline-on-enable-h)

;;; Extra keybindings ---------------------------------------------------------

;; Remapping a command via global-set-key applies to all keymaps. A binding
;; will be matched in a keymap, then the current global map will be checked for
;; remappings of that command to another command. See e.g. evil-jump-forward
;; within Doom Emacs.

;; Per https://www.reddit.com/r/emacs/comments/bj1jjf/key_binding_to_capital_letters_questions/,
;; bind keys to M-<uppercase ascii> or C-S-<lowercase ascii>.

;; Keybinding precedence:
;; https://discourse.doomemacs.org/t/how-to-re-bind-keys/56
;; ~/.config/emacs/.local/straight/repos/evil/evil-core.el
;; https://github.com/noctuid/evil-guide?tab=readme-ov-file#keymap-precedence
;; https://github.com/syl20bnr/spacemacs/wiki/Keymaps-guide (missing an entry
;; for evil minor-mode keymaps within emulation-mode-map-alists)
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Searching-Keymaps.html
;; https://www.masteringemacs.org/article/mastering-key-bindings-emacs#keymap-lookup-order

;; (after! vundo
;;   (evil-collection-define-key 'normal 'vundo-mode-map
;;     "d" 'vundo-diff))

;; Easily scroll popups

;; TODO Can't pass cmd! or cmd!! forms as part of `predlist'. Must be a defined
;; function, because those forms are not evaluated to yield a lambda.
;; TODO This doesn't support combined keymap and state yet. One or the other.
;; I need a way to look up evil state bindings within keymaps first.
(defmacro jkroes/dispatch-scroll-commands (keymap state binding fallback &rest predlist)
  "Bind a predicate dispatcher `predlist' to `binding' in `keymap' or the
keymap associated with an evil `state' symbol. If no predicate in
`predlist' succeeds, then fall back to the command previously
bound to `binding' in the keymap or in the current global map."
  (declare (indent 4))
  (let ((map (or keymap (intern (format "evil-%s-state-map" state)))))
    `(general-def
       ,@(when keymap (list keymap))
       ;; TODO Can I just bind to the evil keymap instead?
       ,@(when state `(:states ',state))
       ,binding
       (general-predicate-dispatch ,@(list fallback)
           ;; (quote ,(or (lookup-key (symbol-value map) (kbd binding))
           ;;             ;; Crude fallback: Search for a global binding.
           ;;             (lookup-key (current-global-map) (kbd binding))))
         ,@predlist))))

(jkroes/dispatch-scroll-commands nil insert "C-n" nil
  (corfu-popupinfo--visible-p)
  #'corfu-popupinfo-scroll-up-5
  (jkroes/corfu-visible-p)
  #'corfu-scroll-up
  (jkroes/embark-actions-buffer-visible)
  #'scroll-other-window)

(jkroes/dispatch-scroll-commands nil insert "C-p" nil
  (corfu-popupinfo--visible-p)
  #'corfu-popupinfo-scroll-down-5
  (jkroes/corfu-visible-p)
  #'corfu-scroll-down
  (jkroes/embark-actions-buffer-visible)
  #'scroll-other-window-down)

(jkroes/dispatch-scroll-commands nil normal "C-n" nil
  (jkroes/embark-actions-buffer-visible)
  #'scroll-other-window)

(jkroes/dispatch-scroll-commands nil normal "C-p" nil
  (jkroes/embark-actions-buffer-visible)
  #'scroll-other-window-down)

(jkroes/dispatch-scroll-commands minibuffer-local-map nil "C-n" #'scroll-up-command
  (jkroes/embark-actions-buffer-visible)
  #'scroll-other-window)

(jkroes/dispatch-scroll-commands minibuffer-local-map nil "C-p" #'scroll-down-command
  (jkroes/embark-actions-buffer-visible)
  #'scroll-other-window-down)

;; The keybindings are too easy to hit and aren't necessary with evil
(when (modulep! :editor evil)
  (unbind-command #'undo global-map)
  (unbind-command #'undo-redo global-map))

;; TODO C-. and C-, are generally undefined and are also good candidates
;; for vertico-repeat/vertico-repeat-select instead of embark-collect, as
;; well as for binding in vertico-map and corfu-map for scrolling
;; Consider also M-n and M-p, which are only used to scan history in the
;; minibuffer.

;; TODO This is a temporary keybinding and workaround to find a definition via
;; completing-read, until I can investigate the lookup module and whether it's
;; possible to incorporate completing read into its commands.
(setq xref-show-definitions-function #'xref-show-definitions-completing-read)
;; xref-find-definitions searches by thing at point, or with a prefix by
;; completing read. In the latter case, the first candidate is the thing at point.
(map! :leader "cd"  (cmd! (let ((current-prefix-arg '(4))) (call-interactively #'xref-find-definitions))))

;;; org -----------------------------------------------------------------------

;;;; tasks

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
;; C-c records a note only text has been inserted and a timestamp otherwise.
(setq org-log-done nil
      org-log-into-drawer "LOGBOOK")

;; NOTE Keywords should be reserved for task states that you want to count
;; for statistics cookies. Metadata like idea or project should be implemented
;; as tags instead.
(setq org-todo-keywords
        '((sequence
           "TODO(t)"     ; A task that is ready to start
           "NOW(n!)"     ; An active task
           "CHOOSE(c)"
           "WAIT(w@/!)"  ; A suspended task
           "|"
           "CHOSEN"
           "DONE(d!/@)"    ; Task successfully completed
           "KILL(k@/@)"))) ; Task was cancelled, aborted, or is no longer applicable

(defvar jkroes/active-todo-states '("TODO" "NOW"))
(defvar jkroes/choice-todo-states '("CHOOSE" "CHOSEN"))

;; Block switching of parent state to done until child headings or checkboxes
;; are done. NOTE This does not prevent switching parent state from done to
;; todo regardless of children state
(setq org-enforce-todo-dependencies t
      org-enforce-todo-checkbox-dependencies t)

;; When updating statistics cookies, either count the number of direct child
;; headings or the number of checkboxes recursively
(setq org-hierarchical-todo-statistics t
      org-checkbox-hierarchical-statistics nil)

;; Ensure a cookie is inserted if needed so that `org-toggle-todo-state' can
;; trigger cascading state changes from the bottom of the subtree upwards. This
;; affects org-todo, org-insert-item, etc.
(advice-add #'org-update-parent-todo-statistics
            :before #'jkroes/insert-statistics-cookie)

(defun jkroes/insert-statistics-cookie (&rest _)
  "Insert cookie in parent heading so that `org-toggle-todo-state'
executes afterwards."
  ;; (save-excursion
  ;;   (when (equal (org-get-todo-state) "CHOOSE")
  ;;     (let* ((cookie-re "\\[\\([0-9]*\\)/\\([0-9]*\\)\\]")
  ;;            (cookie-end (re-search-forward cookie-re (line-end-position) t))
  ;;            (cookie-beginning (match-beginning 0)))
  ;;       ;; Delete cookie if present
  ;;       (when cookie-end (delete-region cookie-beginning cookie-end)))))
  (save-excursion
    (when (> (org-current-level) 1)
      (org-up-heading-safe)
      ;; Don't insert a cookie if one already exists
      (let* ((cookie-re "\\[\\([0-9]*\\)/\\([0-9]*\\)\\]")
             (cookie-end (re-search-forward cookie-re (line-end-position) t)))
        (unless cookie-end
          (org-end-of-line)
          (insert " [/]"))))))

;; When `org-provide-todo-statistics' is enabled, this can walk up the tree
;; changing state recursively until a parent is encountered without a
;; statistics cookie or that is not eligible to change state. The
;; call/hook/variable sequence looks like:
;;
;; org-todo -> jkroes/toggle-statistics-cookie ->
;; org-update-parent-todo-statistics -> cookie-present ->
;; org-after-todo-statistics-hook -> org-toggle-todo-state -> org-todo ...
(add-hook 'org-after-todo-statistics-hook #'jkroes/org-toggle-todo-state)

(defun jkroes/org-toggle-todo-state (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO
otherwise."
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

;; NOTE Think twice before moving headings in and out of subtrees, because
;; doing so may change the todo state of the subtrees. If you disable the
;; advice below, you will need to manually trigger a todo state change (even
;; to the same current state) to force the subtree to update itself. I think
;; this is the safer option.

;; (advice-add #'org-promote :after #'jkroes/org-promote-update-statistics)
;; (advice-add #'org-demote :after #'jkroes/org-demote-update-statistics)

;; (defun jkroes/org-promote-update-statistics ()
;;   (org-update-statistics-cookies nil)
;;   (org-backward-heading-same-level 1)
;;   (jkroes/org-demote-update-statistics))

;; (defun jkroes/org-demote-update-statistics ()
;;   (save-excursion
;;     (let ((continue? t))
;;       (while continue?
;;         (org-update-statistics-cookies nil)
;;         (setq continue? (org-up-heading-safe))))))

;; ;; NOTE There's no clean place to hook into list item creation, because
;; ;; `org-insert-item' does not insert an item if no list item is already
;; ;; present. A checkbox item can be created with C-- followed by C-u SPC m x,
;; ;; but it's probably best to just insert a cookie after first toggling a
;; ;; checkbox item or by inserting subsequent ones with `org-insert-item'

;; (advice-add #'org-toggle-checkbox :after #'org-toggle-todo-checkboxes)
;; (advice-add #'org-insert-item :after #'org-toggle-todo-checkboxes)

;; NOTE If this is uncommented, you need to place a call to
;; org-update-checkbox-count at the very top of org-toggle-todo-checkboxes

;; (after! org-list
;;   (setcdr (assoc 'checkbox org-list-automatic-rules) nil))

;; Affects org-toggle-checkbox and org-insert-item
(add-hook 'org-checkbox-statistics-hook #'org-toggle-todo-checkboxes)

(defun org-toggle-todo-checkboxes (&rest _)
  "Insert a statistics cookie and todo state for a heading with
checkboxes, or if a cookie exists toggle between todo and done state
depending on whether all checkboxes are done."
  ;; Count must be updated before regexp matching.
  ;; (org-update-checkbox-count)
  (save-excursion
    (org-back-to-heading t)
    (let* ((cookie-re "\\[\\([0-9]*\\)/\\([0-9]*\\)\\]")
           (cookie-end (re-search-forward cookie-re (line-end-position) t))
           (cookie-beginning (match-beginning 0))
           (numerator (match-string 1))
           (denominator (match-string 2))
           (state (org-get-todo-state)))
      (cond ((member state (cons "DONE" jkroes/active-todo-states))
             (if cookie-end
                 ;; TODO, NOW -> DONE
                 (cond ((and (member state jkroes/active-todo-states)
                             (equal numerator denominator))
                        (org-todo "DONE"))
                       ;; DONE -> TODO
                       ((and (equal state "DONE")
                             (not (equal numerator denominator)))
                        (org-todo "TODO")))
               ;; Insert a cookie if missing and start from the top
               (org-end-of-line)
               (insert " [/]")
               (org-toggle-todo-checkboxes)))
            ((member state jkroes/choice-todo-states)
             (when (eq this-command #'org-toggle-checkbox)
               (org-todo (car (remove state jkroes/choice-todo-states)))))))))

(defun test (change-plist)
  "Prevent unchecked checkboxes for CHOOSE headings from blocking
switching to CHOSEN and make org-toggle-checkbox execute
org-toggle-radio-button."
  (let ((prev-state (plist-get change-plist :from))
        (state (plist-get change-plist :to)))
    (cond ((and (equal state "CHOOSE")
                (not (equal prev-state "CHOOSE")))
           (jkroes/org-set-radio-keyword)
           (org-set-property "NOBLOCKING" "t")
           (let (org-checkbox-statistics-hook)
             (org-reset-checkbox-state-subtree)))
          ((and (not (equal state "CHOSEN"))
                (equal prev-state "CHOOSE"))
           (jkroes/org-unset-radio-keyword)
           (org-entry-delete nil "NOBLOCKING")
           ))))

;; BUG org-toggle-checkbox calls org-table-radio-button, which both call
;; org-update-checkbox-count-maybe. This calls org-toggle-todo-checkboxes
;; twice. This should be fixed upstream.
(advice-add #'org-toggle-radio-button :around
            (lambda (orig-fun &rest args)
              (advice-add 'org-update-checkbox-count-maybe :override #'ignore)
              (apply orig-fun args)
              (advice-remove 'org-update-checkbox-count-maybe #'ignore)))

;; ;; TODO Write an intermediary helper function for these two functions
;; ;; TODO See org-at-radio-list-p and org-item-re

(defun jkroes/org-set-radio-keyword ()
  (let ((case-fold-search t)
        (radio_keyword "#+ATTR_ORG: :radio t")
        (end (org-entry-end-position))
        (continue? t)
        line-beg line-end)
    (save-excursion
      (org-back-to-heading t)
      ;; Skip all drawers (PROPERTIES, LOGBOOK, etc.)
      (while continue?
        (unless (re-search-forward "^[ \t]*:END:[ \t]*$" end t)
          (setq continue? nil)))
      ;; Search for the first list item within the body of the current heading
      (when (re-search-forward org-list-full-item-re end t)
        (forward-line -1)
        (setq line-beg (line-beginning-position)
              line-end (line-end-position))
        (unless (equal (buffer-substring line-beg line-end)
                       radio_keyword)
          (forward-line)
          (insert (string-join (list radio_keyword "\n"))))))))

(defun jkroes/org-unset-radio-keyword ()
  (let ((case-fold-search t)
        (radio_keyword "#+ATTR_ORG: :radio t")
        (end (org-entry-end-position))
        (continue? t)
        line-beg line-end)
    (save-excursion
      (org-back-to-heading t)
      ;; Skip all drawers (PROPERTIES, LOGBOOK, etc.)
      (while continue?
        (unless (re-search-forward "^[ \t]*:END:[ \t]*$" end t)
          (setq continue? nil)))
      ;; Search for the first list item within the body of the current heading
      (when (re-search-forward org-list-full-item-re end t)
        (forward-line -1)
        (setq line-beg (line-beginning-position)
              line-end (line-end-position))
        (when (equal (buffer-substring line-beg line-end)
                     radio_keyword)
          (delete-region line-beg line-end)
          (delete-char 1)
          t)))))

(defun jkroes/org-statistics-count-checkbox ()
  (interactive)
  (org-set-property "COOKIE_DATA" "checkbox")
  (save-excursion
    (org-back-to-heading t)
    (org-update-statistics-cookies nil)))

(defun jkroes/org-statistics-count-todo ()
  (interactive)
  (org-set-property "COOKIE_DATA" "todo")
  (save-excursion
    (org-back-to-heading t)
    (org-update-statistics-cookies nil)))
