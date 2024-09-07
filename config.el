;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Treat command key like control
(setq ns-command-modifier 'control
      mac-command-modifier 'control)

(when IS-WSL
  (setq browse-url-generic-program "/mnt/c/Windows/System32/cmd.exe"
        browse-url-generic-args '("/c" "start" "")
        browse-url-browser-function 'browse-url-generic))

;; Center and focus Emacs frame on launch
(select-frame-set-input-focus (selected-frame))

;; Do not prompt when killing Emacs
(setq confirm-kill-emacs nil)

;; Disable messages about available keybindings when using M-x
(setq suggest-key-bindings nil)

;; Scroll screen to right (`scroll-left') automatically when cursor moves off
;; screen. See `hscroll-step' and `hscroll-margin' for details.
(setq auto-hscroll-mode t) ; 'current-line

;; BUG Even if `undo-no-redo' is non-nil, if you `undo' all edits in a buffer,
;; switch to a second window, then switch back, `undo' no longer reports "No
;; further undo information." It redoes the first edit in the buffer, then
;; undoes that redo, then reports the message. `vundo' does not have this same
;; issue.

(setq undo-no-redo t)

(use-package! titlecase :defer t)

;; Projectile caching is used with e.g. doom-project-find-file (SPC-f-F).
;; It's probably worth enabling for large projects, but for now it's
;; omitting file candidates that have been recently added to e.g. a
;; private module.
(setq projectile-enable-caching nil)

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

;; TODO This bug doesn't appear in every instance. See the binding at the
;; bottom of this buffer.
;; BUG Disable Doom's descriptions of bindings. If the user rebinds keys with map!
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

;; The keybindings are too easy to hit and aren't necessary with evil
(when (modulep! :editor evil)
  (unbind-command #'undo global-map)
  (unbind-command #'undo-redo global-map))

;; TODO C-. and C-, are generally undefined and are also good candidates
;; for vertico-repeat/vertico-repeat-select instead of embark-collect, as
;; well as for binding in vertico-map and corfu-map for scrolling
;; Consider also M-n and M-p, which are only used to scan history in the
;; minibuffer.

(setq doom-theme 'modus-vivendi)
(setq doom-font (font-spec :family "JuliaMono"
                           :size (jkroes/startup-font-size)))

(setq display-line-numbers-type 'relative)

(remove-hook! 'text-mode-hook #'display-line-numbers-mode)

(defadvice! jkroes/match-display-line-to-visual-line-a (&rest _)
  :after #'visual-line-mode
  (when (or (and visual-line-mode (eq display-line-numbers 'relative))
            (and (null visual-line-mode) (eq display-line-numbers 'visual)))
  (jkroes/toggle-line-numbers)))

(defun jkroes/toggle-line-numbers ()
  "Cycles the current buffer through absolute, relative/visual and no
 line numbers. If line numbers are relative or visual, calling
 this command after toggling visual-line-mode will toggle to the other type."
  (interactive)
  (let* ((evil-not-visual
          (and (bound-and-true-p evil-mode)
               (not (bound-and-true-p
                     evil-respect-visual-line-mode))))
         (types
          `(t
            ,(if (and visual-line-mode
                      (or (not evil-not-visual)
                          (eq major-mode 'org-mode)))
                 'visual
               'relative)
            nil))
         (head (memq display-line-numbers types))
         (tail (seq-difference types head))
         (next (cadr (append head tail))))
    (setq display-line-numbers next)
    (message "Switched to %s line numbers"
             (pcase next
               (`t "normal")
               (`nil "disabled")
               (_ (symbol-name next))))))

;; Useful in conjunction with `enable-recursive-minibuffers'
(minibuffer-depth-indicate-mode)

;; Print full or long results to the messages buffer when evaluating
;; expressions
(setq eval-expression-print-length nil
      eval-expression-print-level  nil
      edebug-print-length 1000)

;; Hide commands in M-x which do not work in the current mode
(setq read-extended-command-predicate #'command-completion-default-include-p)

(map! :when (modulep! :editor evil)
      :map vertico-map
      ;; "C-SPC" #'+vertico/embark-preview
      "C-j"   #'vertico-next
      "M-j" #'vertico-next-group
      ;; Shadows `kill-line', but S-<backspace> and C-S-<backspace> are still
      ;; available
      "C-k"   #'vertico-previous
      "M-k" #'vertico-previous-group)

;; Where my org notes live
(setq org-directory (expand-file-name "~/org"))

;; All of my org files are org-roam files
(setq org-roam-directory org-directory)

;; Don't fold drawers when cycling.
(after! org-fold (fset 'org-fold-hide-drawer-all #'ignore))

;; Keep drawers open on startup
;; (setq org-cycle-hide-drawer-startup nil)

(defun my/org-cycle ()
  "Adapt org-cycle to fold the current code block if point is within
one. Useful for finding one's place within a large code block
without folding any headings."
  (interactive)
  (let* ((element (org-element-at-point))
         (type (org-element-type element)))
    (cond ((eq type 'src-block)
           (let* ((post (org-element-property :post-affiliated element))
                  (start (save-excursion
                           (goto-char post)
                           (line-end-position)))
                  (end (save-excursion
                         (goto-char (org-element-property :end element))
                         (skip-chars-backward " \t\n")
                         (line-end-position))))
             (when (let ((eol (line-end-position)))
                     (and (/= eol start) (/= eol end)))
               (call-interactively #'org-previous-block)))))
    (call-interactively #'org-cycle)))

(setq org-footnote-define-inline nil
      org-footnote-section "Footnotes"
      org-footnote-auto-adjust t ; Like org-footnote-normalize
      org-footnote-auto-label t)

(setq org-priority-lowest 5
      org-priority-highest 1
      org-priority-default 3)

(setq org-log-done nil)

;; Use the LOGBOOK drawer for logging
(setq org-log-into-drawer "LOGBOOK")

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

(setq org-enforce-todo-dependencies t
      org-enforce-todo-checkbox-dependencies t)

(setq org-hierarchical-todo-statistics t
      org-checkbox-hierarchical-statistics nil)

(add-hook 'org-after-todo-statistics-hook #'jkroes/org-toggle-todo)

;; A list of non-done todo states excluding CHOOSE and WAIT.
(defvar jkroes/active-todo-states '("TODO" "NOW"))

(defun jkroes/org-toggle-todo (n-done n-not-done)
  "Toggle between active todo and done keywords based on the number of
 subheadings that are marked as todo/done"
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

(advice-add #'org-update-parent-todo-statistics
            :before #'jkroes/insert-statistics-cookie)

(defun jkroes/insert-statistics-cookie (&rest _)
  (let ((state (org-get-todo-state)))
    (cond ((equal state "CHOOSE")
           ;; (org-set-property "NOBLOCKING" "t")
           (jkroes/org-toggle-radio-keyword 'on)
           (let (org-checkbox-statistics-hook)
             (org-reset-checkbox-state-subtree)))
          ((not (member state '("CHOOSE" "CHOSEN")))
           ;; (org-delete-property "NOBLOCKING")
           (jkroes/org-toggle-radio-keyword 'off))))
  (save-excursion
    ;; Ensure a cookie is inserted so that `org-toggle-todo' can trigger
    ;; recursive state change acrosss the entire subtree.
    (when (> (org-current-level) 1)
      (org-up-heading-safe)
      ;; Don't insert a cookie if one already exists
      (let* ((cookie-re "\\[\\([0-9]*\\)/\\([0-9]*\\)\\]")
             (cookie-end (re-search-forward cookie-re (line-end-position) t)))
        (unless cookie-end
          (org-end-of-line)
          (insert " [/]"))))))

(defun jkroes/org-toggle-radio-keyword (state)
  "Toggle the radio keyword above the first plain list or else next
heading"
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

;; `org-toggle-todo-checkboxes' runs `org-update-checkbox-count', and we don't
;; need it to run beforehand
(after! org-list (setcdr (assoc 'checkbox org-list-automatic-rules) nil))

(add-hook 'org-checkbox-statistics-hook #'org-toggle-todo-checkboxes)

;; BUG When another heading is at the end of the list, if the user has marked
;; the entire list with evil-visual-line (V) from the top down, point will be
;; on the other heading!
(defun org-toggle-todo-checkboxes (&rest _)
  ;; HACK Ugly hack for when another heading is at the end of the list. If the
  ;; user has marked the entire list with evil-visual-line (V) from the top
  ;; down, point will be on the other heading!
  ;; (forward-line -1)
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

(advice-add #'org-toggle-radio-button :around
            (lambda (orig-fun &rest args)
              (advice-add 'org-update-checkbox-count-maybe :override #'ignore)
              (apply orig-fun args)
              (advice-remove 'org-update-checkbox-count-maybe #'ignore)))

;; TODO This is a temporary keybinding and workaround to find a definition via
;; completing-read, until I can investigate the lookup module and whether it's
;; possible to incorporate completing read into its commands.

;; Search by completing read. If a thing is at point, it will be the first candidate
(setq xref-show-definitions-function #'xref-show-definitions-completing-read)
(map! :leader "cd"
      (cmd! (let ((current-prefix-arg '(4)))
              (call-interactively #'xref-find-definitions))))

;; Disable popup management of org-src buffer windows
(after! org
  (advice-remove #'org-edit-src-exit #'+popup--org-edit-src-exit-a)
  (assoc-delete-all "^\\*Org Src" +popup--display-buffer-alist)
  (assoc-delete-all "^\\*Org Src" display-buffer-alist))

;; TODO The first info buffer shows the modeline, but successive buffers do not.
;; Investigate the modeline rules for popups. In the meantime, disable modeline
;; hiding for popups.
(remove-hook '+popup-buffer-mode-hook #'+popup-set-modeline-on-enable-h)

;; If we bind `other-window' directly, it will remap to `ace-window' when
;; the window-select module is active. If we want to circumvent remapping, wrap
;; the remapped command in a function call.
(map! "M-o" (cmd! (call-interactively #'other-window)))

;; BUG When the top line of a window's buffer is blank, the background extends
;; to the entire line, or else the letter is invisible.
;; https://emacs.stackexchange.com/questions/45895/changing-faces-one-at-a-time-outside-customize
(after! ace-window
  (custom-set-faces!
    '(aw-leading-char-face
      :foreground "white" :background "red" :height 500)))

;; TODO Can't pass cmd! or cmd!! forms as part of `predlist'. Must be a defined
;; function, because those forms are not evaluated to yield a lambda.
(defmacro jkroes/dispatch-scroll-commands (keymap state binding fallback &rest predlist)
  "Bind a predicate dispatcher `predlist' to `binding' in `keymap' or the
keymap associated with an evil `state' symbol. If no predicate in
`predlist' succeeds, execute `fallback' if non-nil or else look
up the binding in the active keymaps."
  (declare (indent 4))
  (let ((map (or keymap (intern (format "evil-%s-state-map" state)))))
    `(general-def
       ,@(when keymap (list keymap))
       ;; TODO Can I just bind to the evil keymap instead of using state?
       ,@(when state `(:states ',state))
       ,binding
       (general-predicate-dispatch
           ;;,@(list fallback)
           ;; Fallback to the original binding if one exists
           (quote ,(or (lookup-key (symbol-value map) (kbd binding))
                       ;; Or else to the current global map
                       (lookup-key (current-global-map) (kbd binding))))
         ,@predlist))))

(jkroes/dispatch-scroll-commands nil insert "C-n" nil
  (corfu-popupinfo--visible-p)
  #'corfu-popupinfo-scroll-up-5
  (jkroes/corfu-visible-p)
  #'corfu-scroll-up
  (jkroes/embark-actions-buffer-visible)
  #'scroll-other-window)

;; (general-def :states 'insert "C-n"
;;   (general-predicate-dispatch nil
;;     (corfu-popupinfo--visible-p)
;;     #'corfu-popupinfo-scroll-up-5
;;     (jkroes/corfu-visible-p)
;;     #'corfu-scroll-up
;;     (jkroes/embark-actions-buffer-visible)
;;     #'scroll-other-window))

(jkroes/dispatch-scroll-commands nil insert "C-p" nil
  (corfu-popupinfo--visible-p)
  #'corfu-popupinfo-scroll-down-5
  (jkroes/corfu-visible-p)
  #'corfu-scroll-down
  (jkroes/embark-actions-buffer-visible)
  #'scroll-other-window-down)

;; (general-def :states 'insert "C-p"
;;   (general-predicate-dispatch nil
;;     (corfu-popupinfo--visible-p)
;;     #'corfu-popupinfo-scroll-down-5
;;     (jkroes/corfu-visible-p)
;;     #'corfu-scroll-down
;;     (jkroes/embark-actions-buffer-visible)
;;     #'scroll-other-window-down))

(jkroes/dispatch-scroll-commands nil normal "C-n" nil
  (jkroes/embark-actions-buffer-visible)
  #'scroll-other-window)

;; (general-def :states 'normal "C-n"
;;   (general-predicate-dispatch nil
;;     (jkroes/embark-actions-buffer-visible)
;;     #'scroll-other-window))

(jkroes/dispatch-scroll-commands nil normal "C-p" nil
  (jkroes/embark-actions-buffer-visible)
  #'scroll-other-window-down)

;; (general-def :states 'normal "C-p"
;;   (general-predicate-dispatch nil
;;     (jkroes/embark-actions-buffer-visible)
;;     #'scroll-other-window-down))

(jkroes/dispatch-scroll-commands minibuffer-local-map nil "C-n" #'scroll-up-command
  (jkroes/embark-actions-buffer-visible)
  #'scroll-other-window)

;; (general-def minibuffer-local-map "C-n"
;;   (general-predicate-dispatch #'scroll-up-command
;;     (jkroes/embark-actions-buffer-visible)
;;     #'scroll-other-window))

(jkroes/dispatch-scroll-commands minibuffer-local-map nil "C-p" #'scroll-down-command
  (jkroes/embark-actions-buffer-visible)
  #'scroll-other-window-down)

;; (general-def minibuffer-local-map "C-p"
;;   (general-predicate-dispatch #'scroll-down-command
;;     (jkroes/embark-actions-buffer-visible)
;;     #'scroll-other-window-down))
