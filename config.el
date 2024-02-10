;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; TODO Look into xonsh
;; TODO Learn how to align things with evil-lion.
;; TODO Look into using https://github.com/cyrus-and/zoom
;; TODO Replace dash-docs with devdocs for the +docsets flag. This will be a
;; big project, so in the meantime I am simply rebinding spc-s-k to
;; devdocs-peruse.
;; TODO Debug the font-lock-related issues that are causing emacs to hang on
;; both macos and linux. Someone on
;; https://www.reddit.com/r/emacs/comments/k7cku8/when_emacs_hangs_what_do_you_do/
;; recommended
;; https://github.com/alphapapa/emacs-package-dev-handbook?tab=readme-ov-file#tools-8
;; TODO Also use open-dribble-file to debug Emacs. Kill Emacs as soon as it
;; hangs, then examine the file for whatever keystrokes triggered the hang.
;; TODO Read the zhsell manual (not just the man pages)
;; TODO Look into https://github.com/tuh8888/chezmoi.el TODO Setup
;; https://docs.atuin.sh/ (recommended by chezmoi for tracking shell history)
;; TODO Purchase https://www.monolisa.dev/buy TODO When you edit the contents
;; of a collapsed org-src block from within an org src buffer, it expands the
;; block once you exit. It would be nice to close it again upon exiting back to
;; the org buffer. TODO Look into using the following packages plus (custom?)
;; functions to send text to the buffer for execution in a shell, as an
;; alternative to comint: https://codeberg.org/akib/emacs-eat
;; https://github.com/szermatt/mistty (misty-send-string)
;; https://www.masteringemacs.org/article/running-shells-in-emacs-overview TODO
;; C-h v over a long function or variable name when emacs is half a window on
;; my laptop leads to wrapped lnies with vertico and marginalia. I've seen this
;; issue before. Try C-h v on this: window-selection-change-functions. It might
;; be because it's displaying the text at point and somehow that is changing
;; the display. TODO Learn proper vim commands. ci" deletes inside of double
;; quotes. TODO After updating path in .zshrc, run doom env. E.g. you may have
;; to do this after gem install solargraph to make sure ruby-mode and lsp can
;; find the languageserver TODO (e)debug-on-quit can be used to debug emacs if
;; it freezes and you have to presss c-g TODO Use autoload when you only need a
;; function or command to be available and the package hasn't defined such an
;; autoload. Use require when you need the library to be loaded right away
;; rather than when a function or command is called. TODO Sometimes errors
;; aren't obvious unless you use emacs after `toggle-debug-on-error'. This
;; should be enabled most of the time. TODO Use aliases to construct multiple
;; org-roam nodes. See apps.shell.cp.org TODO Update edebug node to reflect new
;; evil-collection bindings TODO use-package statements in this file may cancel
;; out deferment that doom relies on. Use after! instead of :config where
;; possible; otherwise make sure to try and defer per Doom's defaults. TODO
;; Document setting force-load-messages to t and doom-inhibit-log to nil in
;; early-init.el. The latter for debugging evil-collection mode loading. TODO
;; Document debug-watch TODO Document the fact that you either need to doom
;; sync or `doom/reload' to see changes to module code TODO Bind ffap-menu TODO
;; Finish documenting edebug (see org-roam) TODO org column view TODO Make some
;; buffers entirely temporary (e.g. helpful) TODO evil bindings for ediff TODO
;; learn about orderless-style-dispatchers TODO c-spc previews variable help
;; pages but c-m-v does not scoll it as the other window. make help the other
;; window. TODO Shrunk org table columns show three dots. C-h . over the dots
;; echoes the truncated text. This should be done after a delay anywhere withi
;; nthe column. TODO bind ace-window TODO The best way to handle advice is
;; shown by the definition of consult-org-roam-mode TODO Functions used as
;; advice can be debugged if you use advice-add and not bullshit `defadvice!'
;; TODO Document somewhere how attempting to edebug an advised function fails
;; silently. If edebug doesn't trigger, check via C-h f whether the function is
;; advised. There are other resons why debugging might fail. TODO Create an
;; archive file instead of a graveyard section below TODO Unbind all references
;; to "C-<return>" and [C-return], "<tab>" and [tab], etc., in all packages and
;; modules that you use. It makes debugging keybindings extremely difficult and
;; there is no good reason to bind to them over RET, TAB, etc. TODO corfu
;; bindings. Use define-key! ? TODO vertico preview settings TODO Learna about
;; orderless. In particular, see orderless-affix-dispatch-list. TODO Finish
;; migrating outstanding modules from temp/ and/or master branch TODO Where
;; possible, convert advice to new functions with remaps. See
;; `describe-face-under-hl-line'. This of course won't prevent you from
;; executing original command with M-x. Also consider using `fset'. TODO Update
;; (org) advice to reflect new function definitions as of v9.6. I've already
;; done this for org-insert-heading-a.

;; See ~/.config/doom/modules/editor/file-templates/README.org
(setq user-full-name "Justin Kroes"
      user-mail-address "jkroes14@ucsbalum.com")

;; https://christiantietze.de/posts/2021/06/emacs-center-window-on-current-monitor/
;; On startup, set the font to a smaller size if working on the
;; laptop without external monitors
(setq doom-font (font-spec
                 :family "JuliaMono"
                 :size (startup-font-size)))

;; Do not prompt when killing Emacs
(setq confirm-kill-emacs nil)

;; Disable messages about available keybindings when using M-x
(setq suggest-key-bindings nil)

;; See lisp/doom-keybinds.el for additional settings
(setq which-key-idle-delay 0.3)

;; NOTE This is less useful for commands where Doom defines descriptions that
;; mask remaps
(setq which-key-compute-remaps t)

;; TODO Comment this out if you want to re-enable Doom's descriptions of
;; bindings
(setq which-key-replacement-alist nil)

;; NOTE If using the default of M-SPC instead, in Windows Terminal actions
;; settings unbind alt+space and save settings.
;; (setq doom-leader-alt-key "C-SPC")
;; (setq doom-localleader-alt-key "C-SPC m")

;; Treat command key like control
(setq ns-command-modifier 'control
      mac-command-modifier 'control)

;; On MacOS the binding here should match the shortcut in  System Settings >
;; Keyboard > Keyboard Shortcuts > Keyboard > Move focus to next window
(map! "C-SPC" #'other-frame)

;; The file where bookmarks are saved
(setq bookmark-default-file
      (concat-path doom-private-dir
                   "etc"
                   (concat "bookmarks_"
                           (cond (IS-MAC "macos")
                                 (IS-WSL "wsl")))))

;; Projectile caching is used with e.g. doom-project-find-file (SPC-f-F).
;; It's probably worth enabling for large projects, but for now it's
;; omitting file candidates that have been recently added to e.g. a
;; private module.
(setq projectile-enable-caching nil)

;; Enable which-key paging for help-map
(general-unbind help-map "C-h")

;; Makes inserting org footnotes easier. Type M-e or M-a to
;; forward- or backward-sentence, then SPC-m-f
(setq-hook! 'org-mode-hook
  sentence-end "[.?!…,;:]")

;;; titlecase -----------------------------------------------------------------

(use-package titlecase)

;;; hl-line -------------------------------------------------------------------

(defun describe-face-under-hl-line ()
  "The `hl-line' face obscures underlying faces and must be
disabled to get the underlying face. NOTE When mixing fixed- and
variable-pitch fonts within a buffer, this function will not
correctly display variable-pitch fonts. Instead use
`describe-char'."
  (interactive)
  (if hl-line-mode
      (unwind-protect
          (progn
            (hl-line-mode -1)
            (call-interactively #'describe-face))
        (hl-line-mode))
    (call-interactively #'describe-face)))


;; Inspired by ~/.config/emacs/modules/completion/ivy/config.el
(define-key! [remap describe-face] #'describe-face-under-hl-line)

;;; evil ----------------------------------------------------------------------

;; Easier evil "j" and "k", harder "gg" navigation
;; (setq display-line-numbers-type 'relative)

;;; edebug / messages -------------------------------------------------

(autoload #'edebug-instrument-function "edebug")

;; NOTE Uncomment this if not using evil-collection-edebug
;; As noted in
;; https://github.com/noctuid/evil-guide#why-dont-keys-defined-with-evil-define-key-work-immediately,
;; keymap normalization may be required in some cases. One seems to be use of
;; edebug-mode-map as an evil-intercept map. Without normalization, if in normal
;; mode SPC will trigger leader until you first switch to another evil state.
;; (add-hook 'edebug-mode-hook #'evil-normalize-keymaps)

(add-hook 'edebug-mode-hook
          (defun edebug-enter-normal-state ()
            (when edebug-mode (evil-normal-state))))

;; BUG When switching to buffer *Messages*, Emacs seems to go into an infinite
;; loop. It is caused by the following. Hang can even be triggered by consult
;; preview of *Messages* when within consult-buffer
;; Because this buffer is launched early, I have to use this instead of
;; `messages-buffer-mode-hook'
;; (with-current-buffer "*Messages*"
;;   (+word-wrap-mode)
;;   (display-line-numbers-mode))

;; Print full results to the messages buffer when evaluating expressions
(setq eval-expression-print-length nil
      eval-expression-print-level  nil
      edebug-print-length 1000)

;;; lisp/lib/sessions.el ------------------------------------------------------

;; NOTE I use sessions.el to quickly save and load frame layouts (windows and
;; buffers) so that I can quickly restart Emacs while developing its config. It
;; may have effects beyond that, but I haven't seen any major bugs yet. I prefer
;; doom/quickload-session for one-time saves, and my/load-session for
;; sessions I plan to reopen multiple times.

;; HACK Fix for https://github.com/doomemacs/doomemacs/issues/7580
(setq desktop-base-file-name ".emacs_desktop")

(map! :map doom-leader-quit/session-map
      :desc "Restore last session"         "l" #'my/quickload-session
      :desc "Save session to file"         "L" #'my/load-session)

(defun my/quickload-session ()
  (interactive)
  "Quickload without a confirmation prompt."
  (doom/quickload-session t))

(defun my/load-session ()
  "Load session from file. Do not delete the session file after loading
(use `doom/quickload-session' if you want auto-deletion). Do not prompt for
confirmation."
  (interactive)
  (let* ((desktops (cl-reduce #'remove
                              ;; Use separate session files from quickload
                              (list ".." "." desktop-base-file-name)
                              :initial-value (directory-files desktop-dirname)
                              :from-end t))
         (original (if (length= desktops 1)
                       (expand-file-name (car desktops) desktop-dirname)
                     (read-file-name "Session to restore:"
                                     (file-name-as-directory desktop-dirname)
                                     nil
                                     t)))
         (backup (concat original "_bck")))
    (copy-file original backup)
    (doom/load-session original)
    (copy-file backup original)
    (delete-file backup)))

;;; undo ----------------------------------------------------------------------

;; BUG Even if `undo-no-redo' is non-nil, if you `undo' all edits in a buffer,
;; switch to a second window, then switch back, `undo' no longer reports "No
;; further undo information." It redoes the first edit in the buffer, then
;; undoes that redo, then reports the message. `vundo' does not have this same
;; issue.

(setq undo-no-redo t)

(after! vundo
  (evil-collection-define-key 'normal 'vundo-mode-map
    "d" 'vundo-diff))

;;; modeline ---------------------------------------------------

;; Letter between brackets is more visible than an icon for evil state
(setq doom-modeline-modal-icon nil)

;; TODO The first info buffer shows the modeline, but successive buffers do not.
;; Investigate the modeline rules for popups. In the meantime, disable modeline
;; hiding for popups.
(remove-hook '+popup-buffer-mode-hook #'+popup-set-modeline-on-enable-h)

;;; windows----------------------------------------------------------------

;; BUG When the top line of a window's buffer is blank, the background extends
;; to the entire line, or else the letter is invisible.
;; https://emacs.stackexchange.com/questions/45895/changing-faces-one-at-a-time-outside-customize
(custom-set-faces
  '(aw-leading-char-face
    ((t (:foreground "white" :background "red" :height 200)))))

;; NOTE If we bind `other-window' directly, it will remap to `ace-window' when
;; the window-select module is active. If we want to circumvent remapping, wrap
;; the remapped command in a function call.
(map! "M-o" (lambda () (interactive) (call-interactively #'other-window)))

;;; default -------------------------------------------------------------------

(map! "M-RET" comment-line-break-function)

;; https://emacs.stackexchange.com/questions/22746/add-a-space-after-the-comment-delimiter
(defun comment-indent-new-line--insert-a-space-after (&rest _)
  "Ensure there is exactly one space after `comment-start'"
    (just-one-space))
(advice-add 'comment-indent-new-line :after #'comment-indent-new-line--insert-a-space-after)

;;; scrolling -----------------------------------------------------------------

;; Scroll screen to right (`scroll-left') automatically when cursor moves off
;; screen. See `hscroll-step' and `hscroll-margin' for details.
(setq auto-hscroll-mode t) ; 'current-line

;; Easier hscroll mappings
(map! "M-h" (lambda () (interactive) (evil-scroll-column-left 20))
      "M-l" (lambda () (interactive) (evil-scroll-column-right 20)))

;;; auto-fill -----------------------------------------------------------------

;; Within a comment, typing a nonspace character followed by a space beyond
;; column will cause Emacs to hard wrap your comment
(add-hook 'prog-mode-hook 'turn-on-auto-fill) ; https://www.gnu.org/software/emacs/manual/html_node/efaq/Turning-on-auto_002dfill-by-default.html
(setq comment-auto-fill-only-comments t)
(setq-default fill-column 65)

;; When auto-filling, automatically undo the effects of auto-hscroll-mode by
;; scrolling back again to the left.
;;
;; Stack trace:
;; yas--auto-fill
;; yas--original-auto-fill-function
;; do-auto-fill / normal-auto-fill-function
(advice-add 'do-auto-fill
            :around (lambda (func &rest _)
                      (when (funcall func) (scroll-right))))

;; TODO Set regexp if you need to inhibit auto-fill in specific places
;; (setq auto-fill-inhibit-regexp "")

;;; vertico -------------------------------------------------------------------

;; Notes
;;
;; Pressing "o SPC" within consult-buffer will limit candidates to org buffers.
;; See +vertico--consult-org-source
;;
;; Switch to directory (consult-dir) and recursively find file within dir
;; (consult-dir-jump-file). In particular, use when within the minibuffer.
;; consult-dir will replace the prompt of any filepath-completing function
;; with the selected dir!

;; TODO The +icons flag to the vertico module loads package
;; all-the-icons-completion, which is used by adding
;; all-the-icons-completion-marginalia-setup to marginalia-mode-hook. It
;; creates a pseudo-issue:
;; Run spc-f-f within ~/org, then embark-act. Notice the extra space between
;; the last candidate and the bottom of the frame. This is because vertico and
;; embark use the same number of candidates but different line sizes due to
;; icons. Furthermore, if you decrease all-the-icons-scale-factor and/or
;; all-the-icons-default-adjust, then scroll down the list of org notes, you'll
;; notice that the icons for longer filenames are smaller.
;;
;; Once you have fixed this issue, re-enable the +icons flag.
;;
;; Alternatively, enable vertico-resize:
;; (after! vertico (setq vertico-resize t))



(after! embark
    (map! :map embark-file-map
          ;; Replaces consult-file-externally
          (:when IS-WSL "x" #'open-in-windows)
          ;; Adds file to bookmarks
          "b" #'my/bookmark-set))

;; TODO Modify this so that it works without visiting file via find-file
(defun my/bookmark-set (file)
  "For use with embark-file-map. Bookmark the selected file and
prompt for a name, using filename as default input"
  (let ((curbuf (current-buffer)))
    ;; NOTE bookmark-set uses the current buffer. find-file opens directories
    ;; in dired. dired with e.g. /mnt/c/ throws permission errors that prevent
    ;; a bookmark's creation, but that's fine.
    (find-file file)
    (bookmark-set (read-from-minibuffer
                   "Bookmark name: "
                   (file-name-nondirectory
                    (if (f-dir-p file)
                        (directory-file-name file)
                      file))))
    (kill-buffer (current-buffer))
    (switch-to-buffer curbuf)))

(map! :map doom-leader-file-map :desc "Bookmark" "b" #'file-bookmark)

(defun file-bookmark ()
  (interactive)
  (my/bookmark-set
   (car (find-file-read-args
         "Find file: "
         (confirm-nonexistent-file-or-buffer)))))

;; NOTE I implement bookmarks mainly so I can insert them into file completion
;; functions such as find-file via consult-dir (or directly calling
;; consult-dir). Calling bookmark-jump on a directory launches dired. Dired is
;; an issue for WSL traversing Windows shared drives. It is best to avoid
;; Dired wherever possible.

;;; completion ----------------------------------------------------------------

(use-package! corfu
  :hook (doom-first-input . global-corfu-mode)
  :init
  (setq corfu-cycle nil
        corfu-auto t
        corfu-auto-prefix 1
        ;; What `corfu-insert-separator' (bound to M-SPC during completion)
        ;; insrts. (Note that Doom's default binding for doom-leader-alt-key
        ;; shadows the default binding for `corfu-insert-separator'.)
        ;; NOTE Does this need to be the same as `orderless-component-separator'?
        corfu-separator ?\s
        ;; corfu normally quits if a word boundary (e.g., space) is inserted, but
        ;; a space is permitted if it was inserted by `corfu-insert-separator'.
        corfu-quit-at-boundary 'separator
        ;; Keep corfu alive without a match, following `corfu-insert-separator'
        corfu-quit-no-match 'separator
        corfu-on-exact-match 'quit
        ;; Indent and complete on tab
        tab-always-indent 'complete
        corfu-popupinfo-delay 0.5
        ;; Less jarring to avoid hiding when `corfu-popupinfo-delay' is short
        corfu-popupinfo-hide nil)
  ;; TODO Consider making this similar to minibuffer-map
  (evil-define-key 'insert corfu-map
    [tab]         #'corfu-insert
    (kbd "TAB")   #'corfu-insert
    (kbd "C-b")   #'corfu-scroll-down
    (kbd "C-f")   #'corfu-scroll-up
    (kbd "C-,")   #'corfu-first
    (kbd "C-.")   #'corfu-last
    ;; This will shadow the binding to evil-escape in
    ;; ~/doom-emacs/modules/editor/evil/config.el
    (kbd "C-g")   #'corfu-abort
    (kbd "C-SPC") #'corfu-insert-separator
    (kbd "C-@")   #'corfu-insert-separator ; For the terminal
    ;; You can also press existing bindings for scroll-other-window(-down)
    (kbd "M-p")   #'corfu-popupinfo-scroll-down
    (kbd "M-n")   #'corfu-popupinfo-scroll-up
    (kbd "C-h")   #'corfu-info-documentation ; Works with scroll-other-window(-down)
    (kbd "C-n")   #'corfu-next
    (kbd "C-p")   #'corfu-previous)
  (advice-add 'corfu--setup :after 'evil-normalize-advice)
  (advice-add 'corfu--teardown :after 'evil-normalize-advice)
  :config
  ;;(corfu-popupinfo-mode) ; Documentation popup next to completion
  (corfu-history-mode))

(defun evil-normalize-advice (&rest _) (evil-normalize-keymaps))

;; TODO evil-org interferes with RET. This doesn't seem to be an issue in the
;; company module. See the fix for #1335 in company/config.el.
;; Elsewhere, unbinding RET when `corfu-preselect-first' is enabled allows us to
;; insert a newline without also completing
(after! corfu (define-key corfu-map (kbd "RET") nil))


(defun corfu-abort ()
  "Undo changes made while corfu was active and quit. Takes one keystroke compared
to two to three for corfu-reset but resets changes all at once rather than
incrementally."
  (interactive)
  (cancel-change-group corfu--change-group)
  (corfu-quit))


;; HACK `corfu-info-documentation' spawns a help buffer, which the popup module
;; catches, but the two are not yet compatible. `scroll-other-window' will
;; not scroll the help buffer, and "q" will quit the original buffer. (The latter
;; behavior is likely tied to corfu restoring the window configuration via
;; pre-command-hook.)
;; (defadvice! +popup--ignore-corfu-info-documentation (fn &rest args)
;;   :around #'corfu-info-documentation
;;   (push '("^\\*\\([Hh]elp\\|Apropos\\)" nil) display-buffer-alist)
;;   (apply fn args)
;;   (pop display-buffer-alist))

;; Based on https://github.com/minad/corfu/wiki, but it actually works...
;; NOTE This setup will not be torn down properly if lsp-mode is disabled.
;; See the definition of lsp-completion-mode if you want to improve the
;; code
;; (unless (featurep! :completion company)
;;   (setq lsp-completion-provider :none)
;;   ;; OPTIONAL: The first word uses orderless-flex for filtering. This means the
;;   ;; characters are matched in order but do not have to be consecutive. It
;;   ;; returns more results than a pure orderless style would, at least initially
;;   ;; (add-hook 'lsp-completion-mode-hook
;;   ;; NOTE This also affects vertico; e.g., you can't use regex like "^" on the first word
;;   ;;           (lambda ()
;;   ;;             (add-to-list 'orderless-style-dispatchers
;;   ;;                          (lambda (_pattern index _total)
;;   ;;                            (and (eq index 0) 'orderless-flex)))))
;;   ;; Make lsp completion use orderless
;;   (add-hook 'lsp-completion-mode-hook
;;             (lambda ()
;;               (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
;;                     '(orderless))))
;;   ;; Bust capf caches, refreshing candidates more often (e.g., on backspace)
;;   (add-hook 'lsp-completion-mode-hook
;;             (lambda ()
;;               (setq-local completion-at-point-functions
;;                           (list (cape-capf-buster #'lsp-completion-at-point))))))

(use-package! cape)

(use-package! kind-icon
  :after corfu
  :init (setq kind-icon-default-face 'corfu-default)
  :config (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; TODO Marginlia annotations don't seem to shift left to account for
;; left-truncated candidates.
;; Left-truncate matches/candidates in e.g. consult-line (spc s b) and
;; consult-recent-file (spc f r) and the recent file source for consult-buffer
;; (spc b b)
;; (use-package! vertico-truncate
;;   :config
;;   (vertico-truncate-mode))

;;; emacs lisp ----------------------------------------------------------------

;; modules/config/default/config.el binds this command to [C-return], which is
;; equivalent to "C-<return>" and hides any bindings to "C-RET"
(unbind-command global-map #'+default/newline-below)
(unbind-command evil-insert-state-map #'+default/newline-below)
(unbind-command evil-normal-state-map #'+default/newline-below)
(map! :map emacs-lisp-mode-map :gin "C-RET" #'eval-defun)
(map! :map emacs-lisp-mode-map :gin "C-<return>" #'eval-defun)

;;; org -----------------------------------------------------------------------

;; Where my org notes live
(setq org-directory (expand-file-name "~/org"))

;; Footnotes
(setq org-footnote-define-inline nil
                org-footnote-auto-label t
                org-footnote-auto-adjust t ; Like org-footnote-normalize
                org-footnote-section "Footnotes")

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

;; The org-transclude manual recommends removing this advice
(after! org (advice-remove 'org-link-search '+org--recenter-after-follow-link-a))

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

;; TODO Has org-attach always been scrolled to the bottom? File a bug report so
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

;; NOTE For this to work, the docs for `org-after-todo-statistics-hook' state that
;; the heading needs a statistics cookie. To insert a statistics cookie, manually
;; type "[/]" after e.g. a heading. As you toggle between todo and done for
;; subheadings, the cookie will update to display the fraction of entries marked
;; as done.
(add-hook 'org-after-todo-statistics-hook 'my/org-summary-todo)


(defun my/org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))


;; Insert a blank line when inserting a (non-sub)heading (see advice below), and
;; fold that line when cycling
(setq org-blank-before-new-entry '((heading) (plain-list-item))
      org-cycle-separator-lines 2)


(defadvice! +org/insert-item-below-a (fn &rest args)
  "Still insert a list item when on a list, but use the
built-in org functions for inserting a heading, because
`+org/insert-item-below' does not respect `org-blank-before-new-entry'."
  :around '+org/insert-item-below
  (if (org-at-heading-p)
      (call-interactively #'org-insert-heading-respect-content)
    (apply fn args)))



(defadvice! org-insert-subheading-a (fn &rest args)
  "Ignore `org-blank-before-new-entry when inserting a subheading
as the first child of a heading'"
  :around #'org-insert-subheading
  (let (org-blank-before-new-entry)
    (apply fn args)))


;; When disabled, (C-)M-RET inserts a (sub)heading above
;; when called at beginning of line; else directly below
(after! org (setq org-insert-heading-respect-content nil))

;; C-RET (+org/insert-item-below) already enters insert state, so make M-RET
;; and C-M-RET do the same. We eschew evil-org-define-(b|e)ol-command so that
;; different behavior can be used at beg and end of line
(advice-add 'org-meta-return :after #'evil-insert-state)
(advice-add 'org-insert-subheading :after (lambda (&rest _) (evil-insert-state)))
;; NOTE +org/insert-item-below-a calls this function instead of
;; +org/insert-item-below-a when at a heading
(advice-add 'org-insert-heading-respect-content :after #'evil-insert-state)

(defadvice! org-insert-heading-a (&optional arg invisible-ok top)
  "Make (C-)M-RET respect property drawers when inserting
  (sub)heading below current heading, assuming that
  `org-insert-heading-respect-content' is nil."
  :override 'org-insert-heading
  (interactive "P")
  (let* ((blank? (org--blank-before-heading-p (equal arg '(16))))
	 (level (org-current-level))
	 (stars (make-string (if (and level (not top)) level 1) ?*)))
    (cond
     ((or org-insert-heading-respect-content
	  (member arg '((4) (16)))
	  (and (not invisible-ok)
	       (invisible-p (max (1- (point)) (point-min)))))
      ;; Position point at the location of insertion.  Make sure we
      ;; end up on a visible headline if INVISIBLE-OK is nil.
      (org-with-limited-levels
       (if (not level) (outline-next-heading) ;before first headline
	 (org-back-to-heading invisible-ok)
	 (when (equal arg '(16)) (org-up-heading-safe))
	 (org-end-of-subtree invisible-ok 'to-heading)))
      ;; At `point-max', if the file does not have ending newline,
      ;; create one, so that we are not appending stars at non-empty
      ;; line.
      (unless (bolp) (insert "\n"))
      (when (and blank? (save-excursion
                          (backward-char)
                          (org-before-first-heading-p)))
        (insert "\n")
        (backward-char))
      (when (and (not level) (not (eobp)) (not (bobp)))
        (when (org-at-heading-p) (insert "\n"))
        (backward-char))
      (unless (and blank? (org-previous-line-empty-p))
	(org-N-empty-lines-before-current (if blank? 1 0)))
      (insert stars " " "\n")
      ;; Move point after stars.
      (backward-char)
      ;; When INVISIBLE-OK is non-nil, ensure newly created headline
      ;; is visible.
      (unless invisible-ok
        (if (eq org-fold-core-style 'text-properties)
	    (cond
	     ((org-fold-folded-p
               (max (point-min)
                    (1- (line-beginning-position)))
               'headline)
	      (org-fold-region (line-end-position 0) (line-end-position) nil 'headline))
	     (t nil))
          (pcase (get-char-property-and-overlay (point) 'invisible)
	    (`(outline . ,o)
	     (move-overlay o (overlay-start o) (line-end-position 0)))
	    (_ nil)))))
     ;; At a headline...
     ((org-at-heading-p)
      (cond ((bolp)
	     (when blank? (save-excursion (insert "\n")))
	     (save-excursion (insert stars " \n"))
	     (unless (and blank? (org-previous-line-empty-p))
	       (org-N-empty-lines-before-current (if blank? 1 0)))
	     (end-of-line))
	    ((and (org-get-alist-option org-M-RET-may-split-line 'headline)
		  (org-match-line org-complex-heading-regexp)
		  (org-pos-in-match-range (point) 4))
	     ;; Grab the text that should moved to the new headline.
	     ;; Preserve tags.
	     (let ((split (delete-and-extract-region (point) (match-end 4))))
	       (if (looking-at "[ \t]*$") (replace-match "")
		 (org-align-tags))
	       (end-of-line)
	       (when blank? (insert "\n"))
	       (insert "\n" stars " ")
	       (when (org-string-nw-p split) (insert split))))
	    (t
             ;; HACK Insert heading after property drawer
             (re-search-forward org-property-end-re
                                (save-excursion
                                  (outline-next-heading)
                                  (point))
                                t)
	     (end-of-line)
	     (when blank? (insert "\n"))
	     (insert "\n" stars " "))))
     ;; On regular text, turn line into a headline or split, if
     ;; appropriate.
     ((bolp)
      (insert stars " ")
      (unless (and blank? (org-previous-line-empty-p))
        (org-N-empty-lines-before-current (if blank? 1 0))))
     (t
      (unless (org-get-alist-option org-M-RET-may-split-line 'headline)
        (end-of-line))
      (insert "\n" stars " ")
      (unless (and blank? (org-previous-line-empty-p))
        (org-N-empty-lines-before-current (if blank? 1 0))))))
  (run-hooks 'org-insert-heading-hook))

;; TODO Bind this to something
(defun insert-org-entity ()
  "A dumb replacement for counsel-org-entity. See `org-pretty-entities'."
  (interactive)
  (let* ((str (completing-read
               "Entity: "
               (cl-loop for element in (append org-entities org-entities-user)
                        unless (or (stringp element)
                                   (string-prefix-p "_" (car element))) ; some hspace elements
                        collect (cons
                                 (format "%s | %s"
                                         (cl-first element)    ; name
                                         (cl-seventh element)) ; utf-8
                                 element))))
         (latex (concat "\\" (nth 0 (split-string str "|" t " ")))))
    (insert latex)))


;; TODO Replace this function with advice and remove the reference to it in
;; `+org-dwim-at-point'
;;
;; Note that this function is used by `+org/dwim-at-point' for links
;; when hitting RET
(defun my/org-open-at-point ()
  "Use find-file with file link as initial input instead of opening file link
to a directory in dired. This is hacky but way faster than using dired on
shared drives."
  (interactive)
  (let* ((context
	  ;; Only consider supported types, even if they are not the
	  ;; closest one.
	  (org-element-lineage
	   (org-element-context)
	   '(citation citation-reference clock comment comment-block
             footnote-definition footnote-reference headline
             inline-src-block inlinetask keyword link node-property
             planning src-block timestamp)
	   t)))
    (if (and (eq (car context) 'link)
             (string= (plist-get (car (cdr context)) :type) "file")
             (f-dir-p (plist-get (car (cdr context)) :path)))
        (let ((default-directory (plist-get (car (cdr context)) :path)))
          (call-interactively #'find-file))
        (call-interactively #'org-open-at-point))))

;;;; org-cycle / org-fold -----------------------------------------------------

;; Keep drawers open
(setq org-cycle-hide-drawer-startup nil)

;; Don't fold file-level drawer when cycling. Despite the changelog for org
;; v9.6, drawer folding state is not preserved for the file-level drawer.
(after! org-fold (fset 'org-fold-hide-drawer-all #'ignore))

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




;;;; org-file -----------------------------------------------------------------

;; NOTE This also affects org-attach-open
;; Open these non-text filetypes in Windows instead of WSL
(after! org
  (when IS-WSL
    (setq org-file-apps
          '(("\\.pptx?\\'" . open-in-windows)
            ("\\.pdf?\\'" . open-in-windows)
            ("\\.docx?\\'" . open-in-windows)
            ("\\.txt?\\'" . open-in-windows)
            ("\\.xlsx?\\'" . open-in-windows)
            ("\\.csv?\\'" . open-in-windows)
            ("\\.png?\\'" . open-in-windows)
            ("\\.html?\\'" . open-in-windows)
            (remote . emacs)
            (auto-mode . emacs)
            ;; dired is unbelievably slow on Windows shared network drives
            (directory . open-in-windows))
          browse-url-generic-program "/mnt/c/Windows/System32/cmd.exe"
          browse-url-generic-args '("/c" "start" "")
          browse-url-browser-function 'browse-url-generic)))


;;;; org-src / org-babel ------------------------------------------------------

;; NOTE Large code blocks can slow down `org-cycle-global' noticeably when
;; code block native fontification is enabled
(setq org-src-fontify-natively t)

;; Edit org src block in the same window as org file
(after! org
  (set-popup-rule! "^\\*Org Src" :ignore t)
  (advice-remove 'org-edit-src-exit '+popup--org-edit-src-exit-a)
  (setq org-src-window-setup 'current-window))

;; Normalize evil bindings in org src windows
(add-hook! 'org-src-mode-hook #'evil-normalize-keymaps)

;; Save edits and exit back to org file with "q"
(setq org-src-ask-before-returning-to-edit-buffer nil)
(map! :map org-src-mode-map :n "q" #'my/org-edit-src-save-and-exit)
(defun my/org-edit-src-save-and-exit ()
  (interactive)
  (org-edit-src-save)
  (org-edit-src-exit)
  ;; Prevents accidental text insertion
  (evil-normal-state)
  ;; Assume we want a folded block, because editing a block unfolds it. Since
  ;; we are exiting a src block, we don't need to test whether we are within a block.
  (org-end-of-line)
  (when (not (org-fold-folded-p))
    (my/org-cycle)))


;; BUG This advice was creating huge lags for org-babel-tangle
(after! org
  (advice-remove 'org-export-to-file '+org--dont-trigger-save-hooks-a)
  (advice-remove 'org-babel-tangle '+org--dont-trigger-save-hooks-a))

;;;; org-element --------------------------------------------------------------

;; TODO Prior to org v9.6, org-element threw a lot of errors about invalidated
;; cache. Disable the cache if you are still having issues. org v9.6 refactored
;; the cache and encouraged users to test it out. See
;; https://orgmode.org/Changes.html.
;;
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

;; All of my org files are org-roam files
(setq org-roam-directory org-directory)

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


;;; ediff (doom module development) -------------------------------------------

(setq ediff-split-window-function #'split-window-vertically)

;; The following results in a message contained in `ediff-KILLED-VITAL-BUFFER':
;; (add-hook 'ediff-meta-mode-hook #'kill-buffer). Instead, use this to kill
;; *Ediff Registry* buffer, which seems to spawn automatically in response to
;; `ediff' being invoked, but is one of the few ediff buffers that does not
;; clean itself up.
(add-hook 'ediff-quit-hook (lambda () (kill-buffer "*Ediff Registry*")))

;; Run ediff in another frame. See lisp/doom-ui.el.
(after! ediff
  (remove-hook 'ediff-before-setup-hook #'doom-ediff-save-wconf-h)
  (add-hook 'ediff-before-setup-hook
            (defun ediff-in-new-frame () (select-frame (make-frame))))
  (remove-hook 'ediff-quit-hook #'doom-ediff-restore-wconf-h)
  (remove-hook 'ediff-suspend-hook #'doom-ediff-restore-wconf-h)
  (add-hook! 'ediff-quit-hook :append #'delete-frame))

;; When using my/ediff-doom-private-module, it can be useful to copy the
;; contents of one or both diff buffers for pasting into a third buffer
(add-hook 'ediff-startup-hook #'my/ediff-mode-bindings)
(defun my/ediff-mode-bindings ()
  (map! :map ediff-mode-map
        "ca" #'ediff-copy-A
        "cb" #'ediff-copy-B))
(defun ediff-copy-A (arg)
  (interactive "P")
  (my/ediff-to-diff arg "a"))
(defun ediff-copy-B (arg)
  (interactive "P")
  (my/ediff-to-diff arg "b"))
(defun my/ediff-to-diff (arg &optional keys)
  (interactive "P")
  (ediff-barf-if-not-control-buffer)
  (or keys (setq keys (this-command-keys)))
  (if (eq arg '-) (setq arg -1)) ; translate neg arg to -1
  (if (numberp arg) (ediff-jump-to-difference arg))

  (let* ((char1 (aref keys 0)))
    (my/ediff-copy-diff ediff-current-difference
                        (ediff-char-to-buftype char1))))
(defun my/ediff-copy-diff (n from-buf-type)
  (let* ((reg-to-copy (ediff-get-region-contents n from-buf-type ediff-control-buffer)))
    (kill-new reg-to-copy)))

(defun doom/copy-module-to-private (category module)
  "Copy the Doom module corresponding to the strings category and module as a
private module."
  (interactive
   (mapcar #'intern
           (split-string
            (completing-read "Copy module:"
                             (doom-private-modules-list)
                             nil t)
            " " t)))
  (let* ((path (doom-module-locate-path category module))
         (newpath (replace-regexp-in-string doom-modules-dir
                                            (car doom-modules-dirs) path)))
    (copy-directory path newpath nil t t)))

(autoload #'ediff-read-file-name "ediff")
(autoload #'ediff-files-internal "ediff")
(defun my/ediff-doom-private-module (file-A file-B &optional skip-prompt)
  "Prompt for a private Doom module, then a file with differences
from the corresponding non-private file. If the current buffer is
a private module file, use its module and `buffer-file-name' as
the default values for the two prompts."
  (interactive
   (let* ((mod (current-doom-module-string))
          (file-B
           ;; Per https://howardism.org/Technical/Emacs/alt-completing-read.html,
           ;; completing-read accepts an alist but only uses the car of each alist as
           ;; candidates; however, we can access the cadr within the PREDICATE func
           (completing-read
            "Private module file: "
            (module-files-with-diffs
             (apply #'doom-module-get
                    (append
                     (mapcar #'intern
                             (split-string
                              ;; Private modules with default of the current module
                              (completing-read
                               "Private module: "
                               (doom-private-modules-list
                                (directory-files
                                 (car doom-modules-dirs) t))
                               nil t nil nil mod)))
                     (list :path))))
            ;; If DEF is non-nil but not one of the candidates, the first
            ;; candidate will not be selected. If `mod' is nil,
            ;; then `buffer-file-name' will not be a candidate.
            nil t nil nil (when mod buffer-file-name)))
          (file-A (replace-regexp-in-string
                   (car doom-modules-dirs)
                   doom-modules-dir
                   file-B)))
     (list file-A file-B)))
  (ediff-files-internal file-A file-B nil nil 'ediff-files))

(defun current-doom-module ()
  (when buffer-file-name
    (when-let (mod (doom-module-from-path buffer-file-name))
      (unless (memq (car mod) '(:core :user))
        mod))))

(defun current-doom-module-string ()
  (when-let (mod (current-doom-module))
    (format "%s %s" (car mod) (cdr mod))))

(defun doom-private-modules-list (&optional paths-or-all)
  (cl-loop for (cat . mod) in
           (doom-module-list paths-or-all)
           for format = (format "%s %s" cat mod)
           if mod ; Exclude (:core) and (:user)
           collect format))

(defun module-files-with-diffs (module-path)
  (-filter #'diff-file-between-modules
           (directory-files-recursively module-path "")))

(defun diff-file-between-modules (private-file)
  (let* ((private-file (expand-file-name private-file))
         (doom-file (replace-regexp-in-string
                     (car doom-modules-dirs)
                     doom-modules-dir
                     private-file)))
    (diff2 doom-file private-file)))

(defun diff2 (old new &optional switches)
  (interactive
   (let* ((newf (if (and buffer-file-name (file-exists-p buffer-file-name))
		    (read-file-name
		     (concat "Diff new file (default "
			     (file-name-nondirectory buffer-file-name) "): ")
		     nil buffer-file-name t)
		  (read-file-name "Diff new file: " nil nil t)))
          (oldf (file-newest-backup newf)))
     (setq oldf (if (and oldf (file-exists-p oldf))
		    (read-file-name
		     (concat "Diff original file (default "
			     (file-name-nondirectory oldf) "): ")
		     (file-name-directory oldf) oldf t)
		  (read-file-name "Diff original file: "
				  (file-name-directory newf) nil t)))
     (list oldf newf (diff-switches))))
  (diff-no-select2 old new switches))

(autoload #'diff-check-labels "diff")
(defun diff-no-select2 (old new &optional switches)
  (unless (bufferp new) (setq new (expand-file-name new)))
  (unless (bufferp old) (setq old (expand-file-name old)))
  (or switches (setq switches diff-switches)) ; If not specified, use default.
  (unless (listp switches) (setq switches (list switches)))
  (diff-check-labels)
  (let* ((old-alt (diff-file-local-copy old))
	 (new-alt (diff-file-local-copy new))
	 (command
	  (mapconcat #'identity
		     `(,diff-command
		       ;; Use explicitly specified switches
		       ,@switches
                       ,@(mapcar #'shell-quote-argument
                                 (nconc
                                  (and (or old-alt new-alt)
				       (eq diff-use-labels t)
				       (list "--label"
					     (if (stringp old) old
					       (prin1-to-string old))
					     "--label"
					     (if (stringp new) new
					       (prin1-to-string new))))
                                  (list (or old-alt old)
                                        (or new-alt new)))))
		     " ")))
    (diff-sentinel2
     (call-process shell-file-name nil nil nil
		   shell-command-switch command)
     old-alt new-alt)))

(defun diff-sentinel2 (code &optional old-temp-file new-temp-file)
  (if old-temp-file (delete-file old-temp-file))
  (if new-temp-file (delete-file new-temp-file))
  (cond ((equal 0 code) nil)
        ((equal 1 code) t)
        ;; TODO Handle errors reported by the diff binary
	((equal 2 code) nil)))

;;; helpful

;; TODO Bind this for other modes with buffers that should be easily killable
;; by pressing "q"
(map! :map helpful-mode-map
      "q" (lambda () (interactive) (quit-window t)))

;;; Keybindings ---------------------------------------------------------------

;; TODO Replace these with the sp-* commands? Investigate the differences
;; between the built-in and smartparens commands
;; NOTE These commands require evil-move-beyond-eol to work properly across
;; multiple lines. What are the cons of enabling this setting? Also see
;; recommendations for evil and lisp editing in
;; https://www.reddit.com/r/emacs/comments/rbf31m/does_anybody_else_find_evil_very_painful_for/
;; and discussion of cursor movement in
;; https://www.dr-qubit.org/Evil_cursor_model.html and additional commands in
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/List-Motion.html
(map! :map emacs-lisp-mode-map
      :when evil-move-beyond-eol
      :m "<up>" #'up-list
      :m "<down>" #'down-list
      :m "<left>" #'backward-list
      :m "<right>" #'forward-list)

;; https://www.masteringemacs.org/article/emacs-builtin-elisp-cheat-sheet
;; TODO Replace the bindings below with this. I dono't think evil-collection is
;;loaded without the +everywhere flag for evil, so do this yourself. Or
;;re-enable +everywhere and disable any tweaks/keybindings you dislike. Also
;; modify the white/blacklisted evil-collection modes. See the evil module.
;;(push 'shortdoc evil-collection-mode-list)
;;
;; (map! :map shortdoc-mode-map
;;       :m "j" #'forward-button
;;       :m "<tab>" #'forward-button
;;       :m "k" #'backward-button
;;       :m "<backtab>" #'backward-button)

;;; comint --------------------------------------------------------------------
(setq comint-prompt-read-only nil ; Read-only prompt in ess-R
      comint-scroll-to-bottom-on-input t
      ;; Prefer this to `comint-scroll-to-bottom-on-output'
      comint-scroll-show-maximum-output t
      comint-use-prompt-regexp nil) ; nil enables evil motions

;;; help ----------------------------------------------------------------------

(add-hook 'help-mode-hook #'visual-line-mode)

;;; misc --------------------------------------------------------------------

;; (defun evil-insert-state-on-window-selection ()
;;   "When switching to modes such as inf-ruby-mode, enter insert mode."
;;   (when (memq major-mode '(inf-ruby-mode))
;;     (evil-insert-state)))

;; (add-hook 'doom-switch-window-hook
;;           #'evil-insert-state-on-window-selection)

;;; launchschool --------------------------------------------------------------

(setq-hook! 'js-mode-hook tab-width 2)

;;; toml ----------------------------------------------------------------------

(setq-hook! 'conf-toml-mode-hook tab-width 2)
(setq-hook! 'conf-unix-mode-hook tab-width 2)

;;; chezmoi (go) templates ----------------------------------------------------

(push '("\\.tmpl" . go-template-mode) auto-mode-alist)
(require 'go-template-mode)

;;; popup (experimental) ------------------------------------------------------

;; See the hack in ~/.config/doom/modules/ui/popup/autoload/popup.el

;; (eval-after-load (concat-path (doom-module-locate-path :ui 'popup)
;;                               "autoload/popup.el")
;;   (fset '+popup-kill-buffer-hook-h #'ignore))

;; (advice-remove 'kill-current-buffer
;;                #'doom--switch-to-fallback-buffer-maybe-a)

;; (advice-remove 'kill-current-buffer
;;                #'doom-set-jump-a)

;; Stack trace with the above settings:
;;
;; +popup--delete-window(#<window 127 on *eglot-help for greeting*>)
;; delete-window(#<window 127 on *eglot-help for greeting*>)
;; window--delete(#<window 127 on *eglot-help for greeting*> t t)
;; replace-buffer-in-windows(#<buffer *eglot-help for greeting*>)
;; kill-current-buffer()
;; funcall-interactively(kill-current-buffer)
;; command-execute(kill-current-buffer record)
;; execute-extended-command(nil "kill-current-buffer" nil)
;; funcall-interactively(execute-extended-command nil "kill-current-buffer" nil)
;; command-execute(execute-extended-command)

(with-eval-after-load 'evil-collection-devdocs
  (evil-collection-define-key 'normal 'devdocs-mode-map
    ;; Same binding as Info-top-node
    "gt" 'devdocs-first-page))

(set-popup-rule! "^\\*devdocs\\*$" :size 0.3 :quit 'current)

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
