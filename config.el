;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Treat command key like control
(setq ns-command-modifier 'control
      mac-command-modifier 'control)

(when IS-WSL
  (setq browse-url-generic-program "/mnt/c/Windows/System32/cmd.exe"
        browse-url-generic-args '("/c" "start" "")
        browse-url-browser-function 'browse-url-generic))

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

;; Projectile caching is used with e.g. doom-project-find-file (SPC-f-F).
;; It's probably worth enabling for large projects, but for now it's
;; omitting file candidates that have been recently added to e.g. a
;; private module.
(setq projectile-enable-caching nil)

(use-package! titlecase :defer t)

;; Center and focus Emacs frame on launch
(select-frame-set-input-focus (selected-frame))

;;; helpful -------------------------------------------------------------------

;; BUG The original function expects a list but does not ensure that it
;; receives a list. Because it shouldn't have to. which-key incorrectly
;; specifies `defcustom' :package-version as a string, in contrast to most
;; other packages. See the documentation for `defcustom', which details the
;; exepctation for :package-version.

;; TODO File an issue with which-key
(advice-add #'helpful--version-info :override #'my/helpful--version-info)

;;; modules/editor/evil -------------------------------------------------------

;;(set-evil-initial-state! '(dired-mode) 'emacs)

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

;; TODO C-. and C-, are generally undefined and are also good candidates
;; for vertico-repeat/vertico-repeat-select instead of embark-collect, as
;; well as for binding in vertico-map and corfu-map for scrolling
;; Consider also M-n and M-p, which are only used to scan history in the
;; minibuffer.

(setq doom-theme 'modus-vivendi)
;; TODO On WSL JuliaMono doesn't render org-modern stars, nor does
;; startup-font-size return a value for the font size
(setq doom-font
      (if IS-WSL
          (font-spec :family "Hack")
        (font-spec :family "JuliaMono" :size (jkroes/startup-font-size))))

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

;; Increase the visibility of the evil state indicator
(setq doom-modeline-modal-icon nil)

;; Hide commands in M-x which do not work in the current mode
(setq read-extended-command-predicate
      #'command-completion-default-include-p)

;; Useful in conjunction with `enable-recursive-minibuffers'
(minibuffer-depth-indicate-mode)

;; Print full or long results to the messages buffer when evaluating
;; expressions
(setq eval-expression-print-length nil
      eval-expression-print-level  nil
      edebug-print-length 1000)

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

(setq-default fill-column 79)
(setq comment-auto-fill-only-comments t)

;; Typing a nonspace character followed by a space beyond column will cause
;; Emacs to hard wrap your comment
(add-hook 'prog-mode-hook 'turn-on-auto-fill)

(defadvice! jkroes/scroll-right-on-auto-fill (fn &rest _)
  "When auto-filling, automatically undo the effects of
 auto-hscroll-mode by scrolling back again to the left."
  :around 'do-auto-fill
  (when (funcall fn) (scroll-right)))

;; See lisp/doom-keybinds.el for additional settings
(setq which-key-idle-delay 0.1)

;; This masks Doom's description of bindings for remaps only (e.g. "SPC h b b")
(setq which-key-compute-remaps t)

;; BUG Disable Doom's descriptions of bindings. If the user rebinds keys with map!
;; but doesn't specify :desc, the :desc from previous bindings via map! still
;; shows up for some reason. Unfortunately, this also strips some useful
;; descriptions.
;;
;; (setq which-key-replacement-alist nil)

;; TODO Can't pass cmd! or cmd!! forms as part of `predlist'. Must be a defined
;; function, because those forms are not evaluated to yield a lambda.
(defmacro jkroes/dispatch-scroll-commands (keymap state binding fallback &rest predlist)
  "Bind a predicate dispatcher `predlist' to `binding' in `keymap' or the
keymap associated with an evil `state' symbol. If no predicate in
`predlist' succeeds, execute `fallback' if non-nil or else look
up the binding in the active keymaps."
  (declare (indent 4))
  (let* ((map (or keymap (intern (format "evil-%s-state-map" state))))
         ;; If no predicate matches, fall back to the fallback argument or to
         ;; the previous binding in map
         (command (or fallback
                      (lookup-key (symbol-value map)
                                  (kbd binding)))))
    `(general-def
       ,@(when keymap (list keymap))
       ;; TODO Can I just bind to the evil keymap instead of using state?
       ,@(when state `(:states ',state))
       ,binding
       (general-predicate-dispatch #',command
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

(jkroes/dispatch-scroll-commands minibuffer-local-map nil "C-n" scroll-up-command
  (jkroes/embark-actions-buffer-visible)
  #'scroll-other-window)

(jkroes/dispatch-scroll-commands minibuffer-local-map nil "C-p" scroll-down-command
  (jkroes/embark-actions-buffer-visible)
  #'scroll-other-window-down)

;; TODO This is a temporary keybinding and workaround to find a definition via
;; completing-read, until I can investigate the lookup module and whether it's
;; possible to incorporate completing read into its commands.

;; Search by completing read. If a thing is at point, it will be the first candidate
(setq xref-show-definitions-function #'xref-show-definitions-completing-read)
(map! :leader "cd"
      (cmd! (let ((current-prefix-arg '(4)))
              (call-interactively #'xref-find-definitions))))

(when (modulep! :editor evil)
  (unbind-command #'undo global-map)
  (unbind-command #'undo-redo global-map))

;; TODO Binding is overriden in org-mode. Need to find an alternative binding
;; sequence. Then again, org-mode typically uses visual-line-mode...
(map! "M-h" (lambda () (interactive) (evil-scroll-column-left 40))
      "M-l" (lambda () (interactive) (evil-scroll-column-right 40)))

;; HACK vertico changes the behavior of the ~completing-read~ argument
;; ~require-match~ when it is a function. If the function returns nil, the
;; minibuffer should not exit even if the input matches a completion candidate.
;; This hack fixes that behavior. If you encounter issues with completing in
;; other commands, make sure this hack is not the source.
(advice-add #'vertico--match-p :override #'jkroes/vertico--match-p)

(defun jkroes/vertico--match-p (input)
  "Return t if INPUT is a valid match."
  (let ((rm minibuffer--require-match))
    (or (memq rm '(nil confirm-after-completion))
        (equal "" input) ;; Null completion, returns default value
        (if (functionp rm)
            (funcall rm input) ;; Emacs 29 supports functions
          (test-completion input minibuffer-completion-table minibuffer-completion-predicate))
        (if (eq rm 'confirm) (eq (ignore-errors (read-char "Confirm")) 13)
          (minibuffer-message "Match required") nil))))

(when (featurep :system 'macos)
  (setq consult-locate-args "mdfind"))

(setq consult-dir-shadow-filenames nil)

(setq consult-dir-jump-file-command
      (cmd! (call-interactively #'+default/find-file-under-here)))

(after! orderless
  (add-to-list 'completion-category-overrides
        '(project-file (styles +vertico-basic-remote orderless partial-completion))))

(after! orderless
  (setq orderless-style-dispatchers '(jkroes/orderless-dispatch)))

(defun jkroes/orderless-dispatch (pattern _index _total)
  (cond
   ;; Ensure $ works with Consult commands, which add disambiguation suffixes
   ((string-suffix-p "$" pattern)
    `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x200000-\x300000]*$")))
   ;; Ignore single !
   ((string= "!" pattern) `(orderless-literal . ""))
   ;; Without literal
   ((string-prefix-p "!" pattern) `(orderless-without-literal . ,(substring pattern 1)))
   ;; Annotation
   ((string-prefix-p "," pattern) `(orderless-annotation . ,(substring pattern 1)))
   ((string-suffix-p "," pattern) `(orderless-annotation . ,(substring pattern 0 -1)))
   ;; Character folding
   ((string-prefix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 1)))
   ((string-suffix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 0 -1)))
   ;; Initialism matching
   ((string-prefix-p "`" pattern) `(orderless-initialism . ,(substring pattern 1)))
   ((string-suffix-p "`" pattern) `(orderless-initialism . ,(substring pattern 0 -1)))
   ;; Literal matching
   ((string-prefix-p "=" pattern) `(orderless-literal . ,(substring pattern 1)))
   ((string-suffix-p "=" pattern) `(orderless-literal . ,(substring pattern 0 -1)))
   ;; Flex matching
   ((string-prefix-p "~" pattern) `(orderless-flex . ,(substring pattern 1)))
   ((string-suffix-p "~" pattern) `(orderless-flex . ,(substring pattern 0 -1)))))

(setq embark-confirm-act-all nil)

(setq embark-cycle-key "C-;")

(setq embark-mixed-indicator-delay which-key-idle-delay)

(setq embark-help-key "C-h")

(setq embark-keymap-prompter-key ",")

(after! vertico-multiform
  (add-to-list 'vertico-multiform-categories
               '(embark-keybinding grid)))

;; (setq embark-prompter 'embark-completing-read-prompter)

(after! embark
  (when (eq embark-prompter 'embark-completing-read-prompter)
    (setq embark-indicators
          (remove 'embark-mixed-indicator embark-indicators))))

(after! (embark which-key)
  (cl-nsubstitute #'embark-mixed-indicator
                  #'+vertico-embark-which-key-indicator
                  embark-indicators)
  (advice-remove #'embark-completing-read-prompter
                 #'+vertico--embark-which-key-prompt-a))

(setq prefix-help-command #'jkroes/embark-prefix-help-command)

(defun jkroes/embark-prefix-help-command (&rest _)
  (interactive)
  (let (keys)
    (if (which-key--popup-showing-p)
        (progn
          (setq keys (which-key--current-prefix))
          (which-key--hide-popup-ignore-command))
      (setq keys (this-command-keys-vector))
      (setq keys (seq-take keys (1- (length keys)))))
    (my/embark-prefix-bindings keys)))

(autoload #'embark-completing-read-prompter "embark")

;; Later versions of embark altered this function so that it no longer
;; filters bindings by the current key prefix. This is the original definition
;; from commit 35f3961cd1e6
(defun my/embark-prefix-bindings (&optional prefix)
  "Explore all current keybindings and commands with `completing-read'.
The selected command will be executed. The set keybindings can be restricted
by passing a PREFIX key."
  (let ((keymap (if prefix
                    (key-binding prefix)
                  (make-composed-keymap (current-active-maps t)))))
    (unless (keymapp keymap)
      (user-error "No keybindings found"))
    (when-let (command (embark-completing-read-prompter keymap 'no-default))
      (call-interactively command))))

(setq which-key-use-C-h-commands t)

(map! :map which-key-C-h-map
      "h" #'jkroes/embark-prefix-help-command
      "C-h" #'jkroes/embark-prefix-help-command)

;; The pager text is rendered by replacing each command with its key in
;; `which-key-C-h-map'
(after! which-key
  (setq which-key-C-h-map-prompt
        (string-replace "\\[which-key-show-standard-help]"
                        "\\[jkroes/embark-prefix-help-command]"
                        which-key-C-h-map-prompt)))

(advice-add #'marginalia-annotate-binding
            :override #'my/marginalia-annotate-binding)

(after! marginalia
  (setf (car (alist-get 'symbol marginalia-annotator-registry))
        'my/marginalia-annotate-symbol))

(defun my/marginalia-annotate-binding (cand)
  "Annotate command CAND with keybinding. If CAND is remapped to
  OTHER-COMMAND, return [remap OTHER-COMMAND]."
  (when-let* ((sym (intern-soft cand))
              (key (and (commandp sym) (where-is-internal sym nil 'first-only))))
    (let ((remap (command-remapping sym)))
      (propertize (format " (%s)" (if remap remap (key-description key)))
                  'face 'marginalia-key))))

(defun my/marginalia-annotate-symbol (cand)
  (when-let (sym (intern-soft cand))
    (marginalia--fields
     (:left (marginalia-annotate-binding cand))
     ((marginalia--symbol-class sym) :face 'marginalia-type)
     ((cond
       ((fboundp sym) (marginalia--function-doc sym))
       ((facep sym) (documentation-property sym 'face-documentation))
       (t (documentation-property sym 'variable-documentation)))
      :truncate 1.0 :face 'marginalia-documentation)
     ;; ((abbreviate-file-name (or (symbol-file sym) ""))
     ;;  :truncate -0.5 :face 'marginalia-file-name)
     )))

(map! :when (modulep! :editor evil)
      :map vertico-map
      ;; "C-SPC" #'+vertico/embark-preview
      "C-j"   #'vertico-next
      "M-j" #'vertico-next-group
      ;; Shadows `kill-line', but S-<backspace> and C-S-<backspace> are still
      ;; available
      "C-k"   #'vertico-previous
      "M-k" #'vertico-previous-group)

(map! :map embark-file-map
      ;; When Emacs runs on WSL, open files externally in Windows
      (:when IS-WSL "x" #'open-in-windows)
      ;; Adds file to bookmarks
      "b" #'my/bookmark-set)

;; Where my org notes live
(setq org-directory (expand-file-name "~/org"))

;; All of my org files are org-roam files
(setq org-roam-directory org-directory)

(after! org
  (setq org-highlight-latex-and-related '(native script entities)))

(use-package! org-appear
  :hook (org-mode . org-appear-mode)
  :init
  ;; Hide emphasis markers
  (setq org-hide-emphasis-markers t
        org-appear-autoemphasis t)

  ;; Replace link with description text
  (setq org-link-descriptive t
        ;; You can always edit links with spc-m-l-l
        org-appear-autolinks nil)

  ;; Render subscripts/superscripts and Org entities
  (setq org-pretty-entities t
        ;; Requires brackets to render when `org-use-sub-superscripts' is `{}'.
        ;; E.g. r_{1} or r^{1}.
        org-appear-autosubmarkers t
        ;; E.g. \ast
        org-appear-autoentities t)

  ;; Hide listed keywords. org-modern has a setting that hides #+ instead.
  ;; (setq org-hidden-keywords '(title)
  ;;       org-appear-autokeywords t)

  ;; Render subscripts/superscripts and Org entities inside latex
  ;; fragments
  (setq org-appear-inside-latex nil)

  ;; Toggle org-appear off after idling over an element
  (setq org-appear-trigger #'always
        org-appear-delay 0.5))

(use-package! org-modern
  :hook ((org-mode . org-modern-mode)
         ;; TODO No image of this is available, and I can't see a difference...
         (org-agenda-finalize . org-modern-agenda))
  :init
  ;; TODO Customize org-modern settings:
  ;;   org-modern-fold-stars
  ;;   org-modern-footnote
  ;;   org-modern-list
  ;;   org-modern-checkbox
  ;;   org-modern-tag-faces
  ;;   org-modern-block-fringe (incompatible with org-indent)
  ;;   org-modern-keyword
  ;;   org-modern-radio-target
  ;;   org-modern-internal-target
  ;;   org-modern-progress

  ;; org-modern does not use `org-todo-keyword-faces'. The car of each alist
  ;; element should match an element in `org-todo-keywords'
  (setq org-modern-todo-faces
        '(("NOW" :inherit org-done :inverse-video t)
          ("WAIT" :inherit org-warning :inverse-video t)
          ;; NOTE If you inherit explivitly from org-modern-done or
          ;; org-modern-todo, the label will be smaller than other labels,
          ;; possibly because those faces explicitly inherit from
          ;; org-modern-label, which sets :height to 0.8, while faces in
          ;; `org-modern-todo-faces' automatically inherit from org-modern-label.
          ;; I'm guessing the reduced height is applied multiple times
          ;; multiplicatively.
          ("KILL" :background "gray20" :foreground "red")))

  ;; Hide keywords prefix. org-appear has a setting that hides the entire
  ;; keyword instead.
  (setq org-modern-keyword t)

  ;; org settings

  (setq-hook! 'org-mode-hook line-spacing 0.3)

  (setq org-auto-align-tags nil
        org-catch-invisible-edits 'show-and-error

        ;; Agenda styling
        org-agenda-tags-column 0
        org-agenda-block-separator ?─
        org-agenda-time-grid
        '((daily today require-timed)
          (800 1000 1200 1400 1600 1800 2000)
          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
        org-agenda-current-time-string
        "◀── now ─────────────────────────────────────────────────")

  ;; Ellipsis styling
  (setq org-ellipsis " ")
  (after! org-faces
    (set-face-attribute 'org-ellipsis nil :inherit 'default :box nil)))

;;; https://jft.home.blog/2019/07/17/use-unicode-symbol-to-display-org-mode-c

;; (defun prettify-org-checkboxes ()
;;   (push '("[ ]" . "󰝦") prettify-symbols-alist) ; todo
;;   (push '("[/]" . "󱎖") prettify-symbols-alist) ; doing
;;   (push '("[-]" . "󰜺") prettify-symbols-alist) ; cancelled
;;   (push '("[X]" . "") prettify-symbols-alist) ; done
;;   (push '("[>]" . "") prettify-symbols-alist) ; email
;;   (push '("[!]" . "") prettify-symbols-alist) ; important
;;   (push '("[?]" . "") prettify-symbols-alist) ; question
;;   (push '("[a]" . "") prettify-symbols-alist) ; answer
;;   (push '("[b]" . "") prettify-symbols-alist) ; bookmark
;;   (push '("[d]" . "") prettify-symbols-alist) ; calendar
;;   (push '("[e]" . "") prettify-symbols-alist) ; example
;;   (push '("[l]" . "") prettify-symbols-alist) ; location
;;   (push '("[q]" . "󰉾") prettify-symbols-alist) ; quote
;;   (push '("[w]" . "") prettify-symbols-alist) ; waiting
;;   (prettify-symbols-mode))
;; (add-hook 'org-mode-hook #'prettify-org-checkboxes)

;; (defface org-checkbox-done-text
;;   '((t (:foreground "#71696A" :strike-through t)))
;;   "Face for the text part of a checked org-mode checkbox.")

;; (font-lock-add-keywords
;;  'org-mode
;;  `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
;;     1 'org-checkbox-done-text prepend))
;;  'append)

;; TODO The following text can be used and modified to search for whatever
;; pretty bullets you want within vertico/consult/embark.
;; -\ \[[^X\s]\]

;; NOTE Uncomment this if you disable org-superstar-remove-leading-stars and
;; enable org-hide-leading-stars. It works in either situation, but I figured
;; it was best to comment it out to reduce the overhead. This is not necessary
;; for org-modern.

;; (setq-hook! 'org-mode-hook hl-line-range-function #'my/hl-line-skip-org-hide-stars)
;; (defun my/hl-line-skip-org-hide-stars ()
;;   "Don't apply the `hl-line' overlay to org heading stars. Note
;; that leading stars are still visible via the cursor."
;;   (if (and hl-line-mode
;;            ;; (eq major-mode 'org-mode)
;;            (not (bound-and-true-p org-superstar-remove-leading-stars))
;;            org-hide-leading-stars
;;            (org-at-heading-p))
;;       (cons (+ (line-beginning-position) (1+ (org-current-level)))
;;             (line-beginning-position 2))
;;     (cons (line-beginning-position)
;;           (line-beginning-position 2))))

;; NOTE Three configurations for hiding leading stars on org headings:
;;
;; 1. Enable `org-superstar-remove-leading-stars' to hide leading stars and
;; hide the "indentation" from those characters
;;
;; 2. Disable `org-superstar-remove-leading-stars' and enable
;; `org-hide-leading-stars' to apply the `org-hide' face to leading stars,
;; which might require customization depending on your theme. The intention is
;; for this face's foreground to match the background. Note that stars will be
;; visible when `hl-line-mode' is enabled or the cursor is on a star.
;;
;; 3. Disable `org-superstar-remove-leading-stars' and `org-hide-leading-stars'
;; and set `org-superstar-leading-bullet' to `?\s' to preserve all indentation
;; but still hide leading stars. If `org-indent-mode' is enabled, you also
;; need to disable `org-indent-mode-turns-on-hiding-stars' to disable
;; `org-hide-leading-stars'.
;;
;; NOTE See `org-superstar-restart' for enabling changes made on the fly

;; (use-package! org-superstar ; "prettier" bullets
;;   :hook (org-mode . org-superstar-mode)
;;   :config
;;   ;; Make leading stars truly invisible, by rendering them as spaces!
;;   (setq org-superstar-leading-bullet ?\s
;;         org-superstar-leading-fallback ?\s
;;         org-superstar-remove-leading-stars nil
;;         org-superstar-headline-bullets-list '(?\s ?\s ?\s ?\s)
;;         org-superstar-special-todo-items t
;;         org-superstar-todo-bullet-alist
;;         '(("TODO" . ?\s)
;;           ("DONE" . ?☑))))


;; (use-package! org-fancy-priorities ; priority icons
;;   :hook (org-mode . org-fancy-priorities-mode)
;;   :hook (org-agenda-mode . org-fancy-priorities-mode)
;;   :config (setq org-fancy-priorities-list '("⚑" "⬆" "■")))

;; A list of regular expressions matching paths to exclude from org-roam. Any
;; files or directories to exclude must be relative to `org-roam-directory',
;; per the definition of `org-roam-file-p'; this isn't documented, and
;; org-roam's default value for this is wrong: the absolute path to
;; `org-attach-id-dir'.
(setq org-roam-file-exclude-regexp nil)

;; A predicate function that determines valid nodes for org-roam
(setq org-roam-db-node-include-function #'jkroes/org-roam-include-nodes)

(defun jkroes/org-roam-include-nodes ()
  ;; Exclude attachment headings unless there is another reason to keep them,
  ;; such as a ROAM_REFS property
  (not (and (member org-attach-auto-tag (org-get-tags))
            (not (org-entry-get nil "roam_refs" nil)))))

(advice-add #'org-roam-tag-add :override #'jkroes/org-roam-tag-add)

(defun jkroes/org-roam-tag-add (tags)
  (interactive
   (list (let ((crm-separator "[ 	]*:[ 	]*"))
           (completing-read-multiple "Tag: " (org-roam-tag-completions)))))
  (let ((node (org-roam-node-at-point 'assert)))
    (save-excursion
      (goto-char (org-roam-node-point node))
      (if (= (org-outline-level) 0)
          (let ((current-tags (split-string (or (cadr (assoc "FILETAGS"
                                                             (org-collect-keywords '("filetags"))))
                                                "")
                                            ":" 'omit-nulls)))
            (org-roam-set-keyword "filetags" (org-make-tag-string (seq-uniq (append tags current-tags)))))
        (org-set-tags (seq-uniq (append tags (org-get-tags nil t)))))
      tags)))

(use-package! consult-org-roam
  :after org-roam
  :init
  ;; BUG https://github.com/jgru/consult-org-roam/issues/32. Vertico sorting is
  ;; only active if consult-org-roam-mode is disabled or enabled in tandem with
  ;; this advice.
  (advice-add #'consult-org-roam-node-read
              :override #'jkroes/consult-org-roam-node-read)
  :config
  (setq consult-org-roam-grep-func #'consult-ripgrep)
  (consult-org-roam-mode))

(defun jkroes/consult-org-roam-node-read (&optional initial-input filter-fn sort-fn
                                     require-match prompt)
  (let* ((nodes (org-roam-node-read--completions filter-fn sort-fn)) ;;
         (prompt (or prompt "Node: "))
         ;; Sets state-func only when there are nodes to avoid errors
         ;; with empty roam-dirs
         (state-func (when nodes
                       (consult-org-roam--node-preview)))
         (node
          (consult--read
           nodes
           :prompt prompt
           :initial initial-input
           ;; HACK Enabling sorting here allows the user to override sorting
           ;; via vertico-multiform when consult-org-roam-mode is enabled
           :sort t
           :require-match require-match
           :category 'org-roam-node
           ;;:history 'org-roam-node-history
           :state state-func
           :annotate (lambda (title)
                       (funcall org-roam-node-annotation-function
                                (get-text-property 0 'node title)))
           ;; Uses the DEFAULT argument of alist-get to return input in case the input is not found as key.
           :lookup (lambda (selected candidates input narrow) (alist-get selected candidates input nil #'equal)))))
    (if (org-roam-node-p node) (progn node)
      (progn (org-roam-node-create :title node)))))

(add-to-list 'load-path (expand-file-name "libraries" doom-private-dir))
(autoload #'dendroam-find "dendroam")
(autoload #'dendroam-find-master-scratch "dendroam")
(autoload #'dendroam-find-scratch "dendroam")
(autoload #'dendroam-find-meeting "dendroam")
(autoload #'dendroam-find-children "dendroam")
(autoload #'dendroam-find-siblings "dendroam")
(autoload #'dendroam-find-parent "dendroam")
(autoload #'dendroam-open-note "dendroam")

;; See the definition of `org-roam-node-dendroam-tags'
(setq dendroam-hidden-tags nil)

;; Sort nodes alphabetically
(after! vertico-multiform
  (add-to-list 'vertico-multiform-commands
               '(dendroam-find (vertico-sort-function . vertico-sort-alpha))))

(after! org-roam
  (map! :map org-mode-map
        :localleader
        :prefix ("m" . "org-roam")
        "f" #'dendroam-find))

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

(after! org
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"     ; A task that is ready to start
           "NOW(n!)"     ; An active task
           "CHOOSE(c)"
           "WAIT(w@/!)"  ; A suspended task
           "|"
           "CHOSEN"
           "DONE(d!/@)"    ; Task successfully completed
           "KILL(k@/@)")))) ; Task was cancelled, aborted, or is no longer applicable

(setq org-enforce-todo-dependencies t
      org-enforce-todo-checkbox-dependencies t)

(setq org-hierarchical-todo-statistics t
      org-checkbox-hierarchical-statistics nil)

(add-hook 'org-after-todo-statistics-hook
          #'jkroes/org-toggle-todo)

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

;; BUG This inserts the radio keyword above the current heading if there is not
;; a blank line after the heading
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
(after! org-list
  (setcdr (assoc 'checkbox org-list-automatic-rules) nil))

(add-hook 'org-checkbox-statistics-hook
          #'org-toggle-todo-checkboxes)

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
  (let ((at-checkbox? (org-at-item-checkbox-p)))
    (save-excursion
      (org-back-to-heading t)
      (let* ((cookie-re "\\[\\([0-9]*\\)/\\([0-9]*\\)\\]")
             (cookie-end (re-search-forward cookie-re (line-end-position) t))
             (cookie-beginning (when cookie-end (match-beginning 0)))
             (numerator (when cookie-end (string-to-number (match-string 1))))
             (denominator (when cookie-end (string-to-number (match-string 2))))
             (state (org-get-todo-state)))
        (cond ((and (not cookie-end) at-checkbox?)
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
               (org-todo "TODO")))))))

(advice-add #'org-toggle-radio-button
            :around (lambda (orig-fun &rest args)
                      (cl-letf (((symbol-function 'org-update-checkbox-count-maybe)
                                 #'ignore))
                        (apply orig-fun args))))

;; Attachment directory for my work computer.
(when IS-WSL
  (setq org-attach-id-dir
        "/mnt/c/Users/jkroes/OneDrive - California Department of Pesticide Regulation (1)/org-attach"))

(advice-add #'org-attach-expand :override #'jkroes/org-attach-expand-a)

;; Stack trace when following attachment links:
;; org-open-at-point
;; org-link-open
;; org-attach-follow
;; org-link-open-as-file(org-attach-expand)
;; org-open-file
;; (user-error "No such file: %s" file))

;; Use this to enable attachment links below subheadings
(defun jkroes/org-attach-expand-a (file)
  "HACK A version of org-attach-expand that actually will look
 through all parent headings until it finds the linked attachment,
 to quote the docs for `org-attach-use-inheritance'. Normally the
 search stops at the first heading for which there is an
 attachment directory."
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

(advice-add #'org-attach-tag :override #'jkroes/org-attach-tag)

(defun jkroes/org-attach-tag (&optional off)
  "Turn the autotag on or (if OFF is set) off."
  (when org-attach-auto-tag
    ;; FIXME: There is currently no way to set #+FILETAGS
    ;; programmatically.  Do nothing when before first heading
    ;; (attaching to file) to avoid blocking error.
    (unless (org-before-first-heading-p)
      (save-excursion
        (org-back-to-heading t)
        (when (org-entry-get nil "ID")
                (org-toggle-tag org-attach-auto-tag (if off 'off 'on)))))))

(after! marginalia
  (add-to-list 'marginalia-command-categories
               '(org-attach-open . attach))
  (add-to-list 'marginalia-annotator-registry
               '(attach marginalia-annotate-attachment builtin none)))

(defun marginalia-annotate-attachment (cand)
  (marginalia-annotate-file (cdr (embark--expand-attachment nil cand))))

(after! embark
  (add-to-list 'embark-transformer-alist '(attach . embark--expand-attachment))
  (add-to-list 'embark-transformer-alist '(org-roam-node . embark--org-roam-node-file)))

(defun embark--expand-attachment (_ target)
  "Transform marginalia category from `attach' to `file' and
 convert target to filepath. `org-attach-open' does not use the
 path returned by `org-attach-dir' as minibuffer input.
 `embark--vertico-selected' constructs embark targets from the
 candidate and the minibuffer input, so the target is not the
 full path."
  (with-current-buffer (window-buffer (minibuffer-selected-window))
    (cons 'file (expand-file-name target (org-attach-dir)))))

(defun embark--org-roam-node-file (x target)
  "Transform marginalia category from `org-roam-node' to `file' and
 convert target to filepath."
  (cons 'file (org-roam-node-file (get-text-property 0 'node target))))

;; BUG Large code blocks can slow down `org-cycle-global' noticeably when
;; code block native fontification is enabled. Disable this if you notice an
;; issue.
(setq org-src-fontify-natively t)

;; Hide org src block highlighting when headings are folded
(setq org-fontify-whole-block-delimiter-line nil)

(setq org-src-ask-before-returning-to-edit-buffer nil)
(after! org (setq org-src-window-setup 'current-window))

;; https://scripter.co/splitting-an-org-block-into-two/
(defun modi/org-split-block ()
  "Sensibly split the current Org block at point."
  (interactive)
  (if (modi/org-in-any-block-p)
      (save-match-data
        (save-restriction
          (widen)
          (let ((case-fold-search t)
                (at-bol (bolp))
                block-start
                block-end)
            (save-excursion
              (re-search-backward "^\\(?1:[[:blank:]]*#\\+begin_.+?\\)\\(?: .*\\)*$" nil nil 1)
              (setq block-start (match-string-no-properties 0))
              (setq block-end (replace-regexp-in-string
                               "begin_" "end_" ;Replaces "begin_" with "end_", "BEGIN_" with "END_"
                               (match-string-no-properties 1))))
            ;; Go to the end of current line, if not at the BOL
            (unless at-bol
              (end-of-line 1))
            (insert (concat (if at-bol "" "\n")
                            block-end
                            "\n\n"
                            block-start
                            (if at-bol "\n" "")))
            ;; Go to the line before the inserted "#+begin_ .." line
            (beginning-of-line (if at-bol -1 0)))))
    (message "Point is not in an Org block")))

(defun modi/org-in-any-block-p ()
  "Return non-nil if the point is in any Org block.

The Org block can be *any*: src, example, verse, etc., even any
Org Special block.

This function is heavily adapted from `org-between-regexps-p'."
  (save-match-data
    (let ((pos (point))
          (case-fold-search t)
          (block-begin-re "^[[:blank:]]*#\\+begin_\\(?1:.+?\\)\\(?: .*\\)*$")
          (limit-up (save-excursion (outline-previous-heading)))
          (limit-down (save-excursion (outline-next-heading)))
          beg end)
      (save-excursion
        ;; Point is on a block when on BLOCK-BEGIN-RE or if
        ;; BLOCK-BEGIN-RE can be found before it...
        (and (or (org-in-regexp block-begin-re)
                 (re-search-backward block-begin-re limit-up :noerror))
             (setq beg (match-beginning 0))
             ;; ... and BLOCK-END-RE after it...
             (let ((block-end-re (concat "^[[:blank:]]*#\\+end_"
                                         (match-string-no-properties 1)
                                         "\\( .*\\)*$")))
               (goto-char (match-end 0))
               (re-search-forward block-end-re limit-down :noerror))
             (> (setq end (match-end 0)) pos)
             ;; ... without another BLOCK-BEGIN-RE in-between.
             (goto-char (match-beginning 0))
             (not (re-search-backward block-begin-re (1+ beg) :noerror))
             ;; Return value.
             (cons beg end))))))

;; Directory file links launch `find-file' with the directory as initial
;; input, rather than launching dired.
(add-to-list 'find-directory-functions #'jkroes/not-dired)

(setq org-file-apps
      '(("\\.pptx?\\'" . system)
        ("\\.pdf?\\'" . system)
        ("\\.docx?\\'" . system)
        ("\\.txt?\\'" . system)
        ("\\.xlsx?\\'" . system)
        ("\\.csv?\\'" . system)
        ("\\.png?\\'" . system)
        ("\\.html?\\'" . system)
        (remote . emacs)
        (auto-mode . emacs)
        (directory . emacs)))

;; Open files in emacs even if they aren't part of auto-mode-alist
(after! org
  (setq org-file-apps-macos
        '((system . "open %s")
          (t . emacs)))

  ;; TODO Per org-file-apps docstring, we can replace open-in-windows with a
  ;; string "wslview %s" if this has issues
  (setq org-file-apps-gnu
        `(,(cons 'system (if IS-WSL #'open-in-windows 'mailcap))
          (t . emacs))))

;; Insert pairs of tildes in org-mode
(after! smartparens (sp-local-pair 'org-mode "~" "~" ))

(after! org (setq org-insert-heading-respect-content nil))

;; Make the backend for org's native various heading insertion commands enter
;; insert state after insertion
(defadvice! jkroes/org-insert-heading-insert-state-a (&rest _)
  :after (list #'org-insert-heading)
  (when (and (bound-and-true-p evil-local-mode)
             (not (evil-emacs-state-p)))
    (evil-insert 1)))

(advice-add #'+org--insert-item
            :override #'jkroes/org--insert-item)

(defun jkroes/org--insert-item (direction)
  (let ((context (org-element-lineage
                  (org-element-context)
                  '(table table-row headline inlinetask item plain-list)
                  t)))
    (pcase (org-element-type context)
      ;; Add a new list item (carrying over checkboxes if necessary)
      ((or `item `plain-list)
       (let ((orig-point (point)))
         ;; Position determines where org-insert-todo-heading and `org-insert-item'
         ;; insert the new list item.
         (if (eq direction 'above)
             (org-beginning-of-item)
           (end-of-line))
         (let* ((ctx-item? (eq 'item (org-element-type context)))
                (ctx-cb (org-element-property :contents-begin context))
                ;; Hack to handle edge case where the point is at the
                ;; beginning of the first item
                (beginning-of-list? (and (not ctx-item?)
                                         (= ctx-cb orig-point)))
                (item-context (if beginning-of-list?
                                  (org-element-context)
                                context))
                ;; Horrible hack to handle edge case where the
                ;; line of the bullet is empty
                (ictx-cb (org-element-property :contents-begin item-context))
                (empty? (and (eq direction 'below)
                             ;; in case contents-begin is nil, or contents-begin
                             ;; equals the position end of the line, the item is
                             ;; empty
                             (or (not ictx-cb)
                                 (= ictx-cb
                                    (1+ (point))))))
                (pre-insert-point (point)))
           ;; Insert dummy content, so that `org-insert-item'
           ;; inserts content below this item
           (when empty?
             (insert " "))
           (org-insert-item (org-element-property :checkbox context))
           ;; Remove dummy content
           (when empty?
             (delete-region pre-insert-point (1+ pre-insert-point))))))
      ;; Add a new table row
      ((or `table `table-row)
       (pcase direction
         ('below (save-excursion (org-table-insert-row t))
                 (org-table-next-row))
         ('above (save-excursion (org-shiftmetadown))
                 (+org/table-previous-row))))

      ;; Otherwise, add a new heading, carrying over any todo state, if
      ;; necessary.
      (_
       (let ((level (or (org-current-level) 1)))
         ;; I intentionally avoid `org-insert-heading' and the like because they
         ;; impose unpredictable whitespace rules depending on the cursor
         ;; position. It's simpler to express this command's responsibility at a
         ;; lower level than work around all the quirks in org's API.
         (pcase direction
           ;; HACK Removed call to org-end-of-subtree to do simple insertion
           ;; below the current heading
           (`below
            (let (org-insert-heading-respect-content)
              (goto-char (line-end-position))
              (insert "\n" (make-string level ?*) " ")))
           (`above
            (org-back-to-heading)
            (insert (make-string level ?*) " ")
            (save-excursion (insert "\n"))))
         (run-hooks 'org-insert-heading-hook)
         (when-let* ((todo-keyword (org-element-property :todo-keyword context))
                     (todo-type    (org-element-property :todo-type context)))
           (org-todo
            (cond ((eq todo-type 'done)
                   ;; Doesn't make sense to create more "DONE" headings
                   (car (+org-get-todo-keywords-for todo-keyword)))
                  (todo-keyword)
                  ('todo)))))))

    (when (org-invisible-p)
      (org-show-hidden-entry))
    (when (and (bound-and-true-p evil-local-mode)
               (not (evil-emacs-state-p)))
      (evil-insert 1))))

;; Don't insert blank lines when creating a heading
(setq org-blank-before-new-entry '((heading) (plain-list-item))
      ;; Show all empty lines when headings are folded
      org-cycle-separator-lines -1)

(defvar org-global-cyle-shows-blocks nil
  "Whether org-cycle-global unfolds blocks.")

;; Don't hide blocks unless org-global-cyle-shows-blocks
(advice-add #'org-cycle-internal-global
            :override #'jkroes/org-cycle-internal-global)


(defun jkroes/org-cycle-internal-global ()
  "Do the global cycling action."
  ;; Hack to avoid display of messages for .org  attachments in Gnus
  (let ((ga (string-match-p "\\*fontification" (buffer-name))))
    (cond
     ((and (eq last-command this-command)
	   (eq org-cycle-global-status 'overview))
      ;; We just created the overview - now do table of contents
      ;; This can be slow in very large buffers, so indicate action
      (run-hook-with-args 'org-cycle-pre-hook 'contents)
      (unless ga (org-unlogged-message "CONTENTS..."))
      (org-cycle-content)
      (unless ga (org-unlogged-message "CONTENTS...done"))
      (setq org-cycle-global-status 'contents)
      (run-hook-with-args 'org-cycle-hook 'contents))

     ((and (eq last-command this-command)
	   (eq org-cycle-global-status 'contents))
      ;; We just showed the table of contents - now show everything
      (run-hook-with-args 'org-cycle-pre-hook 'all)
      (org-fold-show-all
       (append (list 'headings)
               (when org-global-cyle-shows-blocks (list 'blocks))))
      (unless ga (org-unlogged-message "SHOW ALL"))
      (setq org-cycle-global-status 'all)
      (run-hook-with-args 'org-cycle-hook 'all))

     (t
      ;; Default action: go to overview
      (run-hook-with-args 'org-cycle-pre-hook 'overview)
      (org-cycle-overview)
      (unless ga (org-unlogged-message "OVERVIEW"))
      (setq org-cycle-global-status 'overview)
      (run-hook-with-args 'org-cycle-hook 'overview)))))

;; See org-use-sub-superscripts
(setq org-export-with-sub-superscripts '{})

;; TODO Run this if you need to generate a Word style template file:
;;
;; pandoc --print-default-data-file=reference.docx > ~/org/custom-reference.docx
;;
;; See org-pandoc-valid-options for available pandoc CLI flags

;; (add-to-list (cons 'reference-doc "~/org/custom-reference.docx")
;;              org-pandoc-options)

(defun my/org-cycle ()
  "Adapt org-cycle to fold the current code block if point is within
one. Useful for finding one's place within a large code block
without folding any headings."
  (interactive)
  ;; Move to the start of the block so that org-cycle will call
  ;; org-fold-hide-block-toggle
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

(defun my/org-edit-src-save-and-exit ()
  (interactive)
  (org-edit-src-save)
  (org-edit-src-exit)
  ;; Prevents accidental text insertion
  (evil-normal-state))

(autoload 'ffap-string-at-point "ffap")
(defun jkroes/org-dwim-at-point (&optional arg)
  "Do-what-I-mean at point.

If on a:
- checkbox list item or todo heading: toggle it.
- citation: follow it
- headline: cycle ARCHIVE subtrees, toggle latex fragments and inline images in
  subtree; update statistics cookies/checkboxes and ToCs.
- clock: update its time.
- footnote reference: jump to the footnote's definition
- footnote definition: jump to the first reference of this footnote
- timestamp: open an agenda view for the time-stamp date/range at point.
- table-row or a TBLFM: recalculate the table's formulas
- table-cell: clear it and go into insert mode. If this is a formula cell,
  recaluclate it instead.
- babel-call: execute the source block
- statistics-cookie: update it.
- src block: execute it
- latex fragment: toggle it.
- link: follow it
- otherwise, refresh all inline images in current tree."
  (interactive "P")
  (if (button-at (point))
      (call-interactively #'push-button)
    (let* ((context (org-element-context))
           (type (org-element-type context)))
      ;; skip over unimportant contexts
      (while (and context (memq type '(verbatim code bold italic underline strike-through subscript superscript)))
        (setq context (org-element-property :parent context)
              type (org-element-type context)))
      (pcase type
        ((or `citation `citation-reference)
         (org-cite-follow context arg))
        ;; HACK To enable marginalia annotations (and embark-act, which
        ;; relies on the metadata marginalia sets), we either need to
        ;; bind this-command to org-attach-open or call it with
        ;; execute-extended-command
        (`property-drawer
           (cond ((let (org-attach-use-inheritance) (org-attach-dir))
                  (let ((this-command #'org-attach-open)) (org-attach-open)))))
        (`headline
         ;; HACK Avoid errors generated by calling org-update-checkbox-count
         ;; or other functions meant to run in an org buffer after switching to
         ;; an attached file
         (catch 'attach
           (cond ((let (org-attach-use-inheritance) (org-attach-dir))
                  (let ((this-command #'org-attach-open)) (org-attach-open))
                  (throw 'attach nil))
                 ((memq (bound-and-true-p org-goto-map)
                        (current-active-maps))
                  (org-goto-ret))
                 ((and (fboundp 'toc-org-insert-toc)
                       (member "TOC" (org-get-tags)))
                  (toc-org-insert-toc)
                  (message "Updating table of contents"))
                 ((string= "ARCHIVE" (car-safe (org-get-tags)))
                  (org-force-cycle-archived))
                 ((or (org-element-property :todo-type context)
                      (org-element-property :scheduled context))
                  (org-todo
                   (if (eq (org-element-property :todo-type context) 'done)
                       (or (car (+org-get-todo-keywords-for (org-element-property :todo-keyword context)))
                           'todo)
                     'done))))
           ;; Update any metadata or inline previews in this subtree
           ;; (org-update-checkbox-count)
           ;; (org-update-parent-todo-statistics)
           (when (and (fboundp 'toc-org-insert-toc)
                      (member "TOC" (org-get-tags)))
             (toc-org-insert-toc)
             (message "Updating table of contents"))
         (let* ((beg (if (org-before-first-heading-p)
                         (line-beginning-position)
                       (save-excursion (org-back-to-heading) (point))))
                (end (if (org-before-first-heading-p)
                         (line-end-position)
                       (save-excursion (org-end-of-subtree) (point))))
                (overlays (ignore-errors (overlays-in beg end)))
                (latex-overlays
                 (cl-find-if (lambda (o) (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay))
                             overlays))
                (image-overlays
                 (cl-find-if (lambda (o) (overlay-get o 'org-image-overlay))
                             overlays)))
           (+org--toggle-inline-images-in-subtree beg end)
           (if (or image-overlays latex-overlays)
               (org-clear-latex-preview beg end)
             (org--latex-preview-region beg end)))))

        (`clock (org-clock-update-time-maybe))

        (`footnote-reference
         (org-footnote-goto-definition (org-element-property :label context)))

        (`footnote-definition
         (org-footnote-goto-previous-reference (org-element-property :label context)))

        ((or `planning `timestamp)
         (org-follow-timestamp-link))

        ((or `table `table-row)
         (if (org-at-TBLFM-p)
             (org-table-calc-current-TBLFM)
           (ignore-errors
             (save-excursion
               (goto-char (org-element-property :contents-begin context))
               (org-call-with-arg 'org-table-recalculate (or arg t))))))

        (`table-cell
         (org-table-blank-field)
         (org-table-recalculate arg)
         (when (and (string-empty-p (string-trim (org-table-get-field)))
                    (bound-and-true-p evil-local-mode))
           (evil-change-state 'insert)))

        (`babel-call
         (org-babel-lob-execute-maybe))

        (`statistics-cookie
         (save-excursion (org-update-statistics-cookies arg)))

        ((or `src-block `inline-src-block)
         (org-edit-src-code))

        ((or `latex-fragment `latex-environment)
         (org-latex-preview arg))

        (`link
         (let* ((lineage (org-element-lineage context '(link) t))
                (path (org-element-property :path lineage)))
           (if (or (equal (org-element-property :type lineage) "img")
                   (and path (image-type-from-file-name path)))
               (+org--toggle-inline-images-in-subtree
                (org-element-property :begin lineage)
                (org-element-property :end lineage))
             (org-open-at-point arg))))

        ((guard (org-element-property :checkbox (org-element-lineage context '(item) t)))
         (org-toggle-checkbox))

        (`paragraph
         (+org--toggle-inline-images-in-subtree))

        ;; HACK Jump to INCLUDE files
        (`keyword
         (when (string= "INCLUDE" (org-element-property :key context))
           (let (string-at-point)
             (save-excursion
               (beginning-of-line)
               (search-forward "#+INCLUDE: \"")
               (setq string-at-point (ffap-string-at-point)))
             (if (file-exists-p string-at-point)
                 (find-file string-at-point)
               (message "Non-existent file argument in INCLUDE keyword")))))
        (_
         (if (or (org-in-regexp org-ts-regexp-both nil t)
                 (org-in-regexp org-tsr-regexp-both nil  t)
                 (org-in-regexp org-link-any-re nil t))
             (call-interactively #'org-open-at-point)
           (+org--toggle-inline-images-in-subtree
            (org-element-property :begin context)
            (org-element-property :end context))))))))

(advice-add #'+org/dwim-at-point
            :override #'jkroes/org-dwim-at-point)

(map! :map org-src-mode-map
      :n "q" #'my/org-edit-src-save-and-exit)

(add-hook! 'org-src-mode-hook #'evil-normalize-keymaps)

;; List of commands:
;; citar-open
;; org-cite-insert / citar-insert-citation
;; citar-insert-reference (for a references section; unused currently)
;; citar-org-roam-ref-add
;; org-roam-ref-find (for finding a node by URL)
;; citar-org-roam-ref-add: Add an additional citation key to roam refs of the
;;   node at point.
;; citar-org-roam-cited
;; - TODO This throws an error "Wrong type argument: org-roam-node, nil" if
;; there are no citations. The function is fine, but it needs to be rewritten
;; to return nil rather than attempting to visit a nonexistent node.
;; TODO +org/dwim erases citations when this is the default action (on WSL,
;; untested on MacOS):
;;   (setq citar-default-action #'citar-insert-edit)

(setq citar-bibliography
      (list (expand-file-name
             "org-cite.bib"
             (cond (IS-WSL "/mnt/d")
                   (t org-directory))))
      org-cite-global-bibliography citar-bibliography)

;; TODO suffix should be converted to an annotation if possible, similar to dendroam
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

(defun open-in-zotero (file)
  "Open file resources in Zotero PDF viewer."
  (string-match ".*/storage/\\(.*\\)/.*\\.pdf" file)
  (browse-url
   ;; NOTE You can also use select instead of open-pdf to see the
   ;; attachment item in the item pane
   (replace-match "zotero://open-pdf/library/items/\\1" nil nil file)))

;; Convert Windows to WSL paths when opening PDF files
(after! citar-file
  (add-to-list 'citar-file-parser-functions 'citar-file--parser-default-wsl))

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

;; Whether to use multiple selection
(setq citar-select-multiple nil)

;; TODO Get this working like in the image in the README. May clash with roam
;; completion.
;;
;; Complete citations keys by typing [@] and the number of letters indicated by
;; corfu-auto-prefix after the @
(add-hook 'org-mode-hook #'citar-capf-setup)

(defvar citar-indicator-links-icons
   (citar-indicator-create
    :symbol (nerd-icons-octicon "nf-oct-link")
    :function #'citar-has-links
    :tag "has:links"))

(defvar citar-indicator-files-icons
  (citar-indicator-create
   :symbol (nerd-icons-sucicon "nf-seti-pdf")
   :function #'citar-has-files
   :padding "  "
   :tag "has:files"))

(defvar citar-indicator-notes-icons
  (citar-indicator-create
   ;; :symbol (nerd-icons-faicon "nf-fa-sticky_note_o")
   :symbol (nerd-icons-mdicon "nf-md-file_document_outline")
   :function #'citar-has-notes
   :tag "has:notes"))

(defvar citar-indicator-cited
  (citar-indicator-create
   :symbol (nerd-icons-octicon "nf-oct-cross_reference")
   :function #'citar-is-cited
   :tag "is:cited"))

(defvar citar-indicators
  (list citar-indicator-links-icons
        citar-indicator-files-icons
        citar-indicator-notes-icons
        citar-indicator-cited))

;; Subdirectory of `org-roam-directory' for citar notes
(setq citar-org-roam-subdir "references")

;; citar note filetitle, which is the title field from the bibliography
(setq citar-org-roam-note-title-template "${title}")

(defadvice! citar-org-roam--create-capture-note-a (citekey entry)
  "Slightly modified function for citar-reference-note creation"
  :after #'citar-org-roam--create-capture-note
  (when (fboundp 'evil-insert)
    (evil-append-line 1)
    (insert "\n")))

;; Disable popup management of org-src buffer windows
(after! org
  (advice-remove #'org-edit-src-exit #'+popup--org-edit-src-exit-a)
  (assoc-delete-all "^\\*Org Src" +popup--display-buffer-alist)
  (assoc-delete-all "^\\*Org Src" display-buffer-alist))

;; TODO The first info buffer shows the modeline, but successive buffers do not.
;; Investigate the modeline rules for popups. In the meantime, disable modeline
;; hiding for popups.
(remove-hook '+popup-buffer-mode-hook #'+popup-set-modeline-on-enable-h)

;; BUG When the top line of a window's buffer is blank, the background extends
;; to the entire line, or else the letter is invisible.
;; https://emacs.stackexchange.com/questions/45895/changing-faces-one-at-a-time-outside-customize
(after! ace-window
  (custom-set-faces!
    '(aw-leading-char-face
      :foreground "white" :background "red" :height 500)))

;; If we bind `other-window' directly, it will remap to `ace-window' when
;; the window-select module is active. If we want to circumvent remapping, wrap
;; the remapped command in a function call.
(map! "M-o" (cmd! (call-interactively #'other-window)))
