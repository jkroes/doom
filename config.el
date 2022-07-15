;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!
(setq doom-font (font-spec :family "Hack" :size (cond (IS-MAC 14) (IS-LINUX 12))))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;;; My notes

;;;; emacs-mac server issues
;; https://gist.github.com/railwaycat/4043945

;;;; Troubleshooting straight
;; https://github.com/radian-software/straight.el/issues/292
;; https://github.com/radian-software/straight.el/issues/540

;;;; Working with Doom
;; After modifying modules or init.el, run doom sync
;; After modifying packages.el--speifically recipes for existing packages--run
;; doom sync -u. You need to run this the first time Doom is installed
;; To upgrade Doom itself, run doom upgrade
;; After an upgrade, you will have to run pdf-tools-install
;; Messing with modules may break lazy loading and lead to unexpected results when
;; trying to defer a package. To see what loads and when, enable force-load-messages
;; in early-init.el
;; Install/sync/upgrade fail on work computers, it might be because recipes
;; clone from gitlab, and gitlab is blocked. Clone the package manually (from
;; github potentially) while inside ~/.emacs.d/.local/straight/repos, then try
;; again. Try also disconnecting from the VPN. Alternatively, a package may have
;; renamed its branch from master to main. Delete the repo at issue and try again.
;; Before and after install/sync/upgrade, run doom sync

;;; My code

;; When Emacs freezes and won't respond to C-g, open a terminal and run ~pkill
;; -SIGTERM Emacs~ as many times as needed to kill Emacs. Then check
;; ~doom-cache-dir~ for a file named backtrace. Ignore the lines from the top to
;; the anonymous function defined below.
(defun concat-path (&rest parts)
  "Concatenate unlimited path components"
  (cl-reduce (lambda (a b) (expand-file-name b a)) parts))

(defun save-backtrace ()
  (require 'backtrace)
  (with-temp-file (concat-path doom-cache-dir "backtrace")
    ;; Pulled from backtrace function
    (insert (backtrace-to-string
             (backtrace-get-frames 'backtrace)))))
(add-hook 'kill-emacs-hook 'save-backtrace)

;; If you are getting prompts about bytes that don't work with the coding system
;; and to save as a new encoding, use the following to find the bad characters.
(defun find-next-unsafe-char (&optional coding-system)
  "Find the next character in the buffer that cannot be encoded by
coding-system. If coding-system is unspecified, default to the coding
system that would be used to save this buffer. With prefix argument,
prompt the user for a coding system."
  (interactive "Zcoding-system: ")
  (if (stringp coding-system) (setq coding-system (intern coding-system)))
  (if coding-system nil
    (setq coding-system
          (or save-buffer-coding-system buffer-file-coding-system)))
  (let ((found nil) (char nil) (csets nil) (safe nil))
    (setq safe (coding-system-get coding-system 'safe-chars))
    ;; some systems merely specify the charsets as ones they can encode:
    (setq csets (coding-system-get coding-system 'safe-charsets))
    (save-excursion
      ;;(message "zoom to <")
      (let ((end  (point-max))
            (here (point    ))
            (char  nil))
        (while (and (< here end) (not found))
          (setq char (char-after here))
          (if (or (eq safe t)
                  (< char ?\177)
                  (and safe  (aref safe char))
                  (and csets (memq (char-charset char) csets)))
              nil ;; safe char, noop
            (setq found (cons here char)))
          (setq here (1+ here))) ))
    (and found (goto-char (1+ (car found))))
    found))

(autoload 'doom--help-modules-list (concat-path doom-emacs-dir "core/autoload/help.el"))
(defun doom/copy-module-to-private (category module)
  "Copy the module corresponding to the strings category and module as a private
module."
  (interactive
   (nconc
    (mapcar #'intern
            (split-string
             (completing-read "Copy module: "
                              (doom--help-modules-list)
                              nil t nil nil
                              (doom--help-current-module-str))
             " " t))))
  (let* ((path (doom-module-locate-path category module))
         (newpath (replace-regexp-in-string doom-modules-dir (car doom-modules-dirs) path)))
    (copy-directory path newpath nil t t)))

;; Compare private and non-private module file to see your modifications and
;; any changes Doom has made if you've upgraded it recently
(autoload #'ediff-read-file-name "ediff")
(defun doom/ediff ()
  "Run Ediff on a private Doom module file and its non-private counterpart"
  (interactive)
  (let* ((file-A
          (ediff-read-file-name
	   "Private module file"
           (if (string-prefix-p (car doom-modules-dirs) default-directory)
               default-directory
             (car doom-modules-dirs))
           (ediff-get-default-file-name)
           'no-dirs))
         (file-B (replace-regexp-in-string (car doom-modules-dirs) doom-modules-dir file-A)))
    (ediff-files-internal file-A file-B nil nil 'ediff-files)))
;; TODO Delete this after migrating .doom.d.bck modules
(defun doom/ediff2 ()
  (interactive)
  (let* ((file-A
          (ediff-read-file-name
	   "Private module file"
           (if (string-prefix-p (car doom-modules-dirs) default-directory)
               default-directory
             (car doom-modules-dirs))
           (ediff-get-default-file-name)
           'no-dirs))
         (file-B (replace-regexp-in-string (car doom-modules-dirs) "/Users/jkroes/.doom.d.bck/modules/" file-A)))
    (ediff-files-internal file-A file-B nil nil 'ediff-files)))

(setq mac-pass-command-to-system nil ; So C-h doesn't hide Emacs
      mac-pass-control-to-system nil
      ns-command-modifier 'control ; For Kinesis Advantage 2 keyboard
      mac-command-modifier 'control
      ns-right-option-modifier 'left
      mac-right-option-modifier 'left)

;; NOTE This unbinds +popup/toggle
;; In line with MacOS shortcut and HYPER-SPC when hammerspoon is active
(map! "C-`" #'other-frame)

;; As noted in
;; https://github.com/noctuid/evil-guide#why-dont-keys-defined-with-evil-define-key-work-immediately,
;; keymap normalization may be required in some cases. One seems to be use of
;; edebug-mode-map as an evil-intercept map. Without normalization, if in normal
;; mode SPC will trigger leader until you first switch to another evil state.
(add-hook 'edebug-mode-hook #'evil-normalize-keymaps)
(add-hook 'edebug-mode-hook (defun emacs-state-for-edebug ()
                              (if edebug-mode (evil-emacs-state)
                                (evil-exit-emacs-state))))

(setq confirm-kill-emacs nil)

(after! evil
  (define-key evil-motion-state-map "go" #'consult-outline))

;; Disable smartparens
(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)


;; Default comint settings for an easy to use, immutable, evil-compliant
;; comint-based REPL. Some hooks may override these values. In particular, ess-r
;; overrides comint-use-prompt-regexp and needs to be handled via setq-hook!.
(setq comint-prompt-read-only t ; Read-only prompt
      comint-use-prompt-regexp nil ; nil enables evil motions
      inhibit-field-text-motion nil ; Read-only prompt acts as beginning of line for motions
      comint-scroll-to-bottom-on-input t ; Prevent modifying outside prompt line
      comint-scroll-show-maximum-output t)

;; See core-keybinds.el for additional settings
(setq which-key-idle-delay 0.3
      ;; NOTE This is less useful when Doom supplies its own descriptions
      which-key-compute-remaps t)
;; Enable which-key paging for help-map
(general-unbind help-map "C-h")

;;; Miscellaneous

;; TODO Bind vertico-repeat-select and/or vertico-repeat-last

;; Configure `consult-outline' as a scaled down TOC in a separate buffer
;; https://github.com/minad/consult/issues/552
;; (consult-customize consult-outline :preview-key nil)
;; (vertico-multiform-mode)
;; (setq vertico-multiform-commands '((consult-outline buffer)))


;; For use with +eval/open-repl-other-window
;; TODO eval module is a hot mess. Abandoning it for now.
;; (map! :mode ess-r-mode
;;       :gin "C-RET" #'+eval/line-or-region
;;       :gin "<C-return>" #'+eval/line-or-region)
;; (map! :mode python-mode
;;       :gin "C-RET" #'+eval/line-or-region
;;       :gin "<C-return>" #'+eval/line-or-region)
;; (map! :mode ess-r-mode
;;       :gin "C-RET" #'ess-eval-region-line-and-step
;;       :gin "<C-return>" #'ess-eval-region-or-line-and-step)

;; Free up "q" in lots of modes
(general-unbind evil-normal-state-map "q")

;; Make "C-h h" do the same thing without a key prefix as with one
(general-def help-map "h" #'my/embark-bindings)

;; TODO Replace attach and properies with icons
;; https://thibautbenjamin.github.io/emacs/org-icons
;; Smaller org drawers
;; (custom-set-faces! '(org-drawer :height 100))

;; Don't truncate results when using edebug
(setq edebug-print-length 1000)

;; Try xref-find-definitions without anything at point and with something
;; at point. Notice that "gd" or "SPC c d" can't do both!
(setq xref-show-definitions-function #'xref-show-definitions-completing-read)

;; TODO Checkout shortdoc-display-group! It's a cheatsheet!


;; doom-init-leader-keys-h binds to general-override-mode-map and calls
;; general-override-mode (see also general-override-auto-enable). The only
;; bindings are those leader and localleader, M-x, and A-x. This overrides
;; any other evil binding and nearly every other binding in Emacs. See:
;; https://discourse.doomemacs.org/t/how-to-re-bind-keys/56
;; - This says that evil-normal-state-local-map (evil-local-set-key) has lower
;;   precedence than auxiliary keymaps, but this is wrong.
;; https://github.com/noctuid/general.el#override-keymaps-and-buffer-local-keybindings
;; https://github.com/noctuid/evil-guide#keymap-precedence
;; ~/doom-emacs/.local/straight/repos/evil/evil-core.el
(setq doom-leader-alt-key "M-SPC")
(setq doom-localleader-alt-key "M-SPC m")

;; See core-ui.el
(after! ediff
  (remove-hook 'ediff-before-setup-hook #'doom-ediff-save-wconf-h)
  (add-hook 'ediff-before-setup-hook (defun ediff-in-new-frame ()
                                       (select-frame (make-frame))))
  (remove-hook! (ediff-quit-hook ediff-suspend-hook)
    #'doom-ediff-restore-wconf-h)
  (add-hook! 'ediff-quit-hook :append #'delete-frame))

(use-package! corfu
  :hook (doom-first-input . global-corfu-mode)
  :init
  (setq corfu-cycle nil
        corfu-auto t
        corfu-auto-prefix 1
        ;; Keep corfu alive without a match after `corfu-insert-separator'
        corfu-quit-no-match 'separator
        ;; After `corfu-insert-separator', do not quit at word boundary
        ;; (such as `corfu-separator' when set to `?\s')
        corfu-quit-at-boundary 'separator
        ;; What to insert by `corfu-insert-separator'
        corfu-separator ?\s
        corfu-echo-documentation nil)
  (setq tab-always-indent 'complete)
  (advice-add 'corfu--setup :after 'evil-normalize-keymaps)
  (advice-add 'corfu--teardown :after 'evil-normalize-keymaps)
  :config
  (corfu-history-mode))

;; NOTE Uncomment If you would like to use corfu instead of vertico when
;; completing in the minibuffer (e.g., with `M-:')
;; (defun corfu-enable-in-minibuffer ()
;;   "Enable Corfu in the minibuffer if `completion-at-point' is bound."
;;   (when (where-is-internal #'completion-at-point (list (current-local-map)))
;;     ;; (setq-local corfu-auto nil) Enable/disable auto completion
;;     (corfu-mode 1)))
;; (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)

;; (use-package! corfu-doc
;;   :init (setq corfu-doc-auto t)
;;   :hook (corfu-mode . corfu-doc-mode))

(use-package! cape)

(use-package! kind-icon
  :after corfu
  :init (setq kind-icon-default-face 'corfu-default)
  :config (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(evil-define-key 'insert corfu-map
  [down]        #'corfu-next
  (kbd "C-n")   #'corfu-next
  [up]          #'corfu-previous
  (kbd "C-p")   #'corfu-previous
  (kbd "C-b")   #'corfu-scroll-down
  (kbd "C-f")   #'corfu-scroll-up
  (kbd "C-,")   #'corfu-first
  (kbd "C-.")   #'corfu-last
  ;; This will shadow the binding to evil-escape in
  ;; ~/doom-emacs/modules/editor/evil/config.el
  (kbd "C-g")   #'corfu-reset
  (kbd "C-SPC") #'corfu-insert-separator
  (kbd "C-h")   #'corfu-info-documentation ; Works with scroll-other-window(-down)
  [tab]         #'corfu-insert
  (kbd "TAB")   #'corfu-insert
  ;;(kbd "C-h")   #'corfu-doc-toggle
  (kbd "M-p")   #'corfu-doc-scroll-down
  (kbd "M-n")   #'corfu-doc-scroll-up)
;; TODO evil-org interferes with RET. This doesn't seem to be an issue in the
;; company module. See the fix for #1335 in company/config.el.
;; Elsewhere, unbinding RET when `corfu-preselect-first' is enabled allows us to
;; insert a newline without also completing
(after! corfu
  (define-key corfu-map (kbd "RET") nil))

;; NOTE This has the side effect of quiting corfu rather than banishing a
;; doc buffer created by `corfu-info-documentation' when it is called
;; (defun corfu-abort ()
;;   "Undo changes made while corfu was active and quit. Takes one keystroke compared
;; to two to three for corfu-reset but resets changes all at once rather than
;; incrementally."
;;   (interactive)
;;   (cancel-change-group corfu--change-group)
;;   (corfu-quit))

;; HACK `corfu-info-documentation' spawns a help buffer, which the popup module
;; catches, but the two are not yet compatible. `scroll-other-window' will
;; not scroll the help buffer, and "q" will quit the original buffer. (The latter
;; behavior is likely tied to corfu restoring the window configuration via
;; pre-command-hook.)
(defadvice! +popup--ignore-corfu-info-documentation (fn &rest args)
  :around #'corfu-info-documentation
  (push '("^\\*\\([Hh]elp\\|Apropos\\)" nil) display-buffer-alist)
  (apply fn args)
  (pop display-buffer-alist))

;; Based on https://github.com/minad/corfu/wiki, but it actually works...
;; NOTE This setup will not be torn down properly if lsp-mode is disabled.
;; See the definition of lsp-completion-mode if you want to improve the
;; code
(unless (featurep! :completion company)
  (setq lsp-completion-provider :none)
  ;; OPTIONAL: The first word uses orderless-flex for filtering. This means the
  ;; characters are matched in order but do not have to be consecutive. It
  ;; returns more results than a pure orderless style would, at least initially
  ;; (add-hook 'lsp-completion-mode-hook
  ;; NOTE This also affects vertico; e.g., you can't use regex like "^" on the first word
  ;;           (lambda ()
  ;;             (add-to-list 'orderless-style-dispatchers
  ;;                          (lambda (_pattern index _total)
  ;;                            (and (eq index 0) 'orderless-flex)))))
  ;; Make lsp completion use orderless
  (add-hook 'lsp-completion-mode-hook
            (lambda ()
              (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
                    '(orderless))))
  ;; Bust capf caches, refreshing candidates more often (e.g., on backspace)
  (add-hook 'lsp-completion-mode-hook
            (lambda ()
              (setq-local completion-at-point-functions
                          (list (cape-capf-buster #'lsp-completion-at-point))))))



;; NOTE Keyboards can send either C-m / RET or <return>. <return> is
;; automatically translated to RET if no <return> binding exists. If a <return>
;; binding exists, it shadows RET. This seems to take precedence over map
;; precedence. E.g., <return> in any keymap shadows RET in any keymap. To
;; normalize doom return keybindings, search for C-m, C-ret, C-<return>, c-m-m,
;; c-m-ret, and c-m-<return> Safest option is probably to bind RET and <return>.
;; ;; https://www.fromkk.com/posts/c-m-ret-and-return-key-in-emacs/

;; insert state -> org-tab-first-hook -> org-indent-maybe-h -> indent-for-tab-command (src); else org-cycle -> global-key-binding TAB -> indent-for-tab-command
;; not org -> indent-for-tab-command (see tab-always-indent and indent-line-function)


;;; notes

;; Exclude ATTACH tags from org-roam database
(setq org-roam-db-node-include-function
      (lambda ()
        (not (member org-attach-auto-tag (org-get-tags nil t)))))

;; Use org-directory as org-roam-directory
(setq org-roam-directory "")
(after! org-roam
  ;; Since doom already displays subdirectories kind of like tags in org-roam-node-find,
  ;; why not make capturing to a subdirectory painless?
  (setq org-roam-capture-templates nil)
  (add-to-list 'org-roam-capture-templates
               '("d" "default" plain "%?"
                 :target (file+head
                          "%(read-directory-name \"path: \" org-roam-directory)/%<%Y%m%d%H%M%S>-${slug}.org"
                          "#+title: ${title}\n")
                 :unnarrowed t)))

;; See https://retorque.re/zotero-better-bibtex/exporting/pull/
;; Right click a Zotero collection and select "Download Betterbibtex export" to get the URL
;; TODO This doesn't work on WSL because firewall rules must be disabled to allow WSL to connect to Windows
(defun update-bib-file ()
  (interactive)
  (let ((root_url (shell-quote-argument
                   ;; Use $(hostname).local in lieu of the IP for localhost on WSL
                   "http://127.0.0.1:23119/better-bibtex/export/collection?/1/org-cite.biblatex"))) ; &exportNotes=true
    (shell-command (format "wget %s -O %s" root_url citar-bibliography))))


;; Delete values for extensions.zotero.annotations.noteTemplates.title and
;; extensions.zotero.annotations.noteTemplates.note. Then only highlight
;; annotations will be exported, which simplifies the regexp. This is fine
;; because only highlight annotations contain a link back to the location in
;; the PDF.
;; Right click one or more items, "Add note from annotations"
;; Right click a single note, "Export note" as markdown including zotero links
;; TODO You can select multiple notes, and they will be separated by "---"
;; TODO Format of notes is configurable: https://www.zotero.org/support/note_templates
;; NOTE May have to delete previous annotation file for subsequent export to succeed. If note template contains no title, you need to choose a filename
(defun import-zotero-annotations-from-note (buf)
  "Import Zotero annotations from a markdown notes-export file,
convert the annotations to org-mode links with annotation
comments underneath, and display the buffer"
  (interactive
   (list (find-file-noselect (read-file-name "Markdown Zotero notes file: "))))
  (with-current-buffer buf
    (beginning-of-buffer)
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

;; Open non-text files in Windows instead of WSL
(setq IS-WSL (and (string-match "-[Mm]icrosoft" operating-system-release)
                  (eq system-type 'gnu/linux)))
(when IS-WSL
  ;; TODO Migrate attachments?
  (setq org-attach-id-dir "/mnt/c/Users/jkroes/OneDrive - California Department of Pesticide Regulation/org-attach")
  ;; (setq browse-url-generic-program "wslview"
  ;;       browse-url-browser-function 'browse-url-generic)
  ;; (setf (alist-get 'system org-file-apps-gnu) "wslview \"%s\"")
  ;; (setf (alist-get t org-file-apps-gnu) "wslview \"%s\""))
  ;; NOTE open-in-windows is faster than wslview and seems to work more consistently
  ;; Not sure why wslview doesn't open PDFs at all, let alone in windows
  (setq browse-url-generic-program "/mnt/c/Windows/System32/cmd.exe"
        browse-url-generic-args '("/c" "start" "")
        browse-url-browser-function 'browse-url-generic)
  (after! org
    (setf (alist-get "\\.pdf\\'" org-file-apps nil nil #'string=) #'open-in-windows)
    ;; (add-to-list 'org-file-apps '("^/mnt/" . #'open-in-windows))
    (add-to-list 'org-file-apps '("\\.xlsx?\\'" . open-in-windows) t)
    (add-to-list 'org-file-apps '("\\.docx?\\'" . open-in-windows) t)
    (add-to-list 'org-file-apps '("\\.pptx?\\'" . open-in-windows) t)))

;; NOTE Marginalia annotates minibuffer completions, but if a minibuffer-completion
;; command is not the top-level command executed, it might not be annotated
;; correctly. E.g. I modified +org/dwim-at-point to call org-attach-open. To
;; have these commands annotated correctly, you need to either rebind this-command
;; to the command you want annotated before executing it, or else execute it
;; via execute-extended-command; e.g.
;; (execute-extended-command nil "org-attach-open")

;; Configure org-attach-open to work with embark
;; Unlike e.g. `find-file', `org-attach-open' does not use the path returned by
;; `org-attach-dir' as minibuffer input. `embark--vertico-selected' constructs
;; embark targets from the candidate and the minibuffer input, so the target
;; does not have the full path. By associating `org-attach-open' to a novel
;; marginalia category, and this category to an embark transformer function, we
;; end up with the full filepath and a final `file' category that allows us to
;; execute actions from `embark-file-map' apples
(after! marginalia
  (add-to-list 'marginalia-command-categories '(org-attach-open . attach)))
(after! embark
  (add-to-list 'embark-transformer-alist
               '(attach . embark--expand-attachment)))
(defun embark--expand-attachment (_type target)
  (with-current-buffer (window-buffer (minibuffer-selected-window))
    (cons 'file (expand-file-name target (org-attach-dir)))))


;; HACK org-attach commands ignore ., .., and files ending in ~, but
;; we also want to ignore .DS_STORE on MacOS.
(defvar org-attach-ignore-regexp-list (list "." ".." ".DS_STORE")
  "A list of filenames for org-attach to ignore")
(after! org-attach
  (defun org-attach-file-list (directory)
    "Return a list of files in the attachment DIRECTORY.
This ignores files ending in \"~\"."
    (delq nil
	  (mapcar (lambda (x)
                    (if (string-match
                         (concat "^"
                                 (regexp-opt
                                  org-attach-ignore-regexp-list)
                                 "\\'")
                         x) nil x))
		  (directory-files directory nil "[^~]\\'")))))

;; TODO Setup org-transclusion
;; TODO Set up biblio module

;; Note about keybindings in -nw (terminal) mode:
;; On MacOS, I configure Emacs to translate the CMD to CTRL key. For whatever
;; reason--perhaps because CMD isn't a recognized key for terminal input--
;; CMD-h doesn't produce C-h. It runs the system binding, which by default hides
;; windows. One way to get around this is to remap CMD to CTRL in iTerm's
;; settings. This overrides all uses of cmd shortcuts for iTerm and MacOS.
;; To allow specific exceptions: Disable remapping. Disable the MacOS shortcut.
;; iTerm preferences>Keys>Key Bindings>+>click then type keybinding, and set
;; action to "do not remap modifiers">reenable remapping and system shortcut

;; HACK r languageserver apparently returns an unformatted sring when pressing
;; "C-h" during completion
;; TODO Even with wrapping, the formatting is still wildly off compared to
;; pressing "K" over a symbol
;; NOTE Today I learned you can use "C-h" when completing a function parameter, and
;; it works in ess-r-mode.
(add-hook 'window-configuration-change-hook
          (defun test()
            (dolist (buff (buffer-list))
              (when (string= (buffer-name buff) " *eglot doc*")
                (with-current-buffer buff
                  (visual-line-mode))))))
