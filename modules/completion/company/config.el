;;; completion/company/config.el -*- lexical-binding: t; -*-

;; The default bindings shadow tab in multiple places, since evil uses
;; higher precedence emulation maps than the maps of minor modes. It shadows
;; company-mode-map in particular. See
;; ~/doom-emacs/modules/config/default/+evil-bindings.el, the comment about
;; #4548. That has to be disabled for this to work.
;; (map! :map company-mode-map
;;       "<tab>" 'company-indent-or-complete-common
;;       "TAB" 'company-indent-or-complete-common)


(use-package! company
  :commands (company-complete-common
             company-complete-common-or-cycle
             company-manual-begin
             company-grab-line)
  ;; TODO If company-mode bindings (e.g., TAB) are shadowing non-prog major mode
  ;; bindings, consider changing this to (prog-mode . company-mode) or adding
  ;; excluded modes to company-global-modes
  :hook (doom-first-input . global-company-mode)
  :init
  (setq company-minimum-prefix-length 2
        company-tooltip-limit 14
        company-tooltip-align-annotations t
        ;; Allow typing chars that don't match candidates
        company-require-match nil
        company-global-modes
        '(not erc-mode
              circe-mode
              message-mode
              help-mode
              gud-mode
              vterm-mode)
        company-frontends
        '(company-pseudo-tooltip-frontend  ; always show candidates in overlay tooltip
          company-echo-metadata-frontend)  ; show selected candidate docs in echo area

        ;; Buffer-local backends will be computed when loading a major mode, so
        ;; only specify a global default here.
        company-backends nil

        ;; These auto-complete the current selection when
        ;; `company-auto-commit-chars' is typed. When disabled, nonmatching
        ;; characters (see `company-require-match') end completion without
        ;; inserting a candidate
        company-auto-commit nil

        ;; Only search the current buffer for `company-dabbrev' (a backend that
        ;; suggests text your open buffers). This prevents Company from causing
        ;; lag once you have a lot of buffers open.
        company-dabbrev-other-buffers nil
        ;; Make `company-dabbrev' fully case-sensitive, to improve UX with
        ;; domain-specific words with particular casing.
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil)

  (when (featurep! +tng)
    (add-hook 'global-company-mode-hook #'company-tng-mode))

  :config
  (when (featurep! :editor evil)
    (add-hook 'company-mode-hook #'evil-normalize-keymaps)
    (unless (featurep! +childframe)
      ;; Don't persist company popups when switching back to normal mode.
      ;; `company-box' aborts on mode switch so it doesn't need this.
      (add-hook! 'evil-normal-state-entry-hook
        (defun +company-abort-h ()
          ;; HACK `company-abort' doesn't no-op if company isn't active; causing
          ;;      unwanted side-effects, like the suppression of messages in the
          ;;      echo-area.
          ;; REVIEW Revisit this to refactor; shouldn't be necessary!
          (when company-candidates
            (company-abort)))))
    ;; Allow users to switch between backends on the fly. E.g. C-x C-s followed
    ;; by C-x C-n, will switch from `company-yasnippet' to
    ;; `company-dabbrev-code'.
    (defadvice! +company--abort-previous-a (&rest _)
      :before #'company-begin-backend
      (company-abort)))

  ;; Initialize company backends. `+company-init-backends-h' sets buffer-local
  ;; `company-backends' to the result of calling `+company--backends', which
  ;; merges `+company-backend-alist' and default value of `company-backends'.
  ;; The alist is populated by `set-company-backend!', typically within each
  ;; module's modules/lang/*/config.el
  ;; This is also affected by `+lsp-company-backends' (prepended onto
  ;; `company-backends' even if nil)
  (add-hook 'after-change-major-mode-hook #'+company-init-backends-h 'append)


  ;; NOTE Fix #1335: ensure `company-emulation-alist' is the first item of
  ;;      `emulation-mode-map-alists', thus higher priority than keymaps of
  ;;      evil-mode. We raise the priority of company-mode keymaps
  ;;      unconditionally even when completion is not activated. This should not
  ;;      cause problems, because when completion is activated, the value of
  ;;      `company-emulation-alist' is ((t . company-my-keymap)), when
  ;;      completion is not activated, the value is ((t . nil)).
  (add-hook! 'evil-local-mode-hook
    (when (memq 'company-emulation-alist emulation-mode-map-alists)
      (company-ensure-emulation-alist)))

  ;; Fix #4355: allow eldoc to trigger after completions.
  (after! eldoc
    (eldoc-add-command 'company-complete-selection
                       'company-complete-common
                       'company-capf
                       'company-abort)))


;;
;;; Packages

(after! company-files
  ;; Fix `company-files' completion for org file:* links
  (add-to-list 'company-files--regexps "file:\\(\\(?:\\.\\{1,2\\}/\\|~/\\|/\\)[^\]\n]*\\)"))

;; The README for this package is incorrect. See
;; https://github.com/sebastiencs/company-box/issues/143.
;; TODO Compare to eldoc-box and company-quickhelp.
(use-package! company-box
  :when (featurep! +childframe)
  :hook (company-mode . company-box-mode)
  :config
  ;; Fixes https://github.com/sebastiencs/company-box/issues/165.
  ;; TODO With or without this fix, when a vertical split exists, in the right
  ;; window, and not enough space, the doc childframe displays on the left. But
  ;; it doesn't do this if you have Emacs in a single window in a half-screen
  ;; frame and not enough space. Should be able to display the doc childframe
  ;; outside of Emacs.
  ;; (advice-add 'company-box-doc--set-frame-position
  ;;             :override 'my/company-box-doc--set-frame-position)
  ;; (advice-add 'company-box-doc--make-buffer
  ;;             :override 'my/company-box-doc--make-buffer)
  (define-key company-active-map (kbd "C-h") #'my/company-show-doc-buffer)
  ;;(advice-add 'company-show-doc-buffer :override 'my/company-show-doc-buffer)
  (setq company-box-show-single-candidate t
        company-box-backends-colors nil
        company-box-max-candidates 50
        company-box-icons-alist 'company-box-icons-all-the-icons
        ;; Move company-box-icons--elisp to the end, because it has a catch-all
        ;; clause that ruins icons from other backends in elisp buffers.
        company-box-icons-functions
        (cons #'+company-box-icons--elisp-fn
              (delq 'company-box-icons--elisp
                    company-box-icons-functions))
        company-box-icons-all-the-icons
        (let ((all-the-icons-scale-factor 0.8))
          `((Unknown       . ,(all-the-icons-material "find_in_page"             :face 'all-the-icons-purple))
            (Text          . ,(all-the-icons-material "text_fields"              :face 'all-the-icons-green))
            (Method        . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (Function      . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (Constructor   . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (Field         . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (Variable      . ,(all-the-icons-material "adjust"                   :face 'all-the-icons-blue))
            (Class         . ,(all-the-icons-material "class"                    :face 'all-the-icons-red))
            (Interface     . ,(all-the-icons-material "settings_input_component" :face 'all-the-icons-red))
            (Module        . ,(all-the-icons-material "view_module"              :face 'all-the-icons-red))
            (Property      . ,(all-the-icons-material "settings"                 :face 'all-the-icons-red))
            (Unit          . ,(all-the-icons-material "straighten"               :face 'all-the-icons-red))
            (Value         . ,(all-the-icons-material "filter_1"                 :face 'all-the-icons-red))
            (Enum          . ,(all-the-icons-material "plus_one"                 :face 'all-the-icons-red))
            (Keyword       . ,(all-the-icons-material "filter_center_focus"      :face 'all-the-icons-red))
            (Snippet       . ,(all-the-icons-material "short_text"               :face 'all-the-icons-red))
            (Color         . ,(all-the-icons-material "color_lens"               :face 'all-the-icons-red))
            (File          . ,(all-the-icons-material "insert_drive_file"        :face 'all-the-icons-red))
            (Reference     . ,(all-the-icons-material "collections_bookmark"     :face 'all-the-icons-red))
            (Folder        . ,(all-the-icons-material "folder"                   :face 'all-the-icons-red))
            (EnumMember    . ,(all-the-icons-material "people"                   :face 'all-the-icons-red))
            (Constant      . ,(all-the-icons-material "pause_circle_filled"      :face 'all-the-icons-red))
            (Struct        . ,(all-the-icons-material "streetview"               :face 'all-the-icons-red))
            (Event         . ,(all-the-icons-material "event"                    :face 'all-the-icons-red))
            (Operator      . ,(all-the-icons-material "control_point"            :face 'all-the-icons-red))
            (TypeParameter . ,(all-the-icons-material "class"                    :face 'all-the-icons-red))
            (Template      . ,(all-the-icons-material "short_text"               :face 'all-the-icons-green))
            (ElispFunction . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (ElispVariable . ,(all-the-icons-material "check_circle"             :face 'all-the-icons-blue))
            (ElispFeature  . ,(all-the-icons-material "stars"                    :face 'all-the-icons-orange))
            (ElispFace     . ,(all-the-icons-material "format_paint"             :face 'all-the-icons-pink))))
        ;; Enable auto doc display for candidates
        company-box-doc-enable nil)

  ;; HACK Fix oversized scrollbar in some odd cases
  ;; REVIEW `resize-mode' is deprecated and may stop working in the future.
  ;; TODO PR me upstream?
  (setq x-gtk-resize-child-frames 'resize-mode)

  ;; Disable tab-bar in company-box child frames
  ;; TODO PR me upstream!
  (add-to-list 'company-box-frame-parameters '(tab-bar-lines . 0))

  ;; Don't show documentation in echo area, because company-box displays its own
  ;; in a child frame.
  (delq! 'company-echo-metadata-frontend company-frontends)

  (defun +company-box-icons--elisp-fn (candidate)
    (when (derived-mode-p 'emacs-lisp-mode)
      (let ((sym (intern candidate)))
        (cond ((fboundp sym)  'ElispFunction)
              ((boundp sym)   'ElispVariable)
              ((featurep sym) 'ElispFeature)
              ((facep sym)    'ElispFace)))))

  ;; `company-box' performs insufficient frame-live-p checks. Any command that
  ;; "cleans up the session" will break company-box.
  ;; TODO Fix this upstream.
  (defadvice! +company-box-detect-deleted-frame-a (frame)
    :filter-return #'company-box--get-frame
    (if (frame-live-p frame) frame))
  (defadvice! +company-box-detect-deleted-doc-frame-a (_selection frame)
    :before #'company-box-doc
    (and company-box-doc-enable
         (frame-local-getq company-box-doc-frame frame)
         (not (frame-live-p (frame-local-getq company-box-doc-frame frame)))
         (frame-local-setq company-box-doc-frame nil frame))))


(use-package! company-dict
  :defer t
  :config
  (setq company-dict-dir (expand-file-name "dicts" doom-private-dir))
  (add-hook! 'doom-project-hook
    (defun +company-enable-project-dicts-h (mode &rest _)
      "Enable per-project dictionaries."
      (if (symbol-value mode)
          (add-to-list 'company-dict-minor-mode-list mode nil #'eq)
        (setq company-dict-minor-mode-list (delq mode company-dict-minor-mode-list))))))
