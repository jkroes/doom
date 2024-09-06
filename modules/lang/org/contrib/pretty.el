;;; lang/org/contrib/pretty.el -*- lexical-binding: t; -*-
;;;###if (modulep! +pretty)
(after! org
  (setq org-highlight-latex-and-related '(native script entities)))

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

  ;; Hide listed keywords
  (setq org-hidden-keywords '(title)
        org-appear-autokeywords t)

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

  ;; org-appear handles this for now
  (setq org-modern-keyword nil)

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
