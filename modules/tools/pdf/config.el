;;; tools/pdf/config.el -*- lexical-binding: t; -*-

(use-package! pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :init
  (after! pdf-annot
    (defun +pdf-cleanup-windows-h ()
      "Kill left-over annotation buffers when the document is killed."
      (when (buffer-live-p pdf-annot-list-document-buffer)
        (pdf-info-close pdf-annot-list-document-buffer))
      (when (buffer-live-p pdf-annot-list-buffer)
        (kill-buffer pdf-annot-list-buffer))
      (let ((contents-buffer (get-buffer "*Contents*")))
        (when (and contents-buffer (buffer-live-p contents-buffer))
          (kill-buffer contents-buffer))))
    (add-hook! 'pdf-view-mode-hook
      (add-hook 'kill-buffer-hook #'+pdf-cleanup-windows-h nil t)))

  ;; In case global-display-line-numbers-mode is set. It is mentioned as an issue
  ;; in the README and disables horizontal scrolling in PDFs from my own testing
  (add-hook 'pdf-view-mode-hook (lambda ()(display-line-numbers-mode -1)))

  :config
  (defadvice! +pdf--install-epdfinfo-a (fn &rest args)
    "Install epdfinfo after the first PDF file, if needed."
    :around #'pdf-view-mode
    (if (file-executable-p pdf-info-epdfinfo-program)
        (apply fn args)
      ;; If we remain in pdf-view-mode, it'll spit out cryptic errors. This
      ;; graceful failure is better UX.
      (fundamental-mode)
      (message "Viewing PDFs in Emacs requires epdfinfo. Use `M-x pdf-tools-install' to build it")))

  (pdf-tools-install-noverify)

  ;; For consistency with other special modes
  (map! :map pdf-view-mode-map :gn "q" #'kill-current-buffer)

  ;;(setq-default pdf-view-display-size 'fit-page)
  ;; Enable hiDPI support, but at the cost of memory! See politza/pdf-tools#51
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil)

  ;; Handle PDF-tools related popups better
  (set-popup-rules!
    '(("^\\*Outline*" :side right :size 40 :select nil)
      ("^\\*Edit Annotation " :quit nil)
      ("\\(?:^\\*Contents\\|'s annots\\*$\\)" :ignore t)))

  ;; The mode-line does serve any useful purpose is annotation windows
  (add-hook 'pdf-annot-list-mode-hook #'hide-mode-line-mode)

  ;; HACK Fix #1107: flickering pdfs when evil-mode is enabled
  (setq-hook! 'pdf-view-mode-hook evil-normal-state-cursor (list nil))

  ;; HACK Refresh FG/BG for pdfs when `pdf-view-midnight-colors' is changed by a
  ;;      theme or with `setq!'.
  ;; TODO PR this upstream?
  (defun +pdf-reload-midnight-minor-mode-h ()
    (when pdf-view-midnight-minor-mode
      (pdf-info-setoptions
       :render/foreground (car pdf-view-midnight-colors)
       :render/background (cdr pdf-view-midnight-colors)
       :render/usecolors t)
      (pdf-cache-clear-images)
      (pdf-view-redisplay t)))
  (put 'pdf-view-midnight-colors 'custom-set
       (lambda (sym value)
         (set-default sym value)
         (dolist (buffer (doom-buffers-in-mode 'pdf-view-mode))
           (with-current-buffer buffer
             (if (get-buffer-window buffer)
                 (+pdf-reload-midnight-minor-mode-h)
               ;; Defer refresh for buffers that aren't visible, to avoid
               ;; blocking Emacs for too long while changing themes.
               (add-hook 'doom-switch-buffer-hook #'+pdf-reload-midnight-minor-mode-h
                         nil 'local))))))

  ;; Silence "File *.pdf is large (X MiB), really open?" prompts for pdfs
  (defadvice! +pdf-suppress-large-file-prompts-a (fn size op-type filename &optional offer-raw)
    :around #'abort-if-file-too-large
    (unless (string-match-p "\\.pdf\\'" filename)
      (funcall fn size op-type filename offer-raw))))


(use-package! saveplace-pdf-view
  :after pdf-view)

;; Mappings

(defvar pdf-view-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (set-keymap-parent map image-mode-map)
    ;; Navigation in the document
    (define-key map "h" 'image-scroll-right)
    (define-key map "l" 'image-scroll-left)
    (define-key map "j" 'pdf-view-scroll-up-or-next-page)
    (define-key map "k" 'pdf-view-scroll-down-or-previous-page)
    (define-key map "J" 'pdf-view-next-page-command)
    (define-key map "K" 'pdf-view-previous-page-command)
    (define-key map (kbd "g g") 'pdf-view-first-page)
    (define-key map "G" 'pdf-view-last-page)
    (define-key map (kbd "g t") 'pdf-view-goto-page)
    ;; Zoom in/out.
    (define-key map "+" 'pdf-view-enlarge)
    (define-key map "=" 'pdf-view-enlarge)
    (define-key map "-" 'pdf-view-shrink)
    (define-key map "0" 'pdf-view-scale-reset)
    ;; Fit the image to the window
    (define-key map (kbd "w w") 'pdf-view-fit-width-to-window)
    (define-key map (kbd "w h") 'pdf-view-fit-height-to-window)
    (define-key map (kbd "w p") 'pdf-view-fit-page-to-window)
    ;; Slicing the image
    (define-key map (kbd "s s") 'pdf-view-set-slice-from-bounding-box)
    (define-key map (kbd "s r") 'pdf-view-reset-slice)
    ;; Region
    (define-key map [down-mouse-1] 'pdf-view-mouse-set-region)
    (define-key map [C-down-mouse-1] 'pdf-view-mouse-extend-region)
    ;; NOTE: Until isearch is evilified, e.g., `n' does nothing but `C-s'
    ;; moves to the next match as expected for isearch
    (define-key map "/" 'isearch-forward)
    (define-key map "?" 'isearch-backward)
    map)
  "User-modified keymap used by `pdf-view-mode' when displaying a doc as a set
of images.")

(defvar pdf-outline-buffer-mode-map
  (let ((kmap (make-sparse-keymap)))
    (dotimes (i 10)
      (define-key kmap (vector (+ i ?0)) 'digit-argument))
    (define-key kmap "-" 'negative-argument)
    (define-key kmap (kbd "j") 'next-line)
    (define-key kmap (kbd "k") 'previous-line)
    (define-key kmap (kbd "g g") 'beginning-of-buffer)
    (define-key kmap "G" 'pdf-outline-end-of-buffer)
    ;; Display and move to page
    (define-key kmap (kbd "RET") 'pdf-outline-follow-link-and-quit)
    ;; Move to the PDF window (move back to outline with the same key)
    (define-key kmap (kbd "o") 'pdf-outline-select-pdf-window)
    ;; Display page and remain in outline
    (define-key kmap (kbd "f") 'pdf-outline-display-link)
    ;; PDF follows along as you navigate the outline
    (define-key kmap (kbd "F") 'pdf-outline-follow-mode)
    ;; Move within outline to the item for the displayed page
    (define-key kmap (kbd "'") 'pdf-outline-move-to-current-page)
    (define-key kmap (kbd "`") 'pdf-outline-move-to-current-page)
    (define-key kmap (kbd "q") 'quit-window)
    (define-key kmap (kbd "Q") 'pdf-outline-quit)
    (define-key kmap (kbd "C-c C-q") 'pdf-outline-quit-and-kill)
    kmap)
  "User-modified keymap used in `pdf-outline-buffer-mode'.")
