;;; completion/company/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defvar +company-backend-alist nil
  "An alist matching modes to company backends. The backends for any mode is
built from this.")

;;;###autodef
(defun set-company-backend! (modes &rest backends)
  "For each mode in MODES, create an element in `+company-backend-alist' with
car mode and cdr backends.

MODES should be one symbol or a list of them, representing major or minor modes.
This will overwrite backends for MODES on consecutive uses.

If the car of BACKENDS is nil, unset the elements with car in MODES.

Examples:

  (set-company-backend! 'js2-mode
    'company-tide 'company-yasnippet)

  (set-company-backend! 'sh-mode
    '(company-shell :with company-yasnippet))

  (set-company-backend! '(c-mode c++-mode)
    '(:separate company-irony-c-headers company-irony))

  (set-company-backend! 'sh-mode nil)  ; unsets backends for sh-mode"
  (declare (indent defun))
  (dolist (mode (doom-enlist modes))
    (if (null (car backends))
        (setq +company-backend-alist
              (delq (assq mode +company-backend-alist)
                    +company-backend-alist))
      (setf (alist-get mode +company-backend-alist)
            backends))))


;;
;;; Library

(defun +company--backends ()
  "Merge backends for the current buffer's major-mode, parent modes,
and minor modes specified in `+company-backend-alist' and the
`default-value' of `company-backends'."
  (let (backends)
    (let ((mode major-mode)
          (modes (list major-mode)))
      (while (setq mode (get mode 'derived-mode-parent))
        (push mode modes))
      (dolist (mode modes)
        (dolist (backend (append (cdr (assq mode +company-backend-alist))
                                 (default-value 'company-backends)))
          (push backend backends)))
      (delete-dups
       (append (cl-loop for (mode . backends) in +company-backend-alist
                        if (or (eq major-mode mode)  ; major modes
                               (and (boundp mode)
                                    (symbol-value mode))) ; minor modes
                        append backends)
               (nreverse backends))))))


;;
;;; Hooks

;;;###autoload
(defun +company-init-backends-h ()
  "Set `company-backends' for the current buffer."
  (or (memq major-mode '(fundamental-mode special-mode))
      buffer-read-only
      (doom-temp-buffer-p (or (buffer-base-buffer) (current-buffer)))
      (setq-local company-backends (+company--backends))))

(put '+company-init-backends-h 'permanent-local-hook t)


;;
;;; Commands

;;;###autoload
(defun +company-has-completion-p ()
  "Return non-nil if a completion candidate exists at point."
  (when company-mode
    (unless company-candidates-length
      (company-manual-begin))
    (= company-candidates-length 1)))

;;;###autoload
(defun +company/toggle-auto-completion ()
  "Toggle as-you-type code completion."
  (interactive)
  (require 'company)
  (setq company-idle-delay (unless company-idle-delay 0.2))
  (message "Auto completion %s"
           (if company-idle-delay "enabled" "disabled")))

;;;###autoload
(defun +company/complete ()
  "Bring up the completion popup. If only one result, complete it."
  (interactive)
  (require 'company)
  (when (ignore-errors
          (/= (point)
              (cdr (bounds-of-thing-at-point 'symbol))))
    (save-excursion (insert " ")))
  (when (and (company-manual-begin)
             (= company-candidates-length 1))
    (company-complete-common)))

;;;###autoload
(defun +company/dabbrev ()
  "Invokes `company-dabbrev-code' in prog-mode buffers and `company-dabbrev'
everywhere else."
  (interactive)
  (call-interactively
   (if (derived-mode-p 'prog-mode)
       #'company-dabbrev-code
     #'company-dabbrev)))

;;;###autoload
(defun +company/whole-lines (command &optional arg &rest ignored)
  "`company-mode' completion backend that completes whole-lines, akin to vim's
C-x C-l."
  (interactive (list 'interactive))
  (require 'company)
  (pcase command
    (`interactive (company-begin-backend '+company/whole-lines))
    (`prefix      (company-grab-line "^[\t\s]*\\(.+\\)" 1))
    (`candidates
     (all-completions
      arg
      (delete-dups
       (split-string
        (replace-regexp-in-string
         "^[\t\s]+" ""
         (concat (buffer-substring-no-properties (point-min) (line-beginning-position))
                 (buffer-substring-no-properties (line-end-position) (point-max))))
        "\\(\r\n\\|[\n\r]\\)" t))))))

;;;###autoload
(defun +company/dict-or-keywords ()
  "`company-mode' completion combining `company-dict' and `company-keywords'."
  (interactive)
  (require 'company-dict)
  (require 'company-keywords)
  (let ((company-backends '((company-keywords company-dict))))
    (call-interactively #'company-complete)))

;;;###autoload
(defun +company/dabbrev-code-previous ()
  "TODO"
  (interactive)
  (require 'company-dabbrev)
  (let ((company-selection-wrap-around t))
    (call-interactively #'+company/dabbrev)
    (company-select-previous-or-abort)))


;;;###autoload
(defcustom company-box-doc-no-wrap nil
  "Specify whether or not to wrap the documentation box at the edge of the
Emacs frame."
  :type 'boolean
  :group 'company-box)

;;;###autoload
(defun my/company-box-doc--set-frame-position (frame)
  (-let* ((box-position (frame-position (company-box--get-frame)))
          (box-width (frame-pixel-width (company-box--get-frame)))
          (window (frame-root-window frame))
          (frame-resize-pixelwise t)
          ((width . height) (window-text-pixel-size window nil nil 10000
                                                    10000))
          ((width . height)
           (if company-box-doc-no-wrap
               (window-text-pixel-size window nil nil 10000 10000)
             (window-text-pixel-size window nil nil
                                     (- (frame-outer-width)
                                        (+ 20 (+ box-width
                                                 (nth 0 box-position))))
                                     (- (frame-outer-height) 20))
             ))
          (bottom (+ company-box--bottom (window-pixel-top)
                     (frame-border-width)))
          (x (+ (car box-position) box-width (/ (frame-char-width) 2)))
          (y (cdr box-position))
          (y (if (> (+ y height 20) bottom)
                 (- y (- (+ y height) bottom) 20)
               y))
          (space-right (- (frame-pixel-width) x))
          (space-left (car box-position))
          (x (or (let ((border (* (or (alist-get
                                       'internal-border-width
                                       company-box-doc-frame-parameters)
                                      0)
                                  2)))
                   (and (> width space-right)
                        (> space-left (+ width border
                                         (/ (frame-char-width) 2)))
                        (- (car box-position) width border
                           (/ (frame-char-width) 2))))
                 x)))
    (set-frame-position frame (max x 0) (max y 0))
    (set-frame-size frame width height t)))

;;;###autoload
(defun my/company-box-doc--make-buffer (object)
  (let* ((buffer-list-update-hook nil)
         (inhibit-modification-hooks t)
         (string (cond ((stringp object) object)
                       ((bufferp object)
                        (with-current-buffer object (buffer-string))))))
    (when (and string (> (length (string-trim string)) 0))
      (with-current-buffer (company-box--get-buffer "doc")
        (erase-buffer)
        (insert string)
        (setq mode-line-format nil
              display-line-numbers nil
              header-line-format nil
              show-trailing-whitespace nil
              cursor-in-non-selected-windows nil)
        ;; TODO Get rid of echo area messages
        (toggle-truncate-lines -1)
        (current-buffer)))))

;; NOTE The reason I used this in lieu of ess-display-help-on-object is that I
;; like the way help disappears when you navigate to a different company
;; candidate. The ess help buffer is permanent, and you have to choose whether
;; or not you want to focus on help buffers (ess-help-pop-to-buffer).
;;;###autoload
(defun my/company-show-doc-buffer ()
  "Modified company-show-doc-buffer that displays ess-r help instead of company
docs (including docs from R languageserver) for ess-r-mode"
  (interactive)
  (let ((other-window-scroll-buffer)
        (selection (or company-selection 0)))
    (company--electric-do
      (let* ((selected (nth selection company-candidates))
             (doc-buffer (or (company-call-backend 'doc-buffer selected)
                             (user-error "No documentation available")))
             start)
        (when (eq major-mode 'ess-r-mode)
          (setq doc-buffer (my/ess-display-help-on-object selected)))
        (when (consp doc-buffer)
          (setq start (cdr doc-buffer)
                doc-buffer (car doc-buffer)))
        (setq other-window-scroll-buffer (get-buffer doc-buffer))
        (let ((win (display-buffer doc-buffer t)))
          (set-window-start win (if start start (point-min))))))))
(put 'my/company-show-doc-buffer 'company-keep t)
