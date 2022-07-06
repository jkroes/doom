;;; lang/ess/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +ess/open-julia-repl (&optional arg)
  "Open an ESS Julia REPL"
  (interactive "P")
  (run-ess-julia arg)
  (current-buffer))

;;;###autoload
;; See set-repl-handler! call in modules/lang/ess/config.el. The function passed
;; to it (i.e., this one below) should return the REPL buffer, but the use of
;; current-buffer in the original version of this function returned the R source
;; code buffer. This incorrect buffer made its way to +popup-buffer; as a
;; result, the REPL buffer was created but not displayed as a popup. In fact, it
;; was not displayed in any window.
(defun +ess/open-r-repl (&optional arg)
  "Open an ESS R REPL"
  (interactive "P")
  (run-ess-r arg))

;;;###autoload
(defun my/ess-display-help-on-object (object &optional command)
  "Modified ess-display-help-on-object that returns rather than displays buffer.
For use with my/company-show-doc-buffer."
  (interactive
   (progn
     (ess-force-buffer-current)
     (when current-prefix-arg
       (ess-help--reset-cache))
     (list (ess-find-help-file "Help on"))))
  (let* ((hb-name (concat "*help[" ess-current-process-name "]("
                          (replace-regexp-in-string "^\\?\\|`" "" object) ")*"))
         (old-hb-p (get-buffer hb-name))
         (tbuffer (get-buffer-create hb-name)))
    (when (or (not old-hb-p)
              (ess-process-get 'sp-for-help-changed?)
              (ess--help-get-bogus-buffer-substring old-hb-p))
      (ess-with-current-buffer tbuffer
        (ess--flush-help-into-current-buffer object command)
        (setq ess-help-object object)
        (ess--help-major-mode)
        (setq truncate-lines nil
              ess-help-type 'help)))
    tbuffer))
