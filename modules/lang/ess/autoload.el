;;; lang/ess/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +ess/open-julia-repl (&optional arg)
  "Open an ESS Julia REPL"
  (interactive "P")
  (run-ess-julia arg)
  (current-buffer))

;;;###autoload
(defun +ess/open-r-repl (&optional arg)
  "Open an ESS R REPL"
  (interactive "P")
  (run-ess-r arg)
  (current-buffer))

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
