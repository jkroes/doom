;;; lang/ruby/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +ruby-cleanup-robe-servers-h ()  ; TODO Use me
  "Clean up dangling inf robe processes if there are no more `ruby-mode' buffers
open."
  ;; FIXME This should wait X seconds before cleaning up
  (unless (or (not robe-mode) (doom-buffers-in-mode 'ruby-mode))
    (let (inf-buffer kill-buffer-query-functions)
      (while (setq inf-buffer (robe-inf-buffer))
        (let ((process (get-buffer-process inf-buffer))
              confirm-kill-processes)
          (when (processp process)
            (kill-process (get-buffer-process inf-buffer))
            (kill-buffer inf-buffer)))))))

;;;###autoload
(defun ruby-send-region-if-used ()
  (interactive)
   (if (use-region-p)
       (call-interactively #'ruby-send-region)
     (call-interactively #'ruby-send-line)))

;;;###autoload
(defun inf-ruby-and-robe-start ()
  "For REPLs within standalone ruby scripts"
  (interactive)
  (call-interactively #'inf-ruby)
  (call-interactively #'robe-start))
