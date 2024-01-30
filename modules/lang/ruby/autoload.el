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
(defun inf-ruby-and-robe-start ()
  "For starting robe within standalone ruby scripts."
  (interactive)
  (call-interactively #'inf-ruby)
  (unless (modulep! +lsp)
    (call-interactively #'robe-start)))

;;;###autoload
(defun my/eir-eval-in-ruby ()
  "Based on `eir-eval-in-ruby' from the package `eval-in-repl'."
  (interactive)
  (let (begin end beg-line end-line nlines script code)
    (if (and transient-mark-mode mark-active)
        (progn
          (setq begin (region-beginning)
                end (region-end))
          ;; Fix this so that you run the portion of each line within the region.
          ;; Get the substring from either region-beginning or
          ;; line-beginning-position to either region-end or line-end-position
          (goto-char begin)
          (while (<= (point) end)
            (setq code (cons
                        (buffer-substring-no-properties (max (line-beginning-position) begin)
                                                        (min end (line-end-position)))
                        code))
            (next-line))
          (setq code (nreverse code))
          (deactivate-mark))
      (setq code (list
                  (buffer-substring-no-properties
                   (line-beginning-position)
                   (line-end-position))))
      (next-line))
    (setq code (delete "" code))
    (setq script-win (selected-window))
    (if (buffer-live-p inf-ruby-buffer)
        (select-window (get-buffer-window inf-ruby-buffer))
      ;; This will create the buffer if missing and switch to it
      (call-interactively #'inf-ruby)
      (sit-for pause-for-first-prompt))
    (while code
      (goto-char (point-max))
      (insert (pop code))
      (funcall #'comint-send-input)
      (sit-for pause-for-subsequent-prompts))
    (select-window script-win)))