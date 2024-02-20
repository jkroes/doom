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
  (let (begin end beg-line end-line nlines script code eobp)
    (if (and transient-mark-mode mark-active)
        (progn
          (setq begin (region-beginning)
                end (region-end))
          ;; Fix this so that you run the portion of each line within the region.
          ;; Get the substring from either region-beginning or
          ;; line-beginning-position to either region-end or line-end-position
          (goto-char begin)
          (while (and (< (point) end) (not (eobp)))
            (setq code (cons
                        (buffer-substring-no-properties (max (line-beginning-position) begin)
                                                        (min end (line-end-position)))
                        code))
            (if (not (save-excursion (end-of-line) (eobp)))
                (next-line)
              ;; Necessary to prevent an infinite loop when the
              ;; last line in the buffer is evaluated
              (end-of-line)))
          (setq code (nreverse code))
          (deactivate-mark))
      (setq code (list
                  (buffer-substring-no-properties
                   (line-beginning-position)
                   (line-end-position))))
      ;; Note that it looks weird if we don't jump to the next
      ;; line's beginning, since we're evaluating an entire line
      (unless (save-excursion (end-of-line) (eobp)) (next-line) (beginning-of-line)))
    (setq code (delete "" code))
      ;; Note that it looks weird if we don't jump to the next
      ;; line's beginning
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
