(defun server-shutdown ()
  "Modified version of kill-emacs that prompts the user to save unsaved
buffers. This is needed because kill-emacs does not prompt to save, while C-c
C-c does not kill the server."
  (interactive)
  (save-some-buffers)
  ;; (server-force-delete) ; For unknown reasons, server doesn't always stop
  (kill-emacs))
