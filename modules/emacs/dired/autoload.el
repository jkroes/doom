;;; emacs/dired/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +dired/quit-all ()
  "Kill all `dired-mode' buffers."
  (interactive)
  (mapc #'kill-buffer (doom-buffers-in-mode 'dired-mode))
  (message "Killed all dired buffers"))

;;;###autoload
(defun +dired-enable-git-info-h ()
  "Enable `dired-git-info-mode' in git repos."
  (and (not (file-remote-p default-directory))
       (locate-dominating-file "." ".git")
       (dired-git-info-mode 1)))

;;;###autoload
(defun dired-dwim-target-ranger (&optional all-frames)
  "Return directories from all next windows with dired-mode
or ranger-mode buffers."
  (mapcan (lambda (w)
            (with-current-buffer (window-buffer w)
              (when (or (eq major-mode 'dired-mode)
                        (eq major-mode 'ranger-mode))
                (list (dired-current-directory)))))
          (delq (selected-window) (window-list-1
                                   (next-window nil 'nomini all-frames)
                                   'nomini all-frames))))

