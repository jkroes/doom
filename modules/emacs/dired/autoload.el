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

(defun open-in-windows (path _)
  (let ((browse-url-generic-program "/mnt/c/Windows/System32/cmd.exe")
        (browse-url-generic-args '("/c" "start" "")))
    (browse-url-generic
     (substring
      (shell-command-to-string
       (format "wslpath -w '%s'" path))
      0 -1))))

;;;###autoload
(defun my/ranger-open-in-external-app ()
  "Open the current file or dired marked files in external app. Open WSL files
in Windows."
  (interactive)
  (let ((marked-files (dired-get-marked-files)))
    (cond ((string-equal system-type "windows-nt")
           (mapc
            (lambda (f) (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" f t t)) )
            marked-files) )
          ((string-equal system-type "darwin")
           (mapc
            (lambda (f) (shell-command (format "open \"%s\"" f)))
            marked-files))
          ;; WSL1 has "-Microsoft", WSL2 has "-microsoft-standard"
          ((string-match "-[Mm]icrosoft" operating-system-release)
           (lambda (f) (open-in-windows f nil)))
          ((string-equal system-type "gnu/linux")
           (mapc
            (lambda (f) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" f)))
            marked-files))
          (t (message "System type not supported.")))))

;;;###autoload
(defun my/ranger-disable ()
  "Interactively disable ranger-mode."
  (interactive)
  ;; don't kill ranger buffer if open somewhere else
  (if (> (length (get-buffer-window-list)) 1)
      (progn (delete-window)
             (delete-window ranger-preview-window))
    (ranger-revert)))

