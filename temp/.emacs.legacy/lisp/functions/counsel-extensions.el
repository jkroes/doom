(setq my/counsel-rg-large-repos (list "/opt/homebrew"))

(defun my/counsel-rg (arg)
  (interactive "P")
  (let ((counsel-rg-base-command counsel-rg-base-command)
        defdir)
    ;; Default directory is within a large git repo
    (if (not (-all-p 'null
                     (mapcar
                      (lambda (repo)
                        (string-match-p repo default-directory))
                      my/counsel-rg-large-repos)))
        (progn
          ;; counsel-ag (called by counsel-rg) sets directory to the
          ;; root of any git repo if no initial dir is specified. For
          ;; large repos, prefer searches in default-directory. This also
          ;; skips the request for a dir before rg args when C-u C-u M-x
          ;; counsel-rg.
          (setq defdir default-directory)
          ;; Some (.git)ignore files are overly strict (e.g., Homebrew,
          ;; which ignores almost everything including packages it
          ;; installs). Setting counsel-rg-base-command rather than passing
          ;; arg extra-rg-args allows us to specify additional command
          ;; line flags via C-u C-u M-x counsel-rg. In particular, see
          ;; "-l". Setting it locally preserves its global value.
          (setq counsel-rg-base-command
                `(,(car counsel-rg-base-command),
                  "--no-ignore"
                  ,@(cdr counsel-rg-base-command)))))
    (counsel-rg nil defdir)))
