;;; -*- lexical-binding: t; -*-

(use-package! annotate
 ;; Enable annotation-mode in prog-mode
 :hook (prog-mode . annotate-mode)
 :init
 ;; ~/.config/emacs/lisp/doom.el alters `user-emacs-directory'. We need to
 ;; keep the annotation file within `doom-user-dir' for version control.
 (setq annotate-file (file-name-concat doom-user-cache-dir "annotations"))
 ;; Confirm before deleting annotations
 (setq annotate-annotation-confirm-deletion t)
 ;; Disable annotate-mode in buffers where it is active, then re-enable it
 ;; after quitting ediff
 ;; (add-hook 'ediff-prepare-buffer-hook #'jkroes/ediff-disable-annotate-mode)
 ;; (add-hook! 'ediff-cleanup-hook #'jkroes/ediff-reenable-annotate-mode)
 )
