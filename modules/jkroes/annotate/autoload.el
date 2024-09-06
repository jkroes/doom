;;; -*- lexical-binding: t; -*-

(defvar jkroes/was-annotated nil)

;;;###autoload
(defun jkroes/ediff-disable-annotate-mode ()
  (when (bound-and-true-p annotate-mode)
    (push (buffer-name) jkroes/was-annotated)
    (annotate-save-annotations)
    (annotate-mode -1)))

(defvar ediff-buffer-A)
(defvar ediff-buffer-B)
(defvar ediff-buffer-C)
;;;###autoload
(defun jkroes/ediff-reenable-annotate-mode ()
  (dolist (buf (list ediff-buffer-A ediff-buffer-B ediff-buffer-C))
    (when (member (buffer-name buf) jkroes/was-annotated)
      (when (boundp annotate-mode)
        (setq jkroes/was-annotated
              (delete (buffer-name buf) jkroes/was-annotated))
        (with-current-buffer buf
          (annotate-mode +1))))))
