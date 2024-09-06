;;; config/default/+evil.el -*- lexical-binding: t; -*-

;; Replace a region you have selected (by mouse?) while in insert state simply
;; by typing
(defun +default-disable-delete-selection-mode-h ()
  (delete-selection-mode -1))
(add-hook 'evil-insert-state-entry-hook #'delete-selection-mode)
(add-hook 'evil-insert-state-exit-hook  #'+default-disable-delete-selection-mode-h)


;;
;;; Keybindings

;; This section is dedicated to "fixing" certain keys so that they behave
;; sensibly (and consistently with similar contexts).


(map! :map universal-argument-map
      ;; Make M-u M-u [...] possible
      "M-u" #'universal-argument-more
      ;; Without this, we can't do e.g. SPC-u <N> SPC to insert N spaces. With
      ;; this, we can do SPC-u <N> SPC SPC. (Since we're in insert state
      ;; anyway, I would probably use M-u instead of SPC-u.)
      :prefix doom-leader-key "SPC" #'self-insert-command
      ;; Make SPC u SPC u [...] possible (#747)
      :prefix doom-leader-key     "u" #'universal-argument-more
      :prefix doom-leader-alt-key "u" #'universal-argument-more)


(when (modulep! +bindings)
  (load! "+evil-bindings"))
