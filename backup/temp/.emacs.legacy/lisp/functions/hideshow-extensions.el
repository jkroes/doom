;; Alternate commands (tested in ess-r-mode)

;; Unusable when outline-minor-mode is active
;; (defun hs-hide-all-comments ()
;;   (interactive)
;;   (let ((hs-hide-all-non-comment-function #'ignore)
;;         (hs-hide-comments-when-hiding-all t))
;;     (hs-hide-all)))

(defun my/hs-show-block ()
  "Undo cursor movement when calling hs-show-block."
  (interactive)
  (save-excursion
    (hs-show-block)))

(defun my/hs-hide-block ()
  "When not inside a comment, `hs-hide-block' checks if point is at the start
of a block, and if that fails whether point is within a block (i.e., searches
backwards for a block beginning via `hs-find-block-beginning'). This function
moves point to the end of line first in case a block starts there."
  (interactive)
  (save-excursion
    (end-of-line)
    (hs-hide-block)))

(defun my/hs-toggle-hiding (&optional e)
  "Correct the following behavior in the original function: Go inside a block,
toggle to hide. Cursor should be on the first ellipsis. When toggling again from
that position, `posn-set-point' temporarily moves the cursor past the
ellipses. This makes the second toggle fail to show the block as expected."
  (interactive)
  (hs-life-goes-on
   ;; (posn-set-point (event-end e))
   (if (hs-already-hidden-p)
       (my/hs-show-block)
     (my/hs-hide-block))))

;; Hook functions

(defun alternate-evil-hs-commands ()
  "Update evil-fold-list so that e.g. `evil-open-fold' located on the 'z'
  prefix dispatches the alternate hideshow command for opening folds. If
  hs-minor-mode is not enabled, the outline-mionr-mode commands for outline
  headings will be dispatched instead."
  (require 'evil) ; just in case
  (dolist (x evil-fold-list)
    (when (eq (caar x) 'hs-minor-mode)
      (setf (cdr x)
            (-> (plist-put (cdr x) ':toggle 'my/hs-toggle-hiding)
                (plist-put ':open 'my/hs-show-block)
                (plist-put ':close 'my/hs-hide-block))))))
