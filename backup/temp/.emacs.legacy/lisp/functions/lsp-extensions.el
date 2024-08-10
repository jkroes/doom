;; Hook functions

(defun ess-r-lsp ()
  ;; Use to disable company-box-doc but potentially keep company-box enabled
  (setq-local company-box-doc-enable nil)

  ;; For me, the company tooltip looks similar in ess-r-mode with and without
  ;; company-box. The only difference I've seen so far is in the image/icon for
  ;; modules, which are a wrench with company-box enabled.
  ;; And why does using lsp-mode with company-box-doc result in a single
  ;; unwrapped documentation line? Someone else has this same issue:
  ;; https://github.com/sebastiencs/company-box/issues/81. They disabled
  ;; company-box, and the code below can be used to do the same.
  ;; (make-local-variable 'company-mode-hook)
  ;; (remove-hook 'company-mode-hook 'company-box-mode t)

  (lsp-deferred))

;; Overriden functions

;; (defun lsp--apply-text-edits (edits &optional operation)
;;   "Override original function definition to avoid printing annoying messages
;;   to the echo area after editing a buffer."
;;   (unless (seq-empty-p edits)
;;     (atomic-change-group
;;       (run-hooks 'lsp-before-apply-edits-hook)
;;       (let* ((change-group (prepare-change-group))
;;              (howmany (length edits))
;;              (message (format "Applying %s edits to `%s' ..." howmany
;;                               (current-buffer)))
;;              ;; (_ (lsp--info message))
;;              ;; (reporter (make-progress-reporter message 0 howmany))
;;              (done 0)
;;              (apply-edit (if (not lsp--virtual-buffer)
;;                              #'lsp--apply-text-edit-replace-buffer-contents
;;                            #'lsp--apply-text-edit)))
;;         (unwind-protect
;;             (->> edits
;;                  (nreverse)
;;                  (seq-sort #'lsp--text-edit-sort-predicate)
;;                  (mapc (lambda (edit)
;;                          ;; (progress-reporter-update reporter (cl-incf done))
;;                          (funcall apply-edit edit)
;;                          (when (lsp:snippet-text-edit-insert-text-format? edit)
;;                            (-when-let ((&SnippetTextEdit :range (&RangeToPoint
;;                                                                  :start)
;;                                                          :insert-text-format?
;;                                                          :new-text) edit)
;;                              (when (eq insert-text-format?
;;                                        lsp/insert-text-format-snippet)
;;                                ;; No `save-excursion' needed since expand
;;                                ;; snippet will change point anyway
;;                                (goto-char (+ start (length new-text)))
;;                                (lsp--indent-lines start (point))
;;                                (lsp--expand-snippet new-text start (point)))))
;;                          (run-hook-with-args 'lsp-after-apply-edits-hook
;;                                              operation))))
;;           (undo-amalgamate-change-group change-group)
;;           ;; (progress-reporter-done reporter)
;;           )))))

(defun lsp--prepend-prefix (mappings)
  "Modified original function by replacing lsp-keymap-prefix with \"SPC l\"
  since that is where lsp-command-map is bound currently via general.el"
  (->> mappings
       (-partition 2)
       (-mapcat (-lambda ((key description))
                  (list (concat "SPC l" " " key)
                        description)))))
