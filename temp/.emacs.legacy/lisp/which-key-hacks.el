;;; Hacks to integrate hydras with which-key (see fork of which-key as well)

(defun my/defhydra (name)
  "Replace the docstring of each head with that of the function used to create
it, and modify the binding description to reflect the original function name,
rather than hydra's derived name for the head.

Calls to my/defhydra should follow calls to defhydra."
  (let* ((prefix-sans-slash (symbol-name name))
         (prefix (concat prefix-sans-slash "/")))
    ;; Replacements for which-key descriptions of hydra heads. wk does at most a
    ;; single replacement, unless which-key-allow-multiple-replacements is
    ;; non-nil. In lieu of setting that, you can place more targeted regexps at
    ;; the start of which-key-replacement-alist (by push-ing the less
    ;; comprehensive ones on earlier), as is this done here:

    ;; hydra-window/winner-undo -> winner-undo
    (push `((nil . ,(concat "^" prefix)) .
            (nil . ""))
          which-key-replacement-alist)

    ;; hydra-window/body -> hydra-window
    (push `((nil . ,(concat "^" prefix "body$")) .
            (nil . ,prefix-sans-slash))
          which-key-replacement-alist)

    ;; hydra-window/my/delete-other-window-and-buffers-and-exit
    ;; -> my/delete-other-windows-and-buffers
    (push `((nil . ,(concat "^" prefix "\\(.*\\)-and-exit$")) .
            (nil . "\\1"))
          which-key-replacement-alist)

    ;; hydra-window/hydra-buffer/body-and-exit -> hydra-buffer
    (push `((nil . ,(concat "^" prefix "\\(.*\\)/body-and-exit$")) .
            (nil . "\\1"))
          which-key-replacement-alist)

    ;; Retrieving/modifying docstrings in case toggled in which-key
    (dolist (h (symbol-value (intern (concat prefix "heads"))))
      (let* ((h-cmd (nth 1 h)) ;; The original command in the hydradef
             ;; hydra renames commands in several possible ways, depending on
             ;; :color
             (visible-cmd1 (intern (concat prefix (symbol-name h-cmd))))
             (visible-cmd2 (intern (concat (symbol-name visible-cmd1)
                                           "-and-exit"))))
        ;; NOTE: To retrieve the original docstring defined in the function, you
        ;; must remove the function-documentation property, which shadows it
        (put (cond ((fboundp visible-cmd1) visible-cmd1)
                   ((fboundp visible-cmd2) visible-cmd2))
             ;; Seems to work with autoloads and loaded commands. If a package
             ;; doesn't define an autoload, this will choke on the command.
             'function-documentation
             (if h-cmd
                 (documentation h-cmd)
               ;; Note: nil is only useful for blue/amaranth hydras, which don't
               ;; work with which-key paging commands currently
               (concat "Exit " (symbol-name name))))))))

;; Hide implicit hydra commands from which-key
(push '((nil . "hydra--digit-argument") . t) which-key-replacement-alist)
(push '((nil . "hydra--negative-argument") . t) which-key-replacement-alist)
(push '((nil . "hydra--universal-argument") . t) which-key-replacement-alist)
