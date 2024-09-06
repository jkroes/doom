;;; tree-sitter

;; https://www.masteringemacs.org/article/how-to-get-started-tree-sitter

;; C-h a -ts-mode$ shows available treesitter modes in Emacs
;; TODO Change mode hooks from *-mode-hook to *-ts-mode-hook
;; TODO Old indentation customizations may not work
;; TODO Font lock faces have changed in the new ts modes
;; TODO Customization options are different for the new modes
;; TODO Use ts modes with eglot and other packages. See
;; https://www.adventuresinwhy.com/post/eglot/
;; TODO C-M-f, C-M-SPC, and C-M-k will no longer work well in ts modes.
;; There is a way to remove the ts-based behavior from these commands,
;; but consider using the Combobulate package instead.

;; Language grammars
(setq treesit-language-source-alist
      '((lua "https://github.com/tree-sitter-grammars/tree-sitter-lua")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (r "https://github.com/r-lib/tree-sitter-r")
        (ruby "https://github.com/tree-sitter/tree-sitter-ruby")))

(with-eval-after-load 'treesit
  ;; Install missing language grammars
  (dolist (source treesit-language-source-alist)
    (unless (treesit-ready-p (car source) t)
      (treesit-install-language-grammar (car source))))
  ;; Use treesitter mode instead of base mode
  (add-to-list 'major-mode-remap-alist '(lua-mode . lua-ts-mode))
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
  ;; TODO If you install emacs-ess, replace this with a line that modifies
  ;; major-mode-remap-alist
  (add-to-list 'auto-mode-alist '("\\.[rR]\\'" . r-ts-mode))
  (add-to-list 'major-mode-remap-alist '(ruby-mode . ruby-ts-mode)))


