;;; lang/ruby/config.el -*- lexical-binding: t; -*-

(after! projectile
  (add-to-list 'projectile-project-root-files "Gemfile"))


;;
;;; Packages

;; BUG https://github.com/nonsequitur/inf-ruby/issues/181#event-11493756292
;; Code that requires user input via the CLI will cause Emacs to hang if
;; sent to irb to run in an inf-ruby buffer
(use-package! ruby-mode  ; built-in
  ;; Other extensions are already registered in `auto-mode-alist' by `ruby-mode'
  :mode "\\.\\(?:a?rb\\|aslsx\\)\\'"
  :mode "/\\(?:Brew\\|Fast\\)file\\'"
  :interpreter "j?ruby\\(?:[0-9.]+\\)"
  :config
  (setq ruby-insert-encoding-magic-comment nil)

  ;; NOTE If :slot and :vsolt are missing or nil, they are eventually assigned
  ;; a value of 0. To get your popups to stack windows, set a nonzero value for
  ;; :vslot and/or :slot. Note that other popup windows with the same set of
  ;; values can still replace your popup window. E.g., pressing "K" opens up an
  ;; lsp help or *eglot-help* buffer that by default replaces the ruby REPL,
  ;; because neither specifies a :vslot or :slot value. The current behavior of
  ;; the popup module is to quit a window when the buffer is killed rather than
  ;; open a previous window. There is a hack in my private module to fix this.
  (set-popup-rule! "^\\*ruby\\*$" :size 0.3 :select nil :quit nil :ttl nil)

  (set-electric! 'ruby-mode :words '("else" "end" "elsif"))
  (set-repl-handler! 'ruby-mode #'inf-ruby)

  (when (modulep! +lsp)
    (add-hook 'ruby-mode-local-vars-hook #'lsp! 'append))

  (when (modulep! +tree-sitter)
    (add-hook 'ruby-mode-local-vars-hook #'tree-sitter! 'append))

  (after! inf-ruby
    (add-hook 'inf-ruby-mode-hook #'doom-mark-buffer-as-real-h)
    ;; switch to inf-ruby from compile if we detect a breakpoint has been hit
    (add-hook 'compilation-filter-hook #'inf-ruby-auto-enter)
    ;; HACK This is what makes things hang when we are expecting user input.
    ;; Disabling it also disables printing the results of evaluation in the
    ;; script.
    (fset 'ruby-print-result #'ignore))


  ;; so class and module pairs work
  (setq-hook! 'ruby-mode-hook sp-max-pair-length 6)

  ;; launchschool recommendation
  (setq-hook! 'ruby-mode-hook tab-width 2)

  ;; NOTE Sending regions seems to be the only reliable function for working
  ;; with inf-ruby. It may be better to spawn irb or pry within a terminal and
  ;; work with that.
  (map! :map ruby-mode-map
        :gin "C-RET" #'ruby-send-region-if-used
        :gin "C-<return>" #'ruby-send-region-if-used)

  (map! :localleader
        :map ruby-mode-map
        "[" #'ruby-toggle-block
        "{" #'ruby-toggle-block))

;; BUG Enabling the +lsp flag disables robe-mode, which is 100% unusable.
(use-package! robe
  :defer t
  :init
  (add-hook! 'ruby-mode-hook
    (defun +ruby-init-robe-mode-maybe-h ()
      "Start `robe-mode' if `lsp-mode' isn't active."
      (or (bound-and-true-p lsp-mode)
          (bound-and-true-p lsp--buffer-deferred)
          (robe-mode +1))))
  :config
  (set-repl-handler! 'ruby-mode #'robe-start)
  (set-company-backend! 'ruby-mode 'company-robe 'company-dabbrev-code)
  (set-lookup-handlers! 'ruby-mode
    :definition #'robe-jump
    :documentation #'robe-doc)
  (when (boundp 'read-process-output-max)
    ;; Robe can over saturate IPC, making interacting with it slow/clobbering
    ;; the GC, so increase the amount of data Emacs reads from it at a time.
    (setq-hook! '(robe-mode-hook inf-ruby-mode-hook)
      read-process-output-max (* 1024 1024)))
  (when (modulep! :editor evil)
    (add-hook 'robe-mode-hook #'evil-normalize-keymaps))
  (map! :localleader
        :map robe-mode-map
        ;; Before I switched to lsp-mode, I was using inf-ruby-and-robe-start
        ;; here; however, robe-mode was 100% unusable.
        "'"  #'inf-ruby
        ;; NOTE For now, adding a Gemfile via bundle init, then adding pry and
        ;; pry-doc to the list of gems, has mostly
        "h"  #'robe-doc
        "R"  #'robe-rails-refresh
        :prefix "s"
        "d"  #'ruby-send-definition
        "D"  #'ruby-send-definition-and-go
        "r"  #'ruby-send-region
        "R"  #'ruby-send-region-and-go
        "i"  #'ruby-switch-to-inf))


;; NOTE Must be loaded before `robe-mode'
(use-package! yard-mode
  :hook ruby-mode)


(use-package! rubocop
  :hook (ruby-mode . rubocop-mode)
  :config
  (set-popup-rule! "^\\*RuboCop" :select t)
  (map! :localleader
        :map rubocop-mode-map
        "f" #'rubocop-check-current-file
        "F" #'rubocop-autocorrect-current-file
        "p" #'rubocop-check-project
        "P" #'rubocop-autocorrect-project))


;;
;;; Package & Ruby version management

(use-package! rake
  :defer t
  :init
  (setq rake-cache-file (concat doom-cache-dir "rake.cache"))
  (setq rake-completion-system 'default)
  (map! :after ruby-mode
        :localleader
        :map ruby-mode-map
        :prefix ("k" . "rake")
        "k" #'rake
        "r" #'rake-rerun
        "R" #'rake-regenerate-cache
        "f" #'rake-find-task))

(use-package! bundler
  :defer t
  :init
  (map! :after ruby-mode
        :localleader
        :map ruby-mode-map
        :prefix ("b" . "bundle")
        "c" #'bundle-check
        "C" #'bundle-console
        "i" #'bundle-install
        "u" #'bundle-update
        "e" #'bundle-exec
        "o" #'bundle-open))

(use-package! chruby
  :when (modulep! +chruby)
  :hook (ruby-mode . chruby-use-corresponding)
  :config
  (setq rspec-use-rvm nil
        rspec-use-chruby t))

(after! rbenv
  (setq rspec-use-rvm nil)
  (add-to-list 'exec-path (expand-file-name "shims" rbenv-installation-dir)))


;;
;;; Testing frameworks

(use-package! rspec-mode
  :mode ("/\\.rspec\\'" . text-mode)
  :init
  (setq rspec-use-spring-when-possible nil)
  (when (modulep! :editor evil)
    (add-hook 'rspec-mode-hook #'evil-normalize-keymaps))
  :config
  (set-popup-rule! "^\\*\\(rspec-\\)?compilation" :size 0.3 :ttl nil :select t)
  (setq rspec-use-rvm (executable-find "rvm"))
  (map! :localleader
        :prefix "t"
        :map (rspec-verifiable-mode-map rspec-dired-mode-map rspec-mode-map)
        "a" #'rspec-verify-all
        "r" #'rspec-rerun
        :map (rspec-verifiable-mode-map rspec-mode-map)
        "v" #'rspec-verify
        "c" #'rspec-verify-continue
        "l" #'rspec-run-last-failed
        "T" #'rspec-toggle-spec-and-target
        "t" #'rspec-toggle-spec-and-target-find-example
        :map rspec-verifiable-mode-map
        "f" #'rspec-verify-method
        "m" #'rspec-verify-matching
        :map rspec-mode-map
        "s" #'rspec-verify-single
        "e" #'rspec-toggle-example-pendingness
        :map rspec-dired-mode-map
        "v" #'rspec-dired-verify
        "s" #'rspec-dired-verify-single))


(use-package! minitest
  :defer t
  :config
  (when (modulep! :editor evil)
    (add-hook 'minitest-mode-hook #'evil-normalize-keymaps))
  (map! :localleader
        :map minitest-mode-map
        :prefix "t"
        "r" #'minitest-rerun
        "a" #'minitest-verify-all
        "s" #'minitest-verify-single
        "v" #'minitest-verify))


(use-package! projectile-rails
  :when (modulep! +rails)
  :hook ((ruby-mode inf-ruby-mode projectile-rails-server-mode) . projectile-rails-mode)
  :hook (projectile-rails-server-mode . doom-mark-buffer-as-real-h)
  :hook (projectile-rails-mode . auto-insert-mode)
  :init
  (setq auto-insert-query nil)
  (setq inf-ruby-console-environment "development")
  (when (modulep! :lang web)
    (add-hook 'web-mode-hook #'projectile-rails-mode))
  :config
  (set-popup-rule! "^\\*\\(projectile-\\)?rails" :ttl nil)
  (when (modulep! :editor evil)
    (add-hook 'projectile-rails-mode-hook #'evil-normalize-keymaps))
  (map! :localleader
        :map projectile-rails-mode-map
        "r" #'projectile-rails-command-map))
