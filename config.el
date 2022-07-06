;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!
(setq doom-font (font-spec :family "Hack" :size (cond (IS-MAC 14) (IS-LINUX 12))))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;;; My notes

;;;; emacs-mac server issues
;; https://gist.github.com/railwaycat/4043945

;;;; Troubleshooting straight
;; https://github.com/radian-software/straight.el/issues/292
;; https://github.com/radian-software/straight.el/issues/540

;;;; Working with Doom
;; After modifying modules or init.el, run doom sync
;; After modifying packages.el--speifically recipes for existing packages--run
;; doom sync -u. You need to run this the first time Doom is installed
;; To upgrade Doom itself, run doom upgrade
;; After an upgrade, you will have to run pdf-tools-install
;; Messing with modules may break lazy loading and lead to unexpected results when
;; trying to defer a package. To see what loads and when, enable force-load-messages
;; in early-init.el
;; Install/sync/upgrade fail on work computers, it might be because recipes
;; clone from gitlab, and gitlab is blocked. Clone the package manually (from
;; github potentially) while inside ~/.emacs.d/.local/straight/repos, then try
;; again. Try also disconnecting from the VPN. Alternatively, a package may have
;; renamed its branch from master to main. Delete the repo at issue and try again.
;; Before and after install/sync/upgrade, run doom sync

;;; My code

;; When Emacs freezes and won't respond to C-g, open a terminal and run ~pkill
;; -SIGTERM Emacs~ as many times as needed to kill Emacs. Then check
;; ~doom-cache-dir~ for a file named backtrace. Ignore the lines from the top to
;; the anonymous function defined below.
(defun concat-path (&rest parts)
  "Concatenate unlimited path components"
  (cl-reduce (lambda (a b) (expand-file-name b a)) parts))

(defun save-backtrace ()
  (require 'backtrace)
  (with-temp-file (concat-path doom-cache-dir "backtrace")
    ;; Pulled from backtrace function
    (insert (backtrace-to-string
             (backtrace-get-frames 'backtrace)))))
(add-hook 'kill-emacs-hook 'save-backtrace)
