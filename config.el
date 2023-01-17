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
(setq doom-font (font-spec :family "Hack" :size 14))

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

;; -----------------------------------------------------------------------------

;; Notes on working with Doom:
;;
;; As with many software projects, the documentation is a mix of outdated and
;; never accurate to begin with. The following is a best guess, given this:
;;
;; doom h[elp]: Show help for doom cli.
;;
;; doom help sync: Show help for doom sync.
;;
;; doom sync: Run after modifying init.el (your `doom!' block); packages.el
;; (`package!' blocks); autoload.el, or any file within an autoload/ folder,
;; within a private module. I suspect this is related to `doom compile', which
;; compiles part of your config (init.el and packages.el) and modules. Changes
;; to all but config.el are not recognized automatically because those changes
;; need to be re-compiled.
;;
;; doom sync -u: Also update emacs package versions. This needs to run the
;; first time Doom is installed.
;;
;; Note that not all changes to packages.el are changes to package versions,
;; which explains why packages.el is mentioned in both `doom sync' and
;; `doom sync -u'.
;;
;; doom upgrade: Upgrade doom emacs and, like `doom sync -u', its packages.
;; After upgrading, you might also have to run `pdf-tools-install'.
;;
;; Editing modules may break lazy loading and lead to unexpected results when
;; trying to defer a package. To see what loads and when, enable
;; force-load-messages in early-init.el
;;
;; doom install/sync/upgrade might fail on work computers, possibly because
;; recipes clone from gitlab, and gitlab is blocked. Clone the package manually
;; (from github potentially) while inside ~/.emacs.d/.local/straight/repos, then
;; try again. Try also disconnecting from the VPN. Alternatively, a package may
;; have renamed its branch from master to main. Delete the repo at issue and try
;; again.

(setq ns-command-modifier 'control ; For Kinesis Advantage 2 keyboard
      mac-command-modifier 'control
      confirm-kill-emacs nil)
