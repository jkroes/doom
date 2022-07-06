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

;; If you are getting prompts about bytes that don't work with the coding system
;; and to save as a new encoding, use the following to find the bad characters.
(defun find-next-unsafe-char (&optional coding-system)
  "Find the next character in the buffer that cannot be encoded by
coding-system. If coding-system is unspecified, default to the coding
system that would be used to save this buffer. With prefix argument,
prompt the user for a coding system."
  (interactive "Zcoding-system: ")
  (if (stringp coding-system) (setq coding-system (intern coding-system)))
  (if coding-system nil
    (setq coding-system
          (or save-buffer-coding-system buffer-file-coding-system)))
  (let ((found nil) (char nil) (csets nil) (safe nil))
    (setq safe (coding-system-get coding-system 'safe-chars))
    ;; some systems merely specify the charsets as ones they can encode:
    (setq csets (coding-system-get coding-system 'safe-charsets))
    (save-excursion
      ;;(message "zoom to <")
      (let ((end  (point-max))
            (here (point    ))
            (char  nil))
        (while (and (< here end) (not found))
          (setq char (char-after here))
          (if (or (eq safe t)
                  (< char ?\177)
                  (and safe  (aref safe char))
                  (and csets (memq (char-charset char) csets)))
              nil ;; safe char, noop
            (setq found (cons here char)))
          (setq here (1+ here))) ))
    (and found (goto-char (1+ (car found))))
    found))

(autoload 'doom--help-modules-list (concat-path doom-emacs-dir "core/autoload/help.el"))
(defun doom/copy-module-to-private (category module)
  "Copy the module corresponding to the strings category and module as a private
module."
  (interactive
   (nconc
    (mapcar #'intern
            (split-string
             (completing-read "Copy module: "
                              (doom--help-modules-list)
                              nil t nil nil
                              (doom--help-current-module-str))
             " " t))))
  (let* ((path (doom-module-locate-path category module))
         (newpath (replace-regexp-in-string doom-modules-dir (car doom-modules-dirs) path)))
    (copy-directory path newpath nil t t)))

;; Compare private and non-private module file to see your modifications and
;; any changes Doom has made if you've upgraded it recently
(autoload #'ediff-read-file-name "ediff")
(defun doom/ediff ()
  "Run Ediff on a private Doom module file and its non-private counterpart"
  (interactive)
  (let* ((file-A
          (ediff-read-file-name
	   "Private module file"
           (if (string-prefix-p (car doom-modules-dirs) default-directory)
               default-directory
             (car doom-modules-dirs))
           (ediff-get-default-file-name)
           'no-dirs))
         (file-B (replace-regexp-in-string (car doom-modules-dirs) doom-modules-dir file-A)))
    (ediff-files-internal file-A file-B nil nil 'ediff-files)))
;; TODO Delete this after migrating .doom.d.bck modules
(defun doom/ediff2 ()
  (interactive)
  (let* ((file-A
          (ediff-read-file-name
	   "Private module file"
           (if (string-prefix-p (car doom-modules-dirs) default-directory)
               default-directory
             (car doom-modules-dirs))
           (ediff-get-default-file-name)
           'no-dirs))
         (file-B (replace-regexp-in-string (car doom-modules-dirs) "/Users/jkroes/.doom.d.bck/modules/" file-A)))
    (ediff-files-internal file-A file-B nil nil 'ediff-files)))

(setq mac-pass-command-to-system nil ; So C-h doesn't hide Emacs
      mac-pass-control-to-system nil
      ns-command-modifier 'control ; For Kinesis Advantage 2 keyboard
      mac-command-modifier 'control
      ns-right-option-modifier 'left
      mac-right-option-modifier 'left)

;; NOTE This unbinds +popup/toggle
;; In line with MacOS shortcut and HYPER-SPC when hammerspoon is active
(map! "C-`" #'other-frame)

;; As noted in
;; https://github.com/noctuid/evil-guide#why-dont-keys-defined-with-evil-define-key-work-immediately,
;; keymap normalization may be required in some cases. One seems to be use of
;; edebug-mode-map as an evil-intercept map. Without normalization, if in normal
;; mode SPC will trigger leader until you first switch to another evil state.
(add-hook 'edebug-mode-hook #'evil-normalize-keymaps)
(add-hook 'edebug-mode-hook (defun emacs-state-for-edebug ()
                              (if edebug-mode (evil-emacs-state)
                                (evil-exit-emacs-state))))

(setq confirm-kill-emacs nil)

(after! evil
  (define-key evil-motion-state-map "go" #'consult-outline))

;; Disable smartparens
(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)
