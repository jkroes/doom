;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;; (package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;; (package! another-package
;;   :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;; (package! this-package
;;   :recipe (:host github :repo "username/repo"
;;            :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;; (package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;; (package! builtin-package :recipe (:nonrecursive t))
;; (package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;; (package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;; (package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;; (unpin! pinned-package)
;; ...or multiple packages
;; (unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;; (unpin! t)

;;; My code and notes ---------------------------------------------------------

;; TODO Unless otherwise noted below, continuously update package
;; versions as the corresponding package! statement in Doom Emacs
;; changes.

(package! vundo)

;; Install these packages even if not using evil +everywhere
(package! evil-collection :pin "8be4b75c86bc637dbcd14be7522d6da06df1747e")
(package! evil-org
  :recipe (:host github :repo "hlissner/evil-org-mode")
  :pin "a9706da260c45b98601bcd72b1d2c0a24a017700")

(package! consult-org-roam
  :recipe (:host github :repo "jgru/consult-org-roam"))

;; Alternative to company completion module
(package! corfu
  :recipe (:host github :repo "minad/corfu"
           :files ("*.el" "extensions/*.el"))
  ;; BUG https://github.com/minad/corfu/issues/290
  ;;:pin "b5458a132c678b5fe97b4a7819b9bb1dba31aee2"
  )
;; TODO See the corfu repo for more "complementary" packages
(package! cape)
(package! kind-icon)

;; Windows issue:
;; https://github.com/org-roam/org-roam/issues/2384
(package! org :pin "ca873f7fe47546bca19")

(package! vertico-truncate
  :recipe (:host github :repo "jdtsmith/vertico-truncate"))

(package! titlecase)

;; TODO Get rid of this once doom catches up to inf-ruby commit 74c8be8e270b8.
;; This commit massively improved inf-ruby output from ruby-send-region.
(unpin! inf-ruby)

;; BUG 01/14/23 https://github.com/seagle0128/doom-modeline/issues/689
(unpin! doom-modeline)

;; BUG https://github.com/doomemacs/doomemacs/issues/7039
;; Don't download any snippets until the bug is fixed
(package! doom-snippets :ignore t)

(package! devdocs)

(package! yaml-mode)

(package! evil-tutor)

;; NOTE This demonstrates how to include a custom user package;
;; however, I have since deleted this package and moved it to
;; utility.el so that I can load it prior to any module files.
;; https://github.com/doomemacs/doomemacs/blob/master/docs/getting_started.org#usingloading-local-packages
;; Usage: (use-package! utility)
;; (package! doom-utility-functions
;;   :recipe (:host github :repo "jkroes/doom-utility-functions"
;;            :files ("lisp/*.el")
;;            :build (:not compile)))

;; From old config
;; (package! outline-minor-faces)
;; (package! backline)
;; (package! page-break-lines)
;; Auto revert seems like a bad idea and a potential source of bugs.
;; (package! autorevert :disable t)
;; (package! org-super-agenda)
;; (package! vulpea)
;; (package! fira-code-mode)


;; (package! org-transclusion)
;; (package! zotxt-emacs
;;           :recipe (:host github :repo "egh/zotxt-emacs"))




;; BUG https://github.com/doomemacs/doomemacs/issues/7078
;; (package! transient :pin "c2bdf7e12c530eb85476d3aef317eb2941ab9440")
;; (package! with-editor :pin "391e76a256aeec6b9e4cbd733088f30c677d965b")


;;(package! tempel)
;;(package! tempel-collection)
