
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
;;
;; (package! emacs-dropbox
;;   :recipe (:host github :repo "lorniu/emacs-dropbox" :files ("*.el")))


;; lsp tailwindcss
(package! lsp-tailwindcss :recipe (:host github :repo "merrickluo/lsp-tailwindcss"))

;; org-roam
(package! org-roam
  :recipe (:host github :repo "org-roam/org-roam" :branch "main"))
(package! citar-org-roam)
(package! org-roam-ui)
(package! sqlite3)
(package! pinentry)
(package! gptel)
(package! evil-visual-mark-mode)
(package! solidity-mode)
(package! solidity-flycheck)
(package! company-solidity)
(package! move-text)
(package! rainbow-mode)
(package! apheleia)
(package! dired-narrow)
(package! centered-cursor-mode)
(package! treesit-auto)
(package! exec-path-from-shell)
(package! copilot
  :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el")))
(package! prettier)
(package! svg-tag-mode)
(package! nvm)
(package! aider :recipe (:host github :repo "tninja/aider.el" :files ("aider.el" "aider-core.el" "aider-file.el" "aider-code-change.el" "aider-discussion.el" "aider-prompt-mode.el" "aider-doom.el")))
(package! solaire-mode)
(package! pdf-tools)
(package! org-download)
(package! url)
(package! aidermacs)
(package! org-preview-html)
(package! undo-fu)
(package! undo-fu-session)
(package! eat)
(package! claudemacs
  :recipe (:host github :repo "cpoile/claudemacs"))
(package! spell-fu)
(package! copilot-chat)
