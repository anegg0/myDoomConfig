;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
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

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Library/CloudStorage/ProtonDrive-gael.blanchemain@protonmail.com-folder/orgmode" )


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


;;; =========================================================================
;;; KEYBINDS
;;; =========================================================================


;; Avy line and char navigation
(map! :leader
      :desc "evil-avy-goto-line"
      "j" #'evil-avy-goto-line)

(map! :leader
      :desc "evil-avy-goto-char"
      "k" #'evil-avy-goto-char)

;; Yank from kill ring
(map! :leader
      :desc "yank-from-kill-ring"
      "Y" #'yank-from-kill-ring)

;; Treemacs project
(map! :leader
      :desc "treemacs-add-and-display-current-project-exclusively"
      "z" #'treemacs-add-and-display-current-project-exclusively)

;; Register operations
(map! :leader
      :desc "copy-to-register-1"
      "1" #'xah-copy-to-register-1)

(map! :leader
      :desc "paste-from-register-1"
      "2" #'xah-paste-from-register-1)

(map! :leader
      :desc "copy-to-register"
      "3" #'copy-to-register)

(map! :leader
      :desc "insert-register"
      "4" #'insert-register)

;; Frame and buffer navigation
(map! :leader
      :desc "switch-to-other-frame"
      "7" #'other-frame)

;; Aider integration
(map! :leader
      :desc "aidermacs-add-file"
      "8" #'aidermacs-add-file)

(map! :leader
      :desc "file-one-up"
      "9" #'file-one-up)

(map! :leader
      :desc "avy-move-line"
      "a" #'avy-move-line)

(map! :leader
      :desc "aidermacs-run"
      "0" #'aidermacs-run)

(map! :leader
      :desc "other-window"
      "]" #'other-window)


(map! :leader
      :prefix ("r" . "org-roam")
      :n "F" #'my/org-roam-node-find-by-tag)
