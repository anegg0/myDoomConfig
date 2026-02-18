;;; code-dumpster.el --- Dead code removed from config.el -*- lexical-binding: t; -*-
;;; Preserved here for reference. Each block has a provenance comment.

;;; =========================================================================
;;; From config.el lines 3-10: Doom template boilerplate + commented-out user identity
;;; =========================================================================

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;;; =========================================================================
;;; From config.el lines 12-30: Doom template font documentation + commented-out font spec
;;; =========================================================================

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

;;; =========================================================================
;;; From config.el lines 32-34: Doom template theme documentation
;;; =========================================================================

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

;;; =========================================================================
;;; From config.el lines 37-38: Doom template line numbers documentation
;;; =========================================================================

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.

;;; =========================================================================
;;; From config.el lines 43-73: Doom configuration tutorial block
;;; =========================================================================

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
;;; From config.el line 181: Commented-out frame transparency (replaced by default-frame-alist)
;;; =========================================================================

;; (set-frame-parameter (selected-frame) 'alpha '(95 95))

;;; =========================================================================
;;; From config.el line 188: Commented-out alternative frame dimensions
;;; =========================================================================

;; '((top . 1) (left . 1) (width . 200) (height . 140))))

;;; =========================================================================
;;; From config.el lines 366-367: Doom template org-directory documentation
;;; =========================================================================

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!

;;; =========================================================================
;;; From config.el lines 462-463: Dead documentation about removed setting
;;; =========================================================================

  ;; Let agenda inherit TODO keywords from org-todo-keywords
  ;; (Removed redundant org-agenda-todo-keywords-for-agenda setting)

;;; =========================================================================
;;; From config.el lines 783-811: Abandoned accent-menu global toggle implementation
;;; =========================================================================

  ;; Variable to control whether accent-menu-mode is enabled globally
  ;; (defvar my/accent-menu-enabled nil
  ;;   "Whether accent-menu-mode should be enabled in supported modes.")

  ;; ;; Function to toggle accent-menu-mode globally
  ;; (defun my/toggle-accent-menu-global ()
  ;;   "Toggle accent-menu-mode in all supported buffers."
  ;;   (interactive)
  ;;   (setq my/accent-menu-enabled (not my/accent-menu-enabled))
  ;;   (dolist (buffer (buffer-list))
  ;;     (with-current-buffer buffer
  ;;       (when (derived-mode-p 'text-mode 'org-mode 'message-mode
  ;;                             'markdown-mode 'gfm-mode)
  ;;         (accent-menu-mode (if my/accent-menu-enabled 1 -1)))))
  ;;   (message "Accent menu %s globally"
  ;;            (if my/accent-menu-enabled "enabled" "disabled")))

  ;; ;; Hook function to conditionally enable accent-menu-mode
  ;; (defun my/maybe-enable-accent-menu ()
  ;;   "Enable accent-menu-mode if my/accent-menu-enabled is true."
  ;;   (when my/accent-menu-enabled
  ;;     (accent-menu-mode 1)))

  ;; ;; Add hooks but don't enable by default
  ;; :hook ((text-mode . my/maybe-enable-accent-menu)
  ;;        (org-mode . my/maybe-enable-accent-menu)
  ;;        (message-mode . my/maybe-enable-accent-menu)
  ;;        (markdown-mode . my/maybe-enable-accent-menu)
  ;;        (gfm-mode . my/maybe-enable-accent-menu))

;;; =========================================================================
;;; From config.el lines 943-945: Commented-out whitespace-mode in prog-mode
;;; =========================================================================

;; Make white spaces visible in programming modes.
;; (after! prog-mode
;;   (add-hook! prog-mode #'whitespace-mode))

;;; =========================================================================
;;; From config.el lines 988-996: Commented-out copilot indentation entries
;;; =========================================================================

  ;; (add-to-list 'copilot-indentation-alist '(prog-mode 2))
  ;; (add-to-list 'copilot-indentation-alist '(org-mode 2))
  ;; (add-to-list 'copilot-indentation-alist '(text-mode 2))
  ;; (add-to-list 'copilot-indentation-alist '(markdown-mode 2))
  ;; (add-to-list 'copilot-indentation-alist '(gfm-mode 2))
  ;; (add-to-list 'copilot-indentation-alist '(rust-mode 2))
  ;; (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2))
  ;; (add-to-list 'copilot-indentation-alist '(git-commit-mode 2))
  ;; (add-to-list 'copilot-indentation-alist '(with-editor-mode 2))

;;; =========================================================================
;;; From config.el line 1114: Duplicate comment (kept one copy in config.el)
;;; =========================================================================

;; Auto-invalidate projectile cache when switching git branches: untested code!

;;; =========================================================================
;;; From config.el line 1162: Commented-out alternative API key loading
;;; =========================================================================

  ;; (linear-emacs-load-api-key-from-env)

;;; code-dumpster.el ends here
