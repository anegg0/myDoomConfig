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
      :desc "dired-jump"
      "9" #'dired-jump)

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



;;; =========================================================================
;;; ORGMODE
;;; =========================================================================

(require 'org)
(after! org
  ;; Base settings
  (setq
   load-prefer-newer t
   search-highlight t
   search-whitespace-regexp ".*?"
   org-ellipsis " ▼ "
   org-adapt-indentation nil
   org-habit-show-habits-only-for-today t)

  (setq org-capture-templates
        `(("i" "Inbox" entry  (file "gtd/inbox.org")
           ,(concat "* TODO %?\n"
                    "/Entered on/ %U"))
          ("s" "Slipbox" entry  (file "braindump/org/inbox.org")
           "* %?\n")))
  (defun jethro/org-capture-inbox ()
    (interactive)
    (org-capture nil "i"))

  (defun jethro/org-capture-slipbox ()
    (interactive)
    (org-capture nil "s"))

  ;; Configure org-mode for inline images
  (setq org-startup-with-inline-images t)  ; Show inline images when opening org files
  (setq org-image-actual-width nil)        ; Use image size specifications in org files
  (add-hook 'org-mode-hook #'org-display-inline-images)

  ;; Custom TODO keywords
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(i)" "IN-REVIEW(r)" "|" "BACKLOG(b)" "BLOCKED(l)" "DONE(d)" "CANCELED(c)" "DUPLICATE(p)")))

  ;; Optional: Add custom faces for your TODO states
  (setq org-todo-keyword-faces
        '(("TODO" . (:foreground "red" :weight bold))
          ("IN-PROGRESS" . (:foreground "blue" :weight bold))
          ("IN-REVIEW" . (:foreground "orange" :weight bold))
          ("BACKLOG" . (:foreground "orange" :weight bold))
          ("BLOCKED" . (:foreground "green" :weight bold))
          ("DONE" . (:foreground "green" :weight bold))
          ("CANCELLED" . (:foreground "gray" :weight bold))
          ("DUPLICATE" . (:foreground "black" :weight bold))))

  ;; Define org-agenda-files to include the right directories
  ;; Include all org files from main directories for agenda
  (setq org-agenda-files (list
                          (expand-file-name "main" org-directory)
                          (expand-file-name "daily" org-directory)
                          (expand-file-name "reference" org-directory)
                          (expand-file-name "articles" org-directory)))

  ;; Enable refile targets to include agenda files
  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))

  )

(use-package! org-roam
  ;;ADDED
  :init
  (map! :leader
        :prefix "r"
        :desc "org-roam" "l" #'org-roam-buffer-toggle
        :desc "org-roam-node-insert" "i" #'org-roam-node-insert
        :desc "org-roam-node-find" "f" #'org-roam-node-find
        :desc "org-roam-ref-find" "r" #'org-roam-ref-find
        :desc "org-roam-show-graph" "g" #'org-roam-show-graph
        :desc "org-roam-dailies-capture-today" "T" #'org-roam-dailies-capture-today
        :desc "org-roam-dailies-goto-today" "t" #'org-roam-dailies-goto-today
        :desc "jethro/org-capture-slipbox" "<tab>" #'jethro/org-capture-slipbox
        :desc "org-roam-capture" "c" #'org-roam-capture)
  (setq org-roam-directory (file-truename "~/Library/CloudStorage/ProtonDrive-gael.blanchemain@protonmail.com-folder/orgmode/")
        org-roam-database-connector 'sqlite-builtin
        org-roam-db-gc-threshold most-positive-fixnum
        org-id-link-to-org-use-id t)
  :config
  (org-roam-db-autosync-mode +1)
  (set-popup-rules!
    `((,(regexp-quote org-roam-buffer) ; persistent org-roam buffer
       :side right :width .33 :height .5 :ttl nil :modeline nil :quit nil :slot 1)
      ("^\\*org-roam: " ; node dedicated org-roam buffer
       :side right :width .33 :height .5 :ttl nil :modeline nil :quit nil :slot 2)))
  (add-hook 'org-roam-mode-hook #'turn-on-visual-line-mode)
  (setq org-roam-capture-templates
        '(
          ("m" "main" plain
           "%?"
           :if-new (file+head "main/${slug}.org"
                              "#+title: ${title}\n#+TAGS: :\n#+FILETAGS: :\n")
           :immediate-finish t
           :unnarrowed t)
          ("q" "quote" plain
           "%?"
           :if-new (file+head "main/${slug}.org"
                              "#+title: ${title}\n#+TAGS: :\n#+FILETAGS: :quote:\n")
           :immediate-finish t
           :unnarrowed t)

          ("c" "catb" plain
           "%?"
           :if-new (file+head "main/gb_b_catb_${slug}.org"
                              "#+title: ${title}\n#+TAGS: :\n#+FILETAGS: :catb:\n")
           :immediate-finish t
           :unnarrowed t)
          ("s" "Slipbox" entry  (file "/braindump/org/inbox.org")
           "* %?\n")
          ("r" "reference" plain "%?"
           :if-new
           (file+head "reference/${slug}.org" "#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
          ("a" "article" plain "%?"
           :if-new
           (file+head "articles/${slug}.org" "#+title: ${title}\n#+filetags: :article:\n")
           :immediate-finish t
           :unnarrowed t)
          ("g" "dictionary" plain "%?"
           :if-new
           (file+head "glossary/${slug}.org" "#+title: ${title}\n#+filetags: :glossary:\n")
           :immediate-finish t
           :unnarrowed t)))
  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
        (file-name-nondirectory
         (directory-file-name
          (file-name-directory
           (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (error "")))
  (setq org-roam-node-display-template
        (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag))))



;;; =========================================================================
;;; EDITOR
;;; =========================================================================
;; Disable dired-omit-mode globally
(remove-hook 'dired-mode-hook 'dired-omit-mode)


;; Emacs Everywhere Configuration
(after! emacs-everywhere
  ;; Set the default major mode to markdown-mode
  (setq emacs-everywhere-major-mode-function 'markdown-mode)

  ;; Optionally add hooks for specific adjustments when Emacs Everywhere activates
  (add-hook 'emacs-everywhere-init-hooks
            (lambda ()
              ;; Enable visual-line-mode for better text wrapping
              (visual-line-mode)
              ;; Disable line numbers for cleaner interface
              (display-line-numbers-mode -1)
              ;; Optionally center the buffer contents
              (centered-cursor-mode)
              ;; Optionally enable copilot-mode
              (copilot-mode))))


;; Markdown/MDX file configuration
(after! markdown-mode
  ;; Ensure that `mdx` files are open in `gfm-mode` in doom emacs
  (add-to-list 'auto-mode-alist '("\\.mdx\\'" . gfm-mode))

  ;; Add a hook to force gfm-mode for mdx files if needed
  (add-hook 'find-file-hook
            (lambda ()
              (when (and buffer-file-name
                         (string-match-p "\\.mdx\\'" buffer-file-name)
                         (not (eq major-mode 'gfm-mode)))
                (gfm-mode)))))



;;; =========================================================================
;;; PROJECTILE
;;; =========================================================================
;; Auto-invalidate projectile cache when switching git branches: untested code!
(after! magit
  (advice-add 'magit-checkout :after
              (lambda (&rest _)
                (let ((project-root (projectile-project-root)))
                  (when project-root
                    (message "Invalidating projectile cache for %s" project-root)
                    (projectile-invalidate-cache nil))))))
