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
(setq doom-font (font-spec :family "Fira Code" :size 11));
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
(setq org-directory "~/Library/CloudStorage/Dropbox/orgmode")

(setq user-full-name "Gael Blanchemain"
      user-mail-address "gael.blanchemain@protonmail.com"
      load-prefer-newer t
      search-highlight t
      search-whitespace-regexp ".*?"
      org-ellipsis " ▼ "
      org-adapt-indentation nil
      org-habit-show-habits-only-for-today t)


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
;; avy lines

;; (defun frame-opacity (frame)
;;   (set-frame-parameter frame 'alpha-background 90))
;; (frame-opacity nil)
;; (add-to-list 'after-make-frame-functions 'frame-opacity)
;; set transparency
;;
(defun my-weebery-is-always-greater ()
  (let* ((banner '("                                                                                "
                   "                                       .=%@@*:                                  "
                   "                                      -%@@@@@%-.                                "
                   "                                      %@@@@@@@#:                                "
                   "                                   .:=@@@@@@@@#:.                               "
                   "                                .:+%@@@@@@@@@@@@%=..                            "
                   "                               .-%@@@@@@@@@@@@@@@@#:.                           "
                   "                               :*@@@@@@@@@@@@@@@@@@*.                           "
                   "                               :#@@@@@@@@@@@@@@@@@@%.                           "
                   "                              .:#@@@@@@@@@@@@@@@@@@%.                           "
                   "                             .-#@@@@@@@@@@@@@@@@@@@@#-                          "
                   "                              .-%@@@@@@@@@@@@@@@@@@%:                           "
                   "                               .=@@@@@@@@@@@@@@@@@@:.                           "
                   "                                -%@@@@@@@@@@@@@@@@+.                            "
                   "                                .+@@@@@@@@@@@@@@@#:.                            "
                   "                                 :#@@@@@@@@@@@@@@-.                             "
                   "                                 .*@@@@@@@@@@@@@*:                              "
                   "                                 .+*%@@@@@@@@@@@+.                              "
                   "                                 :#+*@@@@@@@@@#*+.                              "
                   "                                .::-#@@@@@@@@@#-=:.                             "
                   "                             ...-+%@@@@@@@@@@@@%*-..                            "
                   "                       ..+#%@@@@@@@@@@@@@@@@@@@@@@@@@%*=:..                     "
                   "                      -%@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@%=.                   "
                   "                     -%@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@*.                  "
                   "                    .*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@%-                  "
                   "                    :#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@=                  "
                   "                    -%@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@=                  "
                   "                    =@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@+.                 "
                   "                   .+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@*.                 "
                   "                   .*@@@@@@@@#@@@@@@@@@@@@@@@@@@@@@%:@@@@@@@@*:                 "
                   "                   :#@@@@@@@=.+@@@@@@@@@@@@@@@@@@@@:.*@@@@@@@#:                 "
                   "                   -%@@@@@@@..:#@@@@@@@@@@@@@@@@@@=..-@@@@@@@%-                 "
                   "                  .=@@@@@@@#..:*@@@@@@@@@@@@@@@@@%:...%@@@@@@%-                 "
                   "                  .+@@@@@@@+ .:*@@@@@@@@@@@@@@@@@%:...+@@@@@@@=.                "
                   "                 .:#@@@@@@@: .-@@@@@@@@@@@@@@@@@@@-...:@@@@@@@=.                "
                   "                ..*@@@@@@@+..:*@@@@@@@@@@@@@@@@@@@*...+@@@@@@@+.                "
                   "                .+@@@@@@@*...*@@@@@@@@@@@@@@@@@@@@@=:*@@@@@@@%-.                "
                   "               .-@@@@@@@*..-@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@=.                 "
                   "              .-@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@*:...               "
                   "          ....=@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@+-...            "
                   "    ....:-+%@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@+-:...       "
                   " ...-*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@%+:...   "
                   " .=@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#=.. "
                   " =@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@%-."
                   ".@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@-"
                   ".@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#"
                   " +@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@+"
                   " .*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#."
                   " ..+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#:."
                   " .+%@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#:.."
                   " *@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@%-."
                   " +############################################################################+."))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat line (make-string (max 0 (- longest-line (length line))) 32)))
               "\n"))
     'face 'doom-dashboard-banner)))

(setq +doom-dashboard-ascii-banner-fn #'my-weebery-is-always-greater)
;;
;;
(set-frame-parameter (selected-frame) 'alpha '(95 95))
(add-to-list 'default-frame-alist '(alpha 95 95))

(map! :leader
      :desc "evil-avy-goto-line"
      "j" #'evil-avy-goto-line)

;; avy char
(map! :leader
      :desc "evil-avy-goto-char"
      "k" #'evil-avy-goto-char)

;; invoke yank-from-kill-ring with spc-y
(map! :leader
      :desc "yank-from-kill-ring"
      "y" #'yank-from-kill-ring)

;; Sync tremacs with current project
(map! :leader
      :desc "treemacs-add-and-display-current-project-exclusively"
      "z" #'treemacs-add-and-display-current-project-exclusively)

;; copy to register 1
(map! :leader
      :desc "copy-to-register-1"
      "1" #'xah-copy-to-register-1)

;; paste from register 1
(map! :leader
      :desc "paste-from-register-1"
      "2" #'xah-paste-from-register-1)

;; copy-to-register
(map! :leader
      :desc "copy-to-register"
      "3" #'copy-to-register)

;; insert-register
(map! :leader
      :desc "insert-register"
      "4" #'insert-register)

;; Display notmuch-hello
(map! :leader
      :desc "switch-to-other-frame"
      "7" #'switch-to-buffer-other-frame)
;; add-to-aider macro
(map! :leader
      :desc "add-to-aider"
      "8" #'add-to-aider)

;; Display notmuch-hello
(map! :leader
      :desc "file-one-up"
      "9" #'file-one-up)

;; Display notmuch-hello
(map! :leader
      :desc "avy-move-line"
      "a" #'avy-move-line)

;; Opeon an Aider session
(map! :leader
      :desc "aider-run-aider"
      "0" #'aider-run-aider)

;; better markdown hightlighting
(custom-set-faces!
  '(markdown-header-delimiter-face :foreground "#616161" :height 0.9)
  '(markdown-header-face-1 :height 1.8 :foreground "#A3BE8C" :weight extra-bold :inherit markdown-header-face)
  '(markdown-header-face-2 :height 1.4 :foreground "#EBCB8B" :weight extra-bold :inherit markdown-header-face)
  '(markdown-header-face-3 :height 1.2 :foreground "#D08770" :weight extra-bold :inherit markdown-header-face)
  '(markdown-header-face-4 :height 1.15 :foreground "#BF616A" :weight bold :inherit markdown-header-face)
  '(markdown-header-face-5 :height 1.1 :foreground "#b48ead" :weight bold :inherit markdown-header-face)
  '(markdown-header-face-6 :height 1.05 :foreground "#5e81ac" :weight semi-bold :inherit markdown-header-face))
(add-hook! (gfm-mode markdown-mode) #'visual-line-mode #'turn-off-auto-fill)
(global-visual-line-mode +1)

;; treesit-auto
(global-tree-sitter-mode)

;; Org roam
(use-package! org-roam
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
  (setq org-roam-directory (file-truename "~/Library/CloudStorage/Dropbox/orgmode")
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
                              "#+title: ${title}\n#+TAGS: :\n")
           :immediate-finish t
           :unnarrowed t)
          ("c" "catb" plain
           "%?"
           :if-new (file+head "main/gb_b_catb_${slug}.org"
                              "#+title: ${title}\n#+TAGS: :\n")
           :immediate-finish t
           :unnarrowed t)
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
          ;; ("l" "linear_issue" plain "%?"
          ;;  :if-new
          ;;  (file+head "linear/${slug}.org" "#+title: ${title}\n")
          ;;  :immediate-finish t
          ;;  :unnarrowed t)
          ("d" "dictionary" plain "%?"
           :if-new
           (file+head "dictionary/${slug}.org" "#+title: ${title}\n#+filetags: :dictionary:\n")
           :immediate-finish t
           :unnarrowed t)))
  ;; (defun jethro/tag-new-node-as-draft ()
  ;;   (org-roam-tag-add '("draft")))
  ;; (add-hook 'org-roam-capture-new-node-hook #'jethro/tag-new-node-as-draft)
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
(require 'citar)
(use-package citar
  :no-require
  :custom
  (org-cite-global-bibliography '("~/Library/CloudStorage/Dropbox/orgmode/master.json"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  ;; optional: org-cite-insert is also bound to C-c C-x C-@
  :bind
  (:map org-mode-map :package org ("C-c b" . #'org-cite-insert)))
(defun jethro/org-roam-node-from-cite (keys-entries)
  (interactive (list (citar-select-ref :multiple nil :rebuild-cache t)))
  (let ((title (citar--format-entry-no-widths (cdr keys-entries)
                                              "${author editor} :: ${title}")))
    (org-roam-capture- :templates
                       '(("r" "reference" plain "%?" :if-new
                          (file+head "reference/${citekey}.org"
                                     ":PROPERTIES:
                                     :ROAM_REFS: [cite:@${citekey}]
                                     :END:
                                     #+title: ${title}\n")
                          :immediate-finish t
                          :unnarrowed t))
                       :info (list :citekey (car keys-entries))
                       :node (org-roam-node-create :title title)
                       :props '(:finalize find-file))))
(setq org-roam-dailies-directory "daily/")
;; Org-roam-tag-find
(defun my/org-roam-node-has-tag (node tag)
  "Filter function to check if the given NODE has the specified TAG."
  (member tag (org-roam-node-tags node)))

(setq org-todo-keywords                 ;
      '((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "|" "DONE(d)" "|" "Cancelled(c)")))


(require 'find-lisp)
(defun jethro/org-capture-inbox ()
  (interactive)
  (org-capture nil "i"))

(defun jethro/org-capture-slipbox ()
  (interactive)
  (org-capture nil "s"))

(defun my/org-roam-node-find-by-tag ()
  "Find and open an Org-roam node based on a specified tag."
  (interactive)
  (let ((tag (read-string "Enter tag: ")))
    (org-roam-node-find nil nil (lambda (node) (my/org-roam-node-has-tag node tag)))))
(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %?"
         :target (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n"))))
(use-package deft
  :commands (deft)
  :config
  (setq deft-directory org-directory))
(setq deft-recursive t)
(setq deft-strip-summary-regexp
      (concat "\\("
	      "^:.+:.*\n" ; any line with a :SOMETHING:
	      "\\|^#\\+.*\n" ; anyline starting with a #+
	      "\\|^\\*.+.*\n" ; anyline where an asterisk starts the line
	      "\\)"))
(advice-add 'deft-parse-title :override
            (lambda (file contents)
              (if deft-use-filename-as-title
	          (deft-base-filename file)
	        (let* ((case-fold-search 't)
	               (begin (string-match "title: " contents))
	               (end-of-begin (match-end 0))
	               (end (string-match "\n" contents begin)))
	          (if begin
	              (substring contents end-of-begin end)
	            (format "%s" file))))))


;; Associate .mdx and with markdown-mode
(add-to-list 'auto-mode-alist '("\\.mdx\\'" . gfm-mode))

;; Associate .svg and with xml-mode
(add-to-list 'auto-mode-alist '("\\.svg\\'" . xml-mode))

(use-package! epa-file
  :config
  (epa-file-enable)
  (setq epg-gpg-program "/Users/allup/.gnupg")
  (setq epa-file-select-keys nil)
  )
(setq magit-commit-arguments '("--gpg-sign=%s"))
;; rainbow-mode
(use-package! rainbow-mode
  :hook (((css-mode scss-mode org-mode typescript-mode js-mode emacs-lisp-mode). rainbow-mode))
  :defer 5)

;; terminal improvements
(defun my-remove-cr (&optional begin end)
  "Remove line prefixes ending with carriage-return.

                                     ;;xha's copy/paste to/from register
                                     BEGIN END specifies region, otherwise works on entire buffer."
  (save-excursion
    (goto-char (or begin (point-min)))
    (while (re-search-forward "^.*\033\\[2K\033\\[1G" end t)
      (replace-match ""))))

;; copy to register
(defun xah-copy-to-register-1 ()
  "Copy current line or selection to register 1.

  URL `http://xahlee.info/emacs/emacs/elisp_copy-paste_register_1.html'
  Version: 2012-07-17 2022-10-03 2023-04-07"
  (interactive)
  (let (xp1 xp2)
    (if (region-active-p)
        (setq xp1 (region-beginning) xp2 (region-end))
      (setq xp1 (line-beginning-position) xp2 (line-end-position)))
    (copy-to-register ?1 xp1 xp2)
    (message "Copied to register 1: [%s]." (buffer-substring-no-properties xp1 xp2))))


;; paste from register
(defun xah-paste-from-register-1 ()
  "Copy current line or selection to register 1.

  URL `http://xahlee.info/emacs/emacs/elisp_copy-paste_register_1.html'
  Version: 2015-12-08 2023-04-07"
  (interactive)
  (when (region-active-p)
    (delete-region (region-beginning) (region-end)))
  (insert-register ?1 t))


;;make centered-cursor-mode global
(use-package centered-cursor-mode
  :demand
  :config
  ;; Optional, enables centered-cursor-mode in all buffers.
  (global-centered-cursor-mode))


;;make centered-cursor-mode global
(use-package evil-visual-mark-mode
  :demand
  :config
  ;; Optional, enables centered-cursor-mode in all buffers.
  (evil-visual-mark-mode))

;; pinentry setup for gpg
(use-package! pinentry
  :init (setq epa-pinentry-mode `loopback)
  (pinentry-start))

;; using evil-mode in the minibuffer
(setq evil-want-minibuffer t)

(setq vterm-always-compile-module t)
;; using apheleia everywhere
(apheleia-global-mode +1)

;; disable dired-omit-mode
(after! dired
  (setq dired-omit-mode nil))

;; solve gpg bug in magit
(setq transient-default-level 5)

;; disable smart parens globally
;;(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)


;; enable emacs everywhere in markdown mode with copilot
;; (remove-hook 'emacs-everywhere-init-hooks #'emacs-everywhere-major-mode-org-or-markdown) ; or #'org-mode if that's what's present

;; added windows resize key-bindings
(defhydra hydra/evil-window-resize (:color red)
  "Resize window"
  ("h" evil-window-decrease-width "decrease width")
  ("j" evil-window-decrease-height "decrease height")
  ("k" evil-window-increase-height "increase height")
  ("l" evil-window-increase-width "increase width")
  ("q" nil "quit"))
(map! :leader
      :prefix ("w" . "window")
      :n "r" #'hydra/evil-window-resize/body)

(setq auth-sources '("~/.authinfo" "~/.authinfo.gpg" "~/.netrc"))

(use-package aidermacs
  :bind (("C-c a" . aidermacs-transient-menu))
  :config
                                        ; Set API_KEY in .bashrc, that will automatically picked up by aider or in elisp
  (setenv "ANTHROPIC_API_KEY" anthropic-api-key)
                                        ; defun my-get-openrouter-api-key yourself elsewhere for security reasons
  ;; (setenv "OPENROUTER_API_KEY" (my-get-openrouter-api-key))
  :custom
                                        ; See the Configuration section below
  (aidermacs-use-architect-mode t)
  (aidermacs-default-model "sonnet"))
;; basic aider config
;; (use-package aider
;;   :config
;;   (setq aider-args '("--model" "anthropic/claude-3-5-sonnet-20241022")))
;; (setenv "ANTHROPIC_API_KEY" anthropic-api-key)
;; enable aider minor mode in aider files
(add-hook 'find-file-hook
          (lambda ()
            (when (and (buffer-file-name)
                       (string-match-p "aider" (buffer-file-name)))
              (aider-minor-mode 1))))

(setq
 gptel-model 'llama3.2:latest
 gptel-backend (gptel-make-ollama "Ollama"
                 :host "localhost:11434"
                 :stream t
                 :models '("llama3.2:latest")))

;;Assuming the buffer finishes successfully, close after 1 second.
(defun bury-compile-buffer-if-successful (buffer string)
  "Bury a compilation buffer if succeeded without warnings "
  (when (and (eq major-mode 'comint-mode)
             (string-match "finished" string)
             (not
              (with-current-buffer buffer
                (search-forward "warning" nil t))))
    (run-with-timer 1 nil
                    (lambda (buf)
                      (let ((window (get-buffer-window buf)))
                        (when (and (window-live-p window)
                                   (eq buf (window-buffer window)))
                          (delete-window window))))
                    buffer)))

(add-hook 'compilation-finish-functions #'bury-compile-buffer-if-successful)

;; if the process exits, kill the vterm buffer
(setq vterm-kill-buffer-on-exit t)

;; enable follow-mode so the treemacs cursor follows the buffer file. Also increase the default width to show more stuff.
(after! treemacs
  (treemacs-follow-mode 1)
  (setq treemacs-width 40))

;; gives a much better experience for files with marginalia
(after! marginalia
  (setq marginalia-censor-variables nil)

  (defadvice! +marginalia--anotate-local-file-colorful (cand)
    "Just a more colourful version of `marginalia--anotate-local-file'."
    :override #'marginalia--annotate-local-file
    (when-let (attrs (file-attributes (substitute-in-file-name
                                       (marginalia--full-candidate cand))
                                      'integer))
      (marginalia--fields
       ((marginalia--file-owner attrs)
        :width 12 :face 'marginalia-file-owner)
       ((marginalia--file-modes attrs))
       ((+marginalia-file-size-colorful (file-attribute-size attrs))
        :width 7)
       ((+marginalia--time-colorful (file-attribute-modification-time attrs))
        :width 12))))

  (defun +marginalia--time-colorful (time)
    (let* ((seconds (float-time (time-subtract (current-time) time)))
           (color (doom-blend
                   (face-attribute 'marginalia-date :foreground nil t)
                   (face-attribute 'marginalia-documentation :foreground nil t)
                   (/ 1.0 (log (+ 3 (/ (+ 1 seconds) 345600.0)))))))
      ;; 1 - log(3 + 1/(days + 1)) % grey
      (propertize (marginalia--time time) 'face (list :foreground color))))

  (defun +marginalia-file-size-colorful (size)
    (let* ((size-index (/ (log10 (+ 1 size)) 7.0))
           (color (if (< size-index 10000000) ; 10m
                      (doom-blend 'orange 'green size-index)
                    (doom-blend 'red 'orange (- size-index 1)))))
      (propertize (file-size-human-readable size) 'face (list :foreground color)))))

(blink-cursor-mode 1)


;; Ensure that `md` files are open in `gfm-mode` in doom emacs
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

;; Ensure that `mdx` files are open in `rjsx-mode` in doom emacs
(add-to-list 'auto-mode-alist '("\\.mdx\\'" . gfm-mode))
;; Ensure that `svg` files are open in `rjsx-mode` in doom emacs
(add-to-list 'auto-mode-alist '("\\.svg\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.org\\'" . copilot-mode))
(add-to-list 'auto-mode-alist '("\\.mdx\\'" . copilot-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . copilot-mode))

(add-hook 'emacs-everywhere-init-hooks #'gfm-mode)
(add-hook 'emacs-everywhere-init-hooks #'copilot-mode)


;; accept completion from copilot and fallback to company
(after! copilot
  (add-hook! (prog-mode markdown-mode gfm-mode org-mode) #'copilot-mode)
  (map! :map copilot-completion-map
        "TAB"     #'copilot-accept-completion
        "C-<tab>" #'copilot-accept-completion-by-word
        "C-p"     #'copilot-previous-completion
        "C-n"     #'copilot-next-completion))
;; :config
;; Disable company when copilot is active
;; (add-hook 'copilot-mode-hook (lambda ()
;;                                (when copilot-mode
(setq copilot-indent-offset-warning-disable t)

(when (daemonp)
  (exec-path-from-shell-initialize))

;; rust
;; debub adapter protocol
(require 'dap-lldb)
(require 'dap-cpptools)
(after! dap-mode
  (dap-ui-mode)
  (dap-ui-controls-mode 1)
  (require 'dap-lldb)
  (require 'dap-cpptools)
  (require 'dap-gdb-lldb)
  (dap-gdb-lldb-setup)
  (dap-cpptools-setup)
  (dap-register-debug-template "Rust::CppTools Run Configuration"
                               (list :type "cppdbg"
                                     :request "launch"
                                     :name "Rust::debug"
                                     :MIMode "gdb"
                                     :miDebuggerPath "rust-gdb"
                                     :environment []
                                     :program "${workspaceFolder}/target/debug/${workspaceRootFolderName}"
                                     :cwd "${workspaceFolder}"
                                     :console "external"
                                     :dap-compilation "cargo build"
                                     :dap-compilation-dir "${workspaceFolder}")))


(after! 'dap-mode
  (setq dap-default-terminal-kind "integrated") ;; Make sure that terminal programs open a term for I/O in an Emacs buffer
  (dap-auto-configure-mode +1))

(add-hook 'rust-mode-hook 'lsp-deferred)
(add-to-list 'warning-suppress-log-types '(lsp-mode))
(add-to-list 'warning-suppress-types '(lsp-mode))
;;ditaa
(setq org-ditaa-jar-path "/opt/homebrew/bin/ditaa.jar") ;; Make sure that terminal programs open a term for I/O in an Emacs buffer

;; Whitespace color corrections.
(require 'color)
(let* ((ws-lighten 30) ;; Amount in percentage to lighten up black.
       (ws-color (color-lighten-name "#000000" ws-lighten)))
  (custom-set-faces
   `(whitespace-newline                ((t (:foreground ,ws-color))))
   `(whitespace-missing-newline-at-eof ((t (:foreground ,ws-color))))
   `(whitespace-space                  ((t (:foreground ,ws-color))))
   `(whitespace-space-after-tab        ((t (:foreground ,ws-color))))
   `(whitespace-space-before-tab       ((t (:foreground ,ws-color))))
   `(whitespace-tab                    ((t (:foreground ,ws-color))))
   `(whitespace-trailing               ((t (:foreground ,ws-color))))))

;; svg-tag-mode
(use-package! svg-tag-mode
  :config
  (svg-tag-mode +1))

;; lsp-rust-analyzer-store-path, edit with the active path if rustic complains about rust-analyzer
(setq lsp-rust-analyzer-store-path "/Users/allup/.cargo/bin/rust-analyzer")

(after! persp-mode
  (defun display-workspaces-in-minibuffer ()
    (with-current-buffer " *Minibuf-0*"
      (erase-buffer)
      (insert (+workspace--tabline))))
  (run-with-idle-timer 1 t #'display-workspaces-in-minibuffer)
  (+workspace/display))
;; load env vars > EXPERIMENT!
(doom-load-envvars-file "~/.config/emacs/.local/env")

;; Invalidate projectile cache when switching projects
(defun my/projectile-invalidate-cache-on-switch ()
  "Invalidate projectile cache when switching projects."
  (projectile-invalidate-cache nil))
(add-hook 'projectile-after-switch-project-hook #'my/projectile-invalidate-cache-on-switch)


(after! yasnippet
  (yas-reload-all)
  (add-to-list 'yas-snippet-dirs "~/.config/emacs/snippets"))

;; Enable image viewing
(after! image
  (setq image-use-external-converter t)
  (add-to-list 'image-types 'svg)  ; Enable SVG support
  (add-to-list 'image-types 'jpeg))  ; Enable jpeg support

;; Configure org-mode for inline images
(after! org
  (setq org-startup-with-inline-images t)  ; Show inline images when opening org files
  (setq org-image-actual-width nil)        ; Use image size specifications in org files
  (add-hook 'org-mode-hook #'org-display-inline-images)) ; Auto-display images in org buffers

(after! solaire-mode
  (solaire-global-mode +1))

(setq projectile-indexing-method 'hybrid)

(setq
 indent-bars-color '(highlight :face-bg t :blend 0)
 indent-bars-pattern " . . . . ." ; play with the number of dots for your usual font size
 indent-bars-width-frac 0.25
 indent-bars-pad-frac 0.1)

;; allow for the use of the clipboard
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

;; add-to-aider macro
(defalias 'add-to-aider
  (kmacro "<return> SPC A a c SPC o -"))

;; file-one-up macro
(defalias 'file-one-up
  (kmacro "SPC o -"))
