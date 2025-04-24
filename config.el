;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;; =========================================================================
;;; BASIC CONFIGURATION
;;; =========================================================================

;; Personal information
(setq user-full-name "Gael Blanchemain"
      user-mail-address "gael.blanchemain@protonmail.com")

;; Load environment variables
(doom-load-envvars-file "~/.config/emacs/.local/env")

;; Set directories
(setq org-directory "~/Library/CloudStorage/Dropbox/orgmode")

;;; =========================================================================
;;; APPEARANCE
;;; =========================================================================

;; Font settings
(setq doom-font (font-spec :family "Fira Code" :size 11))

;; Theme
(setq doom-theme 'doom-one)

;; Line numbers
(setq display-line-numbers-type t)

;; Frame transparency
(set-frame-parameter (selected-frame) 'alpha '(95 95))
(add-to-list 'default-frame-alist '(alpha 95 95))

;; Set initial frame size and position
(setq initial-frame-alist
      (append initial-frame-alist
              '((top . 1) (left . 1) (width . 230) (height . 180))))

;; Solaire mode for better contrast
(after! solaire-mode
  (solaire-global-mode +1))

;; Cursor settings
(blink-cursor-mode 1)

;; Custom dashboard banner
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

;; Whitespace visualization settings
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

;; Indent guides configuration
(setq
 indent-bars-color '(highlight :face-bg t :blend 0)
 indent-bars-pattern " . . . . ." ; play with the number of dots for your usual font size
 indent-bars-width-frac 0.25
 indent-bars-pad-frac 0.1)

;;; =========================================================================
;;; KEY BINDINGS
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
      "y" #'yank-from-kill-ring)

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
      "7" #'switch-to-buffer-other-frame)

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

;; Window resizing with hydra
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

;;; =========================================================================
;;; EDITOR SETTINGS
;;; =========================================================================

;; Markdown configuration
(custom-set-faces!
  '(markdown-header-delimiter-face :foreground "#616161" :height 0.9)
  '(markdown-header-face-1 :height 1.8 :foreground "#A3BE8C" :weight extra-bold :inherit markdown-header-face)
  '(markdown-header-face-2 :height 1.4 :foreground "#EBCB8B" :weight extra-bold :inherit markdown-header-face)
  '(markdown-header-face-3 :height 1.2 :foreground "#D08770" :weight extra-bold :inherit markdown-header-face)
  '(markdown-header-face-4 :height 1.15 :foreground "#BF616A" :weight bold :inherit markdown-header-face)
  '(markdown-header-face-5 :height 1.1 :foreground "#b48ead" :weight bold :inherit markdown-header-face)
  '(markdown-header-face-6 :height 1.05 :foreground "#5e81ac" :weight semi-bold :inherit markdown-header-face))

(add-hook! (markdown-mode markdown-mode) #'visual-line-mode #'turn-off-auto-fill)

;; Enable visual line mode globally
(global-visual-line-mode +1)

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

;; SVG files in xml-mode
(add-to-list 'auto-mode-alist '("\\.svg\\'" . xml-mode))

;; Global tree-sitter mode
(global-tree-sitter-mode)

;; Apheleia formatter globally
(apheleia-global-mode +1)

;; Enable clipboard integration with system
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

;; Persistent terminal
(setq vterm-kill-buffer-on-exit t)
(setq vterm-always-compile-module t)

;; Enable centered-cursor-mode globally
(use-package centered-cursor-mode
  :demand
  :config
  (global-centered-cursor-mode))

;; Enable evil-visual-mark-mode
(use-package evil-visual-mark-mode
  :demand
  :config
  (evil-visual-mark-mode))

;; Evil in minibuffer
(setq evil-want-minibuffer t)

;; Disable dired-omit-mode
(after! dired
  (setq dired-omit-mode nil))

;; Terminal text handling
(defun my-remove-cr (&optional begin end)
  "Remove line prefixes ending with carriage-return.
BEGIN END specifies region, otherwise works on entire buffer."
  (save-excursion
    (goto-char (or begin (point-min)))
    (while (re-search-forward "^.*\033\\[2K\033\\[1G" end t)
      (replace-match ""))))

;; Utility functions for copy/paste with registers
(defun xah-copy-to-register-1 ()
  "Copy current line or selection to register 1.
URL `http://xahlee.info/emacs/emacs/elisp_copy-paste_register_1.html'"
  (interactive)
  (let (xp1 xp2)
    (if (region-active-p)
        (setq xp1 (region-beginning) xp2 (region-end))
      (setq xp1 (line-beginning-position) xp2 (line-end-position)))
    (copy-to-register ?1 xp1 xp2)
    (message "Copied to register 1: [%s]." (buffer-substring-no-properties xp1 xp2))))

(defun xah-paste-from-register-1 ()
  "Paste text from register 1.
URL `http://xahlee.info/emacs/emacs/elisp_copy-paste_register_1.html'"
  (interactive)
  (when (region-active-p)
    (delete-region (region-beginning) (region-end)))
  (insert-register ?1 t))

;; Define macros
(defalias 'add-to-aider
  (kmacro "<return> SPC A a c SPC o -"))

(defalias 'file-one-up
  (kmacro "SPC o -"))

;;; =========================================================================
;;; PACKAGE CONFIGURATIONS
;;; =========================================================================

;; Org and Org-roam
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
          ("d" "dictionary" plain "%?"
           :if-new
           (file+head "dictionary/${slug}.org" "#+title: ${title}\n#+filetags: :dictionary:\n")
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

;; Org-roam dailies
(setq org-roam-dailies-directory "daily/")
(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %?"
         :target (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n"))))

;; Org TODO keywords
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "|" "DONE(d)" "|" "Cancelled(c)")))

;; Deft for quick note access
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

;; Citar for bibliography management
(require 'citar)
(use-package citar
  :no-require
  :custom
  (org-cite-global-bibliography '("~/Library/CloudStorage/Dropbox/orgmode/master.json"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
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

;; Org-roam utility functions
(defun my/org-roam-node-has-tag (node tag)
  "Filter function to check if the given NODE has the specified TAG."
  (member tag (org-roam-node-tags node)))

(defun my/org-roam-node-find-by-tag ()
  "Find and open an Org-roam node based on a specified tag."
  (interactive)
  (let ((tag (read-string "Enter tag: ")))
    (org-roam-node-find nil nil (lambda (node) (my/org-roam-node-has-tag node tag)))))

(defun jethro/org-capture-inbox ()
  (interactive)
  (org-capture nil "i"))

(defun jethro/org-capture-slipbox ()
  (interactive)
  (org-capture nil "s"))

;; GPG/Pinentry
(use-package! epa-file
  :config
  (epa-file-enable)
  (setq epg-gpg-program "/Users/allup/.gnupg")
  (setq epa-file-select-keys nil))

(setq magit-commit-arguments '("--gpg-sign=%s"))
(setq transient-default-level 5)

(use-package! pinentry
  :init (setq epa-pinentry-mode `loopback)
  (pinentry-start))

(setq auth-sources '("~/.authinfo" "~/.authinfo.gpg" "~/.netrc"))

;; Rainbow mode for color visualization
(use-package! rainbow-mode
  :hook (((css-mode scss-mode org-mode typescript-mode js-mode emacs-lisp-mode). rainbow-mode))
  :defer 5)

;; Aidermacs configuration
(use-package aidermacs
  :bind (("C-c a" . aidermacs-transient-menu))
  :config
  :custom
  (aidermacs-default-model "sonnet"))

(setq aidermacs-backend 'vterm)
(add-hook 'find-file-hook
          (lambda ()
            (when (and (buffer-file-name)
                       (string-match-p "aider" (buffer-file-name)))
              (aider-minor-mode 1))))

;; GPTel (Ollama) configuration
(setq
 gptel-model 'llama3.2:latest
 gptel-backend (gptel-make-ollama "Ollama"
                 :host "localhost:11434"
                 :stream t
                 :models '("llama3.2:latest")))

;; Treemacs configuration
(after! treemacs
  (treemacs-follow-mode 1)
  (setq treemacs-width 40))

;; Marginalia improvements for file display
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

;; SVG tag mode
(use-package! svg-tag-mode
  :config
  (svg-tag-mode +1))

;; Image support
(after! image
  (setq image-use-external-converter t)
  (add-to-list 'image-types 'svg)  ; Enable SVG support
  (add-to-list 'image-types 'jpeg))  ; Enable jpeg support

;; Configure org-mode for inline images
(after! org
  (setq org-startup-with-inline-images t)  ; Show inline images when opening org files
  (setq org-image-actual-width nil)        ; Use image size specifications in org files
  (add-hook 'org-mode-hook #'org-display-inline-images)) ; Auto-display images in org buffers

;; Yasnippet configuration
(after! yasnippet
  (yas-reload-all)
  (add-to-list 'yas-snippet-dirs "~/.config/emacs/snippets"))

;; Compilation buffer auto-close
(defun bury-compile-buffer-if-successful (buffer string)
  "Bury a compilation buffer if succeeded without warnings."
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

;;; =========================================================================
;;; PROGRAMMING LANGUAGE SPECIFIC CONFIGURATIONS
;;; =========================================================================

;; Copilot configuration
(after! copilot
  (add-hook! (prog-mode markdown-mode markdown-mode org-mode) #'copilot-mode)
  (map! :map copilot-completion-map
        "TAB"     #'copilot-accept-completion
        "C-<tab>" #'copilot-accept-completion-by-word
        "C-p"     #'copilot-previous-completion
        "C-n"     #'copilot-next-completion))

(setq copilot-indent-offset-warning-disable t)

;; Initialize path from shell when running as daemon
(when (daemonp)
  (exec-path-from-shell-initialize))

;; Rust development configuration
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
  (setq dap-default-terminal-kind "integrated") ;; Terminal programs open in Emacs buffer
  (dap-auto-configure-mode +1))

(add-hook 'rust-mode-hook 'lsp-deferred)
(add-to-list 'warning-suppress-log-types '(lsp-mode))
(add-to-list 'warning-suppress-types '(lsp-mode))

;; Rust analyzer path
(setq lsp-rust-analyzer-store-path "/Users/allup/.cargo/bin/rust-analyzer")

;; Project management
(setq projectile-indexing-method 'hybrid)

;; Invalidate projectile cache only when switching Git branches
(defun my/projectile-invalidate-cache-on-branch-change ()
  "Invalidate projectile cache when switching Git branches."
  (let ((current-branch (magit-get-current-branch))
        (branch-file (expand-file-name ".projectile-branch" (projectile-project-root))))
    (when (and current-branch (file-exists-p (projectile-project-root)))
      (if (file-exists-p branch-file)
          (with-temp-buffer
            (insert-file-contents branch-file)
            (let ((stored-branch (string-trim (buffer-string))))
              (unless (string= stored-branch current-branch)
                (projectile-invalidate-cache nil)
                (with-temp-file branch-file
                  (insert current-branch)))))
        (with-temp-file branch-file
          (insert current-branch))))))

(add-hook 'projectile-after-switch-project-hook #'my/projectile-invalidate-cache-on-branch-change)

;; Workspaces display in minibuffer
(after! persp-mode
  (defun display-workspaces-in-minibuffer ()
    (with-current-buffer " *Minibuf-0*"
      (erase-buffer)
      (insert (+workspace--tabline))))
  (run-with-idle-timer 1 t #'display-workspaces-in-minibuffer)
  (+workspace/display))

;; Linear.app integration
(use-package linear
  :commands (linear-list-issues linear-new-issue)
  :config
  ;; Set API key from environment variable
  (when-let ((env-key (getenv "LINEAR_API_KEY")))
    (setq linear-api-key env-key)
    (message "Loaded Linear API key from environment"))

  ;; Alternative: Get API key from auth-source
  (when (and (not linear-api-key) (require 'auth-source nil t))
    (let ((auth-info (auth-source-search :host "api.linear.app" :require '(:secret) :max 1)))
      (when auth-info
        (let ((secret (plist-get (car auth-info) :secret)))
          (when secret
            (setq linear-api-key (if (functionp secret) (funcall secret) secret))
            (message "Loaded Linear API key from auth-source"))))))

  ;; Enable debug logging (set to nil to disable)
  (setq linear-debug t)

  ;; Optional: Add keybindings
  :bind (:map global-map
              ("C-c l l" . linear-list-issues)
              ("C-c l n" . linear-new-issue)))

;; Optional: Integration with Doom Emacs keybindings
(after! linear
  (map! :leader
        (:prefix ("L" . "Linear")
         :desc "List issues" "l" #'linear-list-issues
         :desc "New issue" "n" #'linear-new-issue
         :desc "Test connection" "t" #'linear-test-connection
         :desc "Toggle debug" "d" #'linear-toggle-debug)))

;; Ditaa configuration
(setq org-ditaa-jar-path "/opt/homebrew/bin/ditaa.jar")

;;; =========================================================================
;;; MISCELLANEOUS SETTINGS
;;; =========================================================================

;; General settings
(setq load-prefer-newer t
      search-highlight t
      search-whitespace-regexp ".*?"
      org-ellipsis " ▼ "
      org-adapt-indentation nil
      org-habit-show-habits-only-for-today t)

(setq initial-frame-alist
      (append initial-frame-alist
              '((top . 1) (left . 1) (width . 230) (height . 180))))
