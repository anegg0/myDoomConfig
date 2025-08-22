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
(setq doom-theme 'leuven-dark)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)



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

(map! :leader
      :desc "org-id-get-create"
      "5" #'org-id-get-create)

(map! :leader
      :desc "my/magit-submodule-update-init-recursive"
      "6" #'my/magit-submodule-update-init-recursive)
;; Frame and buffer navigation
(map! :leader
      :desc "my/org-export-html-and-open"
      "7" #'my/org-export-html-and-open)

;; Terminal
(map! :leader
      :desc "eat terminal"
      "o e" #'my/eat-popup)

(map! :leader
      :desc "my/org-md-export-to-markdown-visible-only"
      "e m" #'my/org-md-export-to-markdown-visible-only)

(map! :leader
      :desc "consult-buffer"
      "8" #'consult-buffer)

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

(map! :leader
      :prefix ("C" . "company-mode")
      "C" #'company-mode)

(map! :leader
      :prefix ("P" . "copilot-mode")
      "P" #'copilot-mode)
;;; =========================================================================
;;; APPEARANCE
;;; =========================================================================

;; Font settings
(setq doom-font (font-spec :family "Fira Code" :size 16))


;; Frame transparency
;; (set-frame-parameter (selected-frame) 'alpha '(95 95))
(add-to-list 'default-frame-alist '(alpha 95 95))

;; Set initial frame size and position
(setq initial-frame-alist
      (append initial-frame-alist
              '((top . 1) (left . 1) (width . 170) (height . 140))))
;; '((top . 1) (left . 1) (width . 200) (height . 140))))

;; Solaire mode for better contrast
(after! solaire-mode
  (solaire-global-mode +1))

;; Cursor settings
(blink-cursor-mode 1)



;;; =========================================================================
;;; ORGMODE
;;; =========================================================================

;; LaTeX/PDF export configuration
(after! ox-latex
  ;; Set the default LaTeX compiler (pdflatex, xelatex, or lualatex)
  (setq org-latex-compiler "pdflatex")
  
  ;; Configure the PDF compilation process
  (setq org-latex-pdf-process
        '("pdflatex -interaction nonstopmode -output-directory %o %f"
          "bibtex %b"
          "pdflatex -interaction nonstopmode -output-directory %o %f"
          "pdflatex -interaction nonstopmode -output-directory %o %f"))
  
  ;; Clean up auxiliary files after export
  (setq org-latex-logfiles-extensions
        '("lof" "lot" "tex~" "aux" "idx" "log" "out" "toc" "nav" 
          "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" 
          "ps" "spl" "bbl" "tex" "bcf"))
  
  ;; Configure code listings with minted (syntax highlighting)
  (setq org-latex-listings 'minted)
  (setq org-latex-minted-options
        '(("frame" "lines")
          ("fontsize" "\\scriptsize")
          ("linenos" "")))
  
  ;; Add minted package to default packages
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  
  ;; Custom LaTeX class with modified title handling
  (add-to-list 'org-latex-classes
               '("letter-style"
                 "\\documentclass[11pt,letterpaper]{article}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{graphicx}
\\usepackage{longtable}
\\usepackage{hyperref}
\\usepackage{geometry}
\\geometry{letterpaper,left=1in,right=1in,top=1in,bottom=1in}
\\usepackage{fancyhdr}
\\pagestyle{fancy}
\\fancyhf{}
\\rhead{\\thepage}
\\usepackage{titlesec}
\\titleformat{\\section}{\\Large\\bfseries}{\\thesection}{1em}{}
\\titleformat{\\subsection}{\\large\\bfseries}{\\thesubsection}{1em}{}
\\titleformat{\\subsubsection}{\\normalsize\\bfseries}{\\thesubsubsection}{1em}{}
% Redefine maketitle to show author and date
\\makeatletter
\\renewcommand{\\maketitle}{%
  \\begingroup
  \\renewcommand\\thefootnote{\\@fnsymbol\\c@footnote}%
  \\def\\@makefnmark{\\rlap{\\@textsuperscript{\\normalfont\\@thefnmark}}}%
  \\long\\def\\@makefntext##1{\\parindent 1em\\noindent
            \\hb@xt@1.8em{%
                \\hss\\@textsuperscript{\\normalfont\\@thefnmark}}##1}%
  \\global\\@topnum\\z@
  \\noindent
  \\@author\\\\
  \\@date\\\\[2ex]
  \\endgroup
  \\setcounter{footnote}{0}%
  \\global\\let\\thanks\\relax
  \\global\\let\\maketitle\\relax
  \\global\\let\\@maketitle\\relax
  \\global\\let\\@thanks\\@empty
  \\global\\let\\@author\\@empty
  \\global\\let\\@date\\@empty
  \\global\\let\\@title\\@empty
  \\global\\let\\title\\relax
  \\global\\let\\author\\relax
  \\global\\let\\date\\relax
  \\global\\let\\and\\relax
}
\\makeatother"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  
  ;; Set default class
  (setq org-latex-default-class "article")
  
  ;; Configure hyperref settings (without metadata)
  (setq org-latex-hyperref-template
        "\\hypersetup{
  pdfkeywords={%k},
  pdfcreator={%c},
  pdflang={%L},
  colorlinks=true,
  linkcolor=blue,
  citecolor=blue,
  urlcolor=blue
}")
  
  ;; Disable table of contents by default
  (setq org-latex-toc-command "")
  
  ;; Ensure \maketitle is called to display our custom title
  (setq org-latex-title-command "\\maketitle")
  
  ;; Table export settings
  (setq org-latex-tables-centered t)
  (setq org-latex-tables-booktabs t)
  
  ;; Image handling
  (setq org-latex-image-default-width "0.9\\linewidth")
  
  ;; Add custom packages if needed
  (add-to-list 'org-latex-packages-alist '("" "booktabs"))
  (add-to-list 'org-latex-packages-alist '("" "tabularx")))

;; Custom export function for PDF with preview
(defun my/org-export-pdf-and-open ()
  "Export current org file to PDF and open it."
  (interactive)
  (if (not (eq major-mode 'org-mode))
      (message "This is not an org-mode buffer!")
    (org-latex-export-to-pdf)
    (org-open-file (concat (file-name-sans-extension (buffer-file-name)) ".pdf"))))

;; Add keybinding for PDF export
(map! :leader
      :desc "Export to PDF and open"
      "e p" #'my/org-export-pdf-and-open)


;; Custom Markdown export function that always uses visible-only option,
;; removes anchor tags, and disables table of contents
(defun my/org-md-export-to-markdown-visible-only (&optional async subtreep)
  "Export current buffer to a Markdown file with visible-only option.
This only exports the visible content, ignoring hidden elements.
ASYNC and SUBTREEP have the same meaning as in `org-md-export-to-markdown`.
Also removes anchor tags (<a id=\"org...\"></a>) from the output
and disables the table of contents."
  (interactive)
  ;; Make sure ox-md is loaded
  (require 'ox-md)

  ;; Set export options
  (let* ((org-md-headline-style 'atx) ; Use # style headers instead of underlines
         (org-html-self-link-headlines nil) ; Disable self-link headlines
         (org-export-with-toc nil)) ; Disable table of contents

    ;; Call the original function with visible-only set to t
    (let ((outfile (org-md-export-to-markdown async subtreep t)))
      ;; Post-process to remove anchor tags
      (with-temp-buffer
        (insert-file-contents outfile)
        (goto-char (point-min))
        ;; Remove anchor tags
        (while (re-search-forward "<a id=\"org[^\"]+\"></a>\n?" nil t)
          (replace-match ""))
        ;; Remove any TOC markers
        (goto-char (point-min))
        (when (re-search-forward "^\s*\\[TOC\\]\s*$" nil t)
          (replace-match ""))
        ;; Save the changes
        (write-file outfile))
      ;; Return the filename
      outfile)))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Library/CloudStorage/ProtonDrive-gael.blanchemain@protonmail.com-folder/orgmode" )

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
  
  ;; Disable company-mode in org-mode
  (add-hook 'org-mode-hook (lambda () (company-mode -1)))

  ;; Hot-reload function for org-mode files
  (defun my/org-hot-reload-html-export ()
    "Enable hot-reload for the current org-mode buffer.
    This will automatically export to HTML whenever the file is saved."
    (interactive)
    (if (not (eq major-mode 'org-mode))
        (message "This is not an org-mode buffer!")
      (let ((hook-func (lambda () 
                         (save-excursion
                           (let ((org-export-show-temporary-export-buffer nil))
                             (org-html-export-to-html nil 'visible-only nil nil)
                             (browse-url-of-file (concat (file-name-sans-extension (buffer-file-name)) ".html")))))))
        (add-hook 'after-save-hook hook-func nil t)
        (message "Hot-reload enabled: this buffer will export to HTML on save"))))

  (defvar-local my/org-hot-reload-hook-function nil
    "Store the hook function for hot-reload to enable toggling.")

  (defun my/org-hot-reload-toggle ()
    "Toggle hot-reload HTML export for the current org-mode buffer."
    (interactive)
    (if (not (eq major-mode 'org-mode))
        (message "This is not an org-mode buffer!")
      (if my/org-hot-reload-hook-function
          (progn
            (remove-hook 'after-save-hook my/org-hot-reload-hook-function t)
            (setq my/org-hot-reload-hook-function nil)
            (message "Hot-reload disabled for this buffer"))
        (setq my/org-hot-reload-hook-function
              (lambda () 
                (save-excursion
                  (let ((org-export-show-temporary-export-buffer nil))
                    (org-html-export-to-html nil 'visible-only nil nil)))))
        (add-hook 'after-save-hook my/org-hot-reload-hook-function nil t)
        (message "Hot-reload enabled: this buffer will export to HTML on save"))))

  (defun my/org-export-html-and-open ()
    "Export current org file to HTML and open it in the browser."
    (interactive)
    (if (not (eq major-mode 'org-mode))
        (message "This is not an org-mode buffer!")
      (save-excursion
        (let ((org-export-show-temporary-export-buffer nil))
          (org-html-export-to-html nil )
          (browse-url-of-file (concat (file-name-sans-extension (buffer-file-name)) ".html"))))))

  ;; Configure org-mode for inline images
  (setq org-startup-with-inline-images t)  ; Show inline images when opening org files
  (setq org-image-actual-width nil)        ; Use image size specifications in org files
  (add-hook 'org-mode-hook #'org-display-inline-images)

  ;; Custom TODO keywords
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(i)" "IN-REVIEW(r)" "BACKLOG(b)" "BLOCKED(l)" "|" "DONE(d)" "CANCELED(c)" "DUPLICATE(p)" "NEXT(n)" "HOLD(h)" "WAITING-ON(w)")))

  ;; Optional: Add custom faces for your TODO states
  (setq org-todo-keyword-faces
        '(("TODO" . (:foreground "red" :weight bold))
          ("IN-PROGRESS" . (:foreground "blue" :weight bold))
          ("IN-REVIEW" . (:foreground "orange" :weight bold))
          ("BACKLOG" . (:foreground "purple" :weight bold))
          ("BLOCKED" . (:foreground "green" :weight bold))
          ("DONE" . (:foreground "green" :weight bold))
          ("CANCELED" . (:foreground "gray" :weight bold))
          ("NEXT" . (:foreground "gray" :weight bold))
          ("DUPLICATE" . (:foreground "black" :weight bold))))

  ;; Define org-agenda-files to include the right directories
  ;; Include all org files from main directories for agenda
  (setq org-agenda-files (list
                          (expand-file-name "main" org-directory)
                          (expand-file-name "daily" org-directory)
                          (expand-file-name "gtd" org-directory)
                          (expand-file-name "reference" org-directory)
                          (expand-file-name "articles" org-directory)
                          ;; Explicitly include linear.org
                          (expand-file-name "gtd/linear.org" org-directory)))

  ;; Let agenda inherit TODO keywords from org-todo-keywords
  ;; (Removed redundant org-agenda-todo-keywords-for-agenda setting)

  ;; Set which TODO states should be included in the agenda by default
  ;; This can include both active and inactive states
  (setq org-agenda-todo-list-sublevels t)

  ;; Include all TODO states in agenda views
  (setq org-agenda-todo-ignore-scheduled nil
        org-agenda-todo-ignore-deadlines nil
        org-agenda-todo-ignore-timestamp nil
        org-agenda-todo-ignore-with-date nil)

  ;; Enable refile targets to include agenda files
  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))

  (setq org-capture-templates
        `(("i" "Inbox" entry  (file "gtd/inbox.org")
           ,(concat "* TODO %?\n"
                    "/Entered on/ %U"))
          ("s" "Slipbox" entry  (file "braindump/org/braindump.org")
           "* %?\n")))

  (defun jethro/org-capture-inbox ()
    (interactive)
    (org-capture nil "i"))

  (defun jethro/org-capture-slipbox ()
    (interactive)
    (org-capture nil "s"))

  (bind-key "C-c <tab>" #'jethro/org-capture-inbox)
  (bind-key "C-c SPC" #'jethro/org-capture-slipbox)

  (defun my/org-todo-list-all-by-tag (tag)
    "Display all TODO states in org-todo-list filtered by TAG."
    (interactive "sTag: ")
    (let ((org-agenda-todo-keywords-for-agenda org-todo-keywords)
          (org-agenda-tag-filter-preset `(,(concat "+" tag))))
      (org-todo-list nil)))

  (defun jethro/org-archive-done-tasks ()
    "Archive all done tasks."
    (interactive)
    (org-map-entries 'org-archive-subtree "/DONE" 'file))
  )

;;; =========================================================================
;;; ORGROAM
;;; =========================================================================

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
           :if-new (file+head "catb/${slug}.org"
                              "#+title: ${title}\n#+TAGS: :\n#+FILETAGS: :catb:\n")
           :immediate-finish t
           :unnarrowed t)
          ("o" "OCL" plain
           "%?"
           :if-new (file+head "OCL/OCL_${slug}.org"
                              "#+title: ${title}\n#+TAGS: :\n#+FILETAGS: :OCL:e:\n")
           :immediate-finish t
           :unnarrowed t)
          ("p" "prompts" plain
           "%?"
           :if-new (file+head "prompts/twai_${slug}.org"
                              "#+title: ${title}\n#+TAGS: :\n#+FILETAGS: :twai:e:\n")
           :immediate-finish t
           :unnarrowed t)
          ("r" "reference" plain "%?"
           :if-new
           (file+head "reference/${slug}.org" "#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
          ("a" "article" plain "%?"
           :if-new
           (file+head "articles/${slug}.org" "#+hugo_base_dir: ~/dev/gr0wing-hugo/\n#+HUGO_SECTION: posts\n#+HUGO_CODE_FENCE: nil\n* ${title} :orgmode:\n#+EXPORT_FILE_NAME: ${slug}\n:EXPORT_DATE: %(format-time-string \"%Y-%m-%d\")\n:END:\n#+title: ${title}\n#+TAGS: :\n#+FILETAGS: :article:\n\n")
           :immediate-finish t
           :unnarrowed t)
          ("g" "glossary" plain "%?"
           :if-new
           (file+head "glossary/${slug}.org" "#+title: ${title}\n#+filetags: :glossary:\n")
           :immediate-finish t
           :unnarrowed t)
          ))


  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
        (file-name-nondirectory
         (directory-file-name
          (file-name-directory
           (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (error "")))
  (setq org-roam-node-display-template
        (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))


  ;; Org-roam utility functions
  (defun my/org-roam-node-has-tag (node tag)
    "Filter function to check if the given NODE has the specified TAG."
    (member tag (org-roam-node-tags node)))

  (defun my/org-roam-node-find-by-tag ()
    "Find and open an Org-roam node based on a specified tag."
    (interactive)
    (let ((tag (read-string "Enter tag: ")))
      (org-roam-node-find nil nil (lambda (node) (my/org-roam-node-has-tag node tag)))))

  (defun my/org-roam-find-files-without-id ()
    "Find org files without org-roam ID property and write list to a file.
  The list will be saved to 'list-of-orgmode-files-without-a-roam-id.org'."
    (interactive)
    (let* ((org-dir (expand-file-name org-roam-directory))
           (output-file (expand-file-name "list-of-orgmode-files-without-a-roam-id.org" org-dir))
           (org-files (directory-files-recursively org-dir "\\.org$"))
           (files-without-id '()))

      ;; Check each org file for org-roam ID
      (dolist (file org-files)
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (unless (re-search-forward "^:ID:\\s-+[[:xdigit:]-]+" nil t)
            (push file files-without-id))))

      ;; Write results to the output file
      (with-temp-file output-file
        (insert "#+TITLE: Org Files Without Roam IDs\n\n")
        (insert "* Files missing org-roam IDs\n\n")
        (if files-without-id
            (dolist (file (sort files-without-id #'string<))
              (let ((rel-path (file-relative-name file org-dir)))
                (insert (format "- [[file:%s][%s]]\n" rel-path rel-path))))
          (insert "All files have org-roam IDs.\n")))

      (message "Found %d files without org-roam IDs. Results saved to %s"
               (length files-without-id) output-file)

      ;; Open the file
      (find-file output-file)))

  ;; Add keybinding for the new function
  (map! :leader
        :prefix "r"
        :desc "Find files without org-roam IDs" "m" #'my/org-roam-find-files-without-id)
  )

;;; =========================================================================
;;; EDITOR
;;; =========================================================================

(setq +lsp-backend 'eglot)

;; Show Eglot status in modeline
(setq eglot-report-progress t)

;; Custom modeline segment for Eglot
(defun +modeline-eglot-status ()
  "Display Eglot connection status."
  (when (and (bound-and-true-p eglot--managed-mode)
             (eglot-current-server))
    (let* ((server (eglot-current-server))
           (nick (eglot--project-nickname server))
           (status (if (eglot--shutdown-requested server)
                       "disconnected"
                     "connected")))
      (propertize (format " LSP[%s:%s]" nick status)
                  'face (if (string= status "connected")
                            'success
                          'error)))))

;; Add to modeline
(add-to-list 'global-mode-string '(:eval (+modeline-eglot-status)))

;; Make white spaces visible in programming modes.
(after! prog-mode
  (add-hook! prog-mode #'whitespace-mode))

;; Enable word-wrap-mode globally
(global-visual-line-mode 1)

;; Enable drag-and-drop functionality without emacsclient
(defun my/drag-n-drop-handler (event)
  "Handle files dropped onto Emacs frame."
  (interactive "e")
  (let* ((window (posn-window (event-start event)))
         (files (mapcar (lambda (x)
                          (if (eq system-type 'windows-nt)
                              (substitute-in-file-name x) x))
                        (car (last event)))))
    (with-selected-window window
      (mapc (lambda (file)
              (find-file file))
            files))))

(use-package evil
  :init
  (setq evil-undo-system 'undo-fu))

(setq undo-limit 67108864) ; 64mb.
(setq undo-strong-limit 100663296) ; 96mb.
(setq undo-outer-limit 1006632960) ; 960mb.

;; Enable evil-visual-mark-mode
(use-package evil-visual-mark-mode
  :demand
  :config
  (evil-visual-mark-mode))

;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook ((prog-mode . copilot-mode)
         (git-commit-mode . copilot-mode)  ;; Enable copilot in git commit messages
         (with-editor-mode . copilot-mode)) ;; Enable copilot in magit commit editor
  :bind (:map copilot-completion-map
              ("C-<tab>" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("<tab>" . 'copilot-accept-completion-by-word))
  :config
  (add-to-list 'copilot-indentation-alist '(prog-mode 2))
  (add-to-list 'copilot-indentation-alist '(org-mode 2))
  (add-to-list 'copilot-indentation-alist '(text-mode 2))
  (add-to-list 'copilot-indentation-alist '(markdown-mode 2))
  (add-to-list 'copilot-indentation-alist '(gfm-mode 2))
  (add-to-list 'copilot-indentation-alist '(rust-mode 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2))
  (add-to-list 'copilot-indentation-alist '(git-commit-mode 2))
  (add-to-list 'copilot-indentation-alist '(with-editor-mode 2))

  )

;; Copilot Chat configuration
(use-package! copilot-chat
  :after (copilot)
  :config
  ;;   ;; Configure copilot-chat backend (curl is recommended)
  (setq copilot-chat-backend 'curl)

  ;;   ;; Optional: Set custom keybindings for copilot-chat
  (map! :leader
        :prefix ("C c" . "copilot-chat")
        :desc "Start chat" "c" #'copilot-chat-display
        :desc "Ask about region" "r" #'copilot-chat-ask-region
        :desc "Ask about buffer" "b" #'copilot-chat-ask-buffer
        :desc "Explain code" "e" #'copilot-chat-explain
        :desc "Fix code" "f" #'copilot-chat-fix
        :desc "Generate docs" "d" #'copilot-chat-doc
        :desc "Review code" "R" #'copilot-chat-review
        :desc "Add code" "a" #'copilot-chat-add))

;; Disable dired-omit-mode globally
(remove-hook 'dired-mode-hook 'dired-omit-mode)

;; Enable auto-revert for dired buffers globally
(setq dired-auto-revert-buffer t)
;; Utility functions for copy/paste with registers
(defun xah-copy-to-register-1 ()
  "Copy current line or selection to register 1.

See also:
`xah-copy-to-register-1'
`xah-append-to-register-1'
`xah-paste-from-register-1'
`xah-clear-register-1'

URL `http://xahlee.info/emacs/emacs/elisp_copy-paste_register_1.html'
Version: 2012-07-17 2022-10-03 2023-04-07"
  (interactive)
  (let (xp1 xp2)
    (if (region-active-p)
        (setq xp1 (region-beginning) xp2 (region-end))
      (setq xp1 (line-beginning-position) xp2 (line-end-position)))
    (copy-to-register ?1 xp1 xp2)
    (message "Copied to register 1: [%s]." (buffer-substring-no-properties xp1 xp2))))

(defun xah-paste-from-register-1 ()
  "Paste text from register 1.

See also:
`xah-copy-to-register-1'
`xah-append-to-register-1'
`xah-paste-from-register-1'
`xah-clear-register-1'

URL `http://xahlee.info/emacs/emacs/elisp_copy-paste_register_1.html'
Version: 2015-12-08 2023-04-07"
  (interactive)
  (when (region-active-p)
    (delete-region (region-beginning) (region-end)))
  (insert-register ?1 t))

;; Emacs Everywhere Configuration
(after! emacs-everywhere
  ;; Set the default major mode to markdown-mode
  (setq emacs-everywhere-major-mode-function 'markdown-mode)
  
  ;; Store emacs-everywhere messages in ProtonDrive
  (setq emacs-everywhere-file-dir 
        (expand-file-name "~/Library/CloudStorage/ProtonDrive-gael.blanchemain@protonmail.com-folder/orgmode/emacs-everywhere-msgs/"))
  
  ;; Custom filename function to add .md extension
  (defun my/emacs-everywhere-markdown-filename (app-info)
    "Generate a markdown filename for emacs-everywhere."
    (concat "emacs-everywhere-"
            (format-time-string "%Y%m%d-%H%M%S-" (current-time))
            (emacs-everywhere-app-class app-info)
            ".md"))
  
  ;; Use custom filename function for markdown files
  (setq emacs-everywhere-filename-function #'my/emacs-everywhere-markdown-filename)

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
                (gfm-mode))))

  ;; Disable company-mode for markdown files
  (add-hook 'markdown-mode-hook (lambda () (company-mode -1)))
  (add-hook 'gfm-mode-hook (lambda () (company-mode -1))))



;;; =========================================================================
;;; WORKSPACES & PROJECTILE
;;; =========================================================================

;; Configure workspace-project integration
(after! persp-mode
  ;; Automatically create workspaces when switching projects
  (setq +workspaces-on-switch-project-behavior 'ask)  ; Options: t, nil, 'ask
  
  ;; Set workspace save directory
  (setq persp-save-dir (expand-file-name "workspaces/" doom-etc-dir))
  
  ;; Auto-save workspace sessions
  (setq persp-auto-save-opt 2)  ; 2 = save when killing Emacs
  (setq persp-auto-save-persps-to-their-file-before-kill t)
  
  ;; Session file naming
  (setq persp-auto-save-fname "auto-save")
  
  ;; Include project root in workspace names for clarity
  (setq +workspaces-switch-project-function #'+workspaces/switch-to-project)
  
  ;; Save workspaces more frequently (optional)
  (setq persp-auto-save-num-of-the-last-sessions 10)
  
  ;; Create workspace names based on project root
  (defun my/workspace-name-from-project (project-root)
    "Generate workspace name from PROJECT-ROOT."
    (if project-root
        (file-name-nondirectory (string-trim-right project-root "/"))
      "default"))

  ;; Enhanced workspace-project persistence functions
  (defun my/save-workspace-with-project ()
    "Save current workspace with its project association."
    (interactive)
    (let* ((workspace-name (safe-persp-name (get-current-persp)))
           (project-root (projectile-project-root))
           (workspace-file (expand-file-name 
                           (concat workspace-name ".el") 
                           persp-save-dir)))
      (when workspace-name
        ;; Save the workspace - pass workspace name as a list
        (persp-save-to-file-by-names workspace-file (list workspace-name) t)
        ;; Save project association metadata
        (when project-root
          (my/save-workspace-project-metadata workspace-name project-root))
        (message "Saved workspace '%s' with project '%s'" 
                workspace-name (or project-root "none")))))

  (defun my/save-workspace-project-metadata (workspace-name project-root)
    "Save metadata linking WORKSPACE-NAME to PROJECT-ROOT."
    (let ((metadata-file (expand-file-name "workspace-projects.el" persp-save-dir))
          (metadata (if (file-exists-p (expand-file-name "workspace-projects.el" persp-save-dir))
                       (with-temp-buffer
                         (insert-file-contents (expand-file-name "workspace-projects.el" persp-save-dir))
                         (read (current-buffer)))
                     '())))
      ;; Update or add the workspace-project association
      (setq metadata (assq-delete-all (intern workspace-name) metadata))
      (push (cons (intern workspace-name) project-root) metadata)
      ;; Save updated metadata
      (with-temp-file metadata-file
        (prin1 metadata (current-buffer)))))

  (defun my/load-workspace-with-project (workspace-name)
    "Load workspace and switch to its associated project."
    (interactive 
     (list (completing-read "Workspace: " 
                           (my/get-saved-workspace-names))))
    (let* ((workspace-file (expand-file-name 
                           (concat workspace-name ".el") 
                           persp-save-dir))
           (project-root (my/get-workspace-project workspace-name)))
      ;; Load the workspace
      (when (file-exists-p workspace-file)
        (persp-load-from-file-by-names workspace-file workspace-name)
        ;; Switch to associated project if it exists
        (when (and project-root (file-directory-p project-root))
          (projectile-switch-project-by-name project-root))
        (message "Loaded workspace '%s' with project '%s'" 
                workspace-name (or project-root "none")))))

  (defun my/get-saved-workspace-names ()
    "Get list of saved workspace names."
    (when (file-directory-p persp-save-dir)
      (mapcar (lambda (file) 
                (file-name-sans-extension file))
              (directory-files persp-save-dir nil "\\.el$"))))

  (defun my/get-workspace-project (workspace-name)
    "Get project root associated with WORKSPACE-NAME."
    (let ((metadata-file (expand-file-name "workspace-projects.el" persp-save-dir)))
      (when (file-exists-p metadata-file)
        (with-temp-buffer
          (insert-file-contents metadata-file)
          (let ((metadata (read (current-buffer))))
            (cdr (assq (intern workspace-name) metadata)))))))

  ;; Auto-save workspace when switching away
  (defun my/auto-save-workspace-on-switch ()
    "Automatically save current workspace when switching away."
    (let ((current-workspace (safe-persp-name (get-current-persp))))
      (when (and current-workspace 
                 (not (string= current-workspace persp-nil-name)))
        (condition-case err
            (my/save-workspace-with-project)
          (error (message "Failed to auto-save workspace: %s" 
                         (error-message-string err)))))))

  ;; Enhanced session management
  (defun my/save-complete-session ()
    "Save complete session including all workspaces and their project associations."
    (interactive)
    (let ((session-name (read-string "Session name: " 
                                    (format-time-string "%Y%m%d-%H%M"))))
      ;; Save persp session
      (persp-save-state-to-file 
       (expand-file-name (concat session-name ".persp") persp-save-dir))
      ;; Save all individual workspaces with their projects
      (dolist (workspace (persp-names))
        (unless (string= workspace persp-nil-name)
          (persp-switch workspace)
          (my/save-workspace-with-project)))
      (message "Saved complete session '%s'" session-name)))

  (defun my/load-complete-session ()
    "Load complete session with all workspaces and project associations."
    (interactive)
    (let* ((session-files (when (file-directory-p persp-save-dir)
                           (directory-files persp-save-dir nil "\\.persp$")))
           (session-name (completing-read "Session: " 
                                         (mapcar #'file-name-sans-extension session-files))))
      (when session-name
        (persp-load-state-from-file 
         (expand-file-name (concat session-name ".persp") persp-save-dir))
        (message "Loaded session '%s'" session-name)))))

;; Optional: Enable automatic saving with performance considerations
(defvar my/workspace-auto-save-enabled nil
  "Whether to enable automatic workspace saving.")

(defun my/toggle-workspace-auto-save ()
  "Toggle automatic workspace saving."
  (interactive)
  (setq my/workspace-auto-save-enabled (not my/workspace-auto-save-enabled))
  (if my/workspace-auto-save-enabled
      (progn
        ;; Add hooks for auto-saving
        (add-hook 'persp-before-switch-functions #'my/auto-save-workspace-on-switch)
        (add-hook 'kill-emacs-hook #'my/save-complete-session)
        (message "Workspace auto-save enabled"))
    (progn
      ;; Remove hooks
      (remove-hook 'persp-before-switch-functions #'my/auto-save-workspace-on-switch)
      (remove-hook 'kill-emacs-hook #'my/save-complete-session)
      (message "Workspace auto-save disabled"))))

;; Keybindings for workspace-project management
(map! :leader
      :prefix ("TAB w" . "workspace-project")
      :desc "Save workspace with project" "s" #'my/save-workspace-with-project
      :desc "Load workspace with project" "l" #'my/load-workspace-with-project
      :desc "Save complete session" "S" #'my/save-complete-session
      :desc "Load complete session" "L" #'my/load-complete-session
      :desc "Toggle auto-save" "a" #'my/toggle-workspace-auto-save)

;; Enhanced projectile-workspace integration
(defun my/projectile-create-workspace-for-project ()
  "Create or switch to workspace for current project."
  (interactive)
  (let* ((project-root (projectile-project-root))
         (workspace-name (when project-root
                          (my/workspace-name-from-project project-root))))
    (when workspace-name
      (+workspace/switch-to workspace-name)
      (when project-root
        (my/save-workspace-project-metadata workspace-name project-root))
      (message "Switched to workspace '%s' for project '%s'" 
               workspace-name project-root))))

;; Override projectile switch behavior to always create workspaces
(defun my/projectile-switch-project-with-workspace ()
  "Switch project and create/switch to associated workspace."
  (interactive)
  (let ((+workspaces-on-switch-project-behavior t))
    (call-interactively #'projectile-switch-project)))

;; Additional keybindings for enhanced project-workspace workflow
(map! :leader
      :prefix "p"
      :desc "Switch project with workspace" "W" #'my/projectile-switch-project-with-workspace
      :desc "Create workspace for project" "w" #'my/projectile-create-workspace-for-project)

;; Workarounds for known issues
(defun my/cleanup-magit-buffers-in-workspace ()
  "Clean up magit buffers in current workspace to prevent persistence issues."
  (interactive)
  (let ((magit-buffers (cl-remove-if-not 
                       (lambda (buf) 
                         (string-match-p "^\\*magit" (buffer-name buf)))
                       (persp-buffer-list))))
    (dolist (buf magit-buffers)
      (persp-remove-buffer buf))
    (message "Cleaned up %d magit buffers from workspace" (length magit-buffers))))

;; Automatically clean magit buffers before saving workspace
(defun my/pre-save-workspace-cleanup ()
  "Clean up problematic buffers before saving workspace."
  (my/cleanup-magit-buffers-in-workspace)
  ;; Add other cleanup operations here as needed
  )

;; Enhanced workspace saving with cleanup
(defun my/save-workspace-with-project-safe ()
  "Save current workspace with project association, with cleanup."
  (interactive)
  (my/pre-save-workspace-cleanup)
  (my/save-workspace-with-project))

;; Utility to show current workspace-project associations
(defun my/show-workspace-project-associations ()
  "Display current workspace-project associations."
  (interactive)
  (let* ((metadata-file (expand-file-name "workspace-projects.el" persp-save-dir))
         (associations (when (file-exists-p metadata-file)
                        (with-temp-buffer
                          (insert-file-contents metadata-file)
                          (read (current-buffer))))))
    (if associations
        (with-current-buffer (get-buffer-create "*Workspace-Project Associations*")
          (erase-buffer)
          (insert "Workspace-Project Associations:\n\n")
          (dolist (assoc associations)
            (insert (format "%-20s -> %s\n" (car assoc) (cdr assoc))))
          (display-buffer (current-buffer)))
      (message "No workspace-project associations found"))))

;; Keybinding for safe saving and utilities
(map! :leader
      :prefix ("TAB w" . "workspace-project")
      :desc "Safe save workspace" "C" #'my/save-workspace-with-project-safe
      :desc "Show associations" "A" #'my/show-workspace-project-associations
      :desc "Clean magit buffers" "c" #'my/cleanup-magit-buffers-in-workspace)

;;; =========================================================================
;;; PROJECTILE
;;; =========================================================================
;; Auto-invalidate projectile cache when switching git branches: untested code!
;; Auto-invalidate projectile cache when switching git branches: untested code!
(after! magit

  (defun my/magit-submodule-update-init-recursive ()
    "Run 'git submodule update --init --recursive' via Magit."
    (interactive)
    (magit-run-git-async "submodule" "update" "--init" "--recursive"))

  (defun my/magit-checkout-advice (&rest _)
    "Advice function to run after checkout."
    (let ((project-root (projectile-project-root)))
      (when project-root
        ;; Invalidate projectile cache for all projects
        (message "Invalidating projectile cache for %s" project-root)
        (projectile-invalidate-cache nil)

        ;; Run submodule update only for arbitrum-docs repository
        (when (string-match-p "~/dev/OCL/arbitrum-docs" (expand-file-name project-root))
          (message "Running submodule update for arbitrum-docs repository")
          (my/magit-submodule-update-init-recursive)))))

  ;; Add advice to magit-checkout
  (advice-add 'magit-checkout :after #'my/magit-checkout-advice)
  )


;;; =========================================================================
;;; LINEAR
;;; =========================================================================

;; Load linear-emacs from local development directory
(add-load-path! "~/dev/linear-emacs/")
(require 'linear-emacs nil t)

(defun my/linear-load-api-key-from-auth-source ()
  "Load Linear API key from auth-source."
  (interactive)
  (require 'auth-source)
  (let* ((auth-info (auth-source-search :host "api.linear.app" :user "apikey" :max 1))
         (secret (when auth-info
                   (funcall (plist-get (car auth-info) :secret)))))
    (if secret
        (progn
          (setq linear-emacs-api-key secret)
          (message "Successfully loaded Linear API key from auth-source"))
      (message "Failed to retrieve Linear API key from auth-source"))))

(after! linear-emacs
  ;; (linear-emacs-load-api-key-from-env)
  (my/linear-load-api-key-from-auth-source)
  (setq linear-emacs-org-file-path (expand-file-name "~/Library/CloudStorage/ProtonDrive-gael.blanchemain@protonmail.com-folder/orgmode/gtd/linear.org" org-directory))
  ;; Improved synchronization function that only updates the changed issue
  (defun my/linear-sync-single-issue-at-point ()
    "Sync only the current issue at point to Linear API."
    (interactive)
    (save-excursion
      ;; Move to the beginning of the current heading
      (org-back-to-heading t)
      ;; Check if this is a Linear issue heading
      (when (looking-at "^\\*\\*\\* \\(TODO\\|IN-PROGRESS\\|IN-REVIEW\\|BACKLOG\\|BLOCKED\\|DONE\\|CANCELED\)")
        (let ((todo-state (match-string 1))
              (issue-id nil)
              (issue-identifier nil)
              (team-id nil))
          ;; Get issue ID, identifier, and team ID from properties
          (save-excursion
            ;; Move to the property drawer
            (forward-line)
            (when (looking-at ":PROPERTIES:")
              (forward-line)
              (while (and (not (looking-at ":END:"))
                          (not (eobp)))
                (cond
                 ((looking-at ":ID:\\s-+\\(.+\\)")
                  (setq issue-id (match-string 1)))
                 ((looking-at ":ID-LINEAR:\\s-+\\(.+\\)")
                  (setq issue-identifier (match-string 1)))
                 ;; Extract team ID from the TEAM property
                 ((looking-at ":TEAM:\\s-+\\(.+\\)")
                  ;; Fetch the actual team ID based on team name
                  (let ((team-name (match-string 1)))
                    (setq team-id (linear-emacs--get-team-id-by-name team-name)))))
                (forward-line))))

          ;; If we found an issue ID, state, and team ID, update the Linear API
          (when (and issue-id issue-identifier team-id)
            ;; Map org TODO state to Linear state
            (let ((linear-emacs-state (cond
                                       ((string= todo-state "TODO") "Todo")
                                       ((string= todo-state "IN-PROGRESS") "In Progress")
                                       ((string= todo-state "IN-REVIEW") "In Review")
                                       ((string= todo-state "BACKLOG") "Backlog")
                                       ((string= todo-state "BLOCKED") "Blocked")
                                       ((string= todo-state "DONE") "Done")
                                       ((string= todo-state "CANCELED") "Canceled")
                                       ((string= todo-state "DUPLICATE") "Duplicate")
                                       ;; Non-Linear states - don't sync to Linear
                                       ((member todo-state '("NEXT" "HOLD" "WAITING-ON")) nil)
                                       (t nil))))
              (when linear-emacs-state
                (linear-emacs--update-issue-state-async issue-id linear-emacs-state team-id))))))))

  ;; Override linear-emacs-sync-org-to-linear to only sync the current issue
  (defun linear-emacs-sync-org-to-linear ()
    "Sync only the current issue to Linear API."
    (interactive)
    (if (eq this-command 'org-todo)
        (my/linear-sync-single-issue-at-point)
      ;; For manual sync, use the original function from linear-emacs
      (linear-emacs-sync-current-heading-to-linear)))

  ;; Run linear-emacs-list-issues before org-todo-list to include Linear issues
  (defun my/run-linear-emacs-list-issues-before-todo (&rest _)
    "Run linear-emacs-list-issues before org-todo-list to include Linear issues."
    (when (fboundp 'linear-emacs-list-issues)
      (message "Updating Linear issues before showing todo list...")
      (condition-case err
          (linear-emacs-list-issues)
        (error (message "Error updating Linear issues: %s" (error-message-string err))))))

  ;; Add advice to org-todo-list, but make it optional
  (defvar my/auto-sync-linear-before-todo t
    "Whether to automatically sync Linear issues before showing todo list.")

  (defun my/toggle-linear-auto-sync ()
    "Toggle automatic syncing of Linear issues before todo list."
    (interactive)
    (setq my/auto-sync-linear-before-todo (not my/auto-sync-linear-before-todo))
    (if my/auto-sync-linear-before-todo
        (progn
          (advice-add 'org-todo-list :before #'my/run-linear-emacs-list-issues-before-todo)
          (message "Linear auto-sync before todo list enabled"))
      (advice-remove 'org-todo-list #'my/run-linear-emacs-list-issues-before-todo)
      (message "Linear auto-sync before todo list disabled")))
  
  ;; Enable Linear auto-sync by default
  (when my/auto-sync-linear-before-todo
    (advice-add 'org-todo-list :before #'my/run-linear-emacs-list-issues-before-todo))

  ;; Automatically enable two-way sync when linear.org is opened
  (defun my/enable-linear-org-sync ()
    "Enable Linear-org synchronization when linear.org is opened."
    (when (and buffer-file-name
               (string-match-p "linear\\.org$" buffer-file-name))
      (when (fboundp 'linear-emacs-enable-org-sync)
        (linear-emacs-enable-org-sync)
        (message "Linear-org synchronization enabled for this buffer"))))

  ;; Add hook to auto-enable sync when linear.org is opened
  (add-hook 'find-file-hook #'my/enable-linear-org-sync)

  ;; Enable sync for org-after-todo-state-change-hook globally
  (add-hook 'org-after-todo-state-change-hook
            (lambda ()
              (when (and buffer-file-name
                         (string-match-p "linear\\.org$" buffer-file-name)
                         (fboundp 'linear-emacs-sync-org-to-linear))
                (linear-emacs-sync-org-to-linear))))

  ;; Auto-save linear.org and inbox.org when todo state changes
  (defun my/auto-save-org-files-on-todo-change ()
    "Automatically save linear.org and inbox.org when todo states change."
    (when (and buffer-file-name
               (or (string-match-p "linear\\.org$" buffer-file-name)
                   (string-match-p "inbox\\.org$" buffer-file-name)))
      (save-buffer)
      (message "Auto-saved %s after todo state change" (file-name-nondirectory buffer-file-name))))

  ;; Add hook to auto-save on todo state changes
  (add-hook 'org-after-todo-state-change-hook #'my/auto-save-org-files-on-todo-change)

  ;; Add convenient keybinding for manually syncing all issues
  (map! :leader
        :prefix "l"
        :desc "Sync all Linear issues" "s" #'linear-emacs-list-issues
        :desc "Toggle Linear auto-sync" "t" #'my/toggle-linear-auto-sync
        :desc "Create new Linear issue" "n" #'linear-emacs-new-issue
        :desc "Test Linear connection" "c" #'linear-emacs-test-connection
        :desc "Check Linear setup" "C" #'linear-emacs-check-setup
        :desc "Toggle Linear debug mode" "d" #'linear-emacs-toggle-debug))


;;; =========================================================================
;;; CLAUDEMACS
;;; =========================================================================

;;
;; font insanity for Claudemacs
;;
(defun my/setup-custom-font-fallbacks-mac ()
  (interactive)
  "Configure font fallbacks on mac for symbols and emojis.
This will need to be called every time you change your font size,
to load the new symbol and emoji fonts."

  (setq use-default-font-for-symbols nil)

  ;; --- Configure for 'symbol' script ---
  ;; We add fonts one by one. Since we use 'prepend',
  ;; the last one added here will be the first one Emacs tries.
  ;; So, list them in reverse order of your preference.

  ;; Least preferred among this list for symbols (will be at the end of our preferred list)
  (set-fontset-font t 'symbol "Hiragino Sans" nil 'prepend)
  (set-fontset-font t 'symbol "STIX Two Math" nil 'prepend)
  (set-fontset-font t 'symbol "Zapf Dingbats" nil 'prepend)
  (set-fontset-font t 'symbol "Monaco" nil 'prepend)
  (set-fontset-font t 'symbol "Menlo" nil 'prepend)
  ;; Most preferred for symbols -- use your main font here
  (set-fontset-font t 'symbol "JetBrainsMono Nerd Font Mono" nil 'prepend)


  ;; --- Configure for 'emoji' script ---
  ;; Add fonts one by one, in reverse order of preference.

  ;; Least preferred among this list for emojis
  (set-fontset-font t 'emoji "Hiragino Sans" nil 'prepend)
  (set-fontset-font t 'emoji "STIX Two Math" nil 'prepend)
  (set-fontset-font t 'emoji "Zapf Dingbats" nil 'prepend)
  (set-fontset-font t 'emoji "Monaco" nil 'prepend)
  (set-fontset-font t 'emoji "Menlo" nil 'prepend)
  ;; (set-fontset-font t 'emoji "Noto Emoji" nil 'prepend) ;; If you install Noto Emoji
  ;; Most preferred for emojis -- use your main font here
  (set-fontset-font t 'emoji "JetBrainsMono Nerd Font Mono" nil 'prepend))

;; to test if you have a font family installed:
                                        ;   (find-font (font-spec :family "Menlo"))

;; Then, add the fonts after your setup is complete:
(add-hook 'emacs-startup-hook
          (lambda ()
            (progn
              (when (string-equal system-type "darwin")
                (my/setup-custom-font-fallbacks-mac)))))

;; Set a big buffer so we can search our history.
(with-eval-after-load 'eat
  (setq eat-term-scrollback-size 400000))

;; Configure eat popup
(set-popup-rules!
  '(("^\\*eat\\*" :side bottom :size 0.4 :select t :quit nil :ttl nil)
    ("^\\*eat:.+\\*$" :side bottom :size 0.4 :select t :quit nil :ttl nil)))

;; Wrapper function to ensure eat opens in popup
(defun my/eat-popup ()
  "Open eat terminal in a popup window."
  (interactive)
  (eat))

(use-package! claudemacs)
;; (after! claudemacs
(require 'claudemacs)
(define-key prog-mode-map (kbd "C-c C-e") #'claudemacs-transient-menu)
(define-key emacs-lisp-mode-map (kbd "C-c C-e") #'claudemacs-transient-menu)
(define-key text-mode-map (kbd "C-c C-e") #'claudemacs-transient-menu)


;;; =========================================================================
;;; SSH
;;; =========================================================================

(setq tramp-default-method "ssh")
(setq tramp-ssh-controlmaster-options
      "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")
(after! tramp-sh
  (setq tramp-use-connection-share nil
        tramp-chunksize 2000))

(after! tramp
  ;; Use bash for TRAMP connections
  (setenv "SHELL" "/bin/bash")

  ;; More comprehensive prompt pattern
  (setq tramp-shell-prompt-pattern
        "\\(?:^\\|\n\\|\x0d\\)[^]#$%>\n]*#?[]#$%>] *\\(\e\\[[0-9;]*[a-zA-Z] *\\)*")

  ;; Additional TRAMP optimizations
  (setq tramp-default-method "ssh"
        tramp-copy-size-limit nil
        tramp-use-connection-share nil
        tramp-verbose 6  ;; Enable debugging
        tramp-connection-timeout 10  ;; Set connection timeout
        password-cache-expiry nil  ;; Keep passwords in cache
        tramp-completion-reread-directory-timeout nil)  ;; Disable auto-refresh

  ;; Ensure clean remote environment
  (setq tramp-remote-shell "/bin/bash"
        tramp-remote-shell-args '("-c"))
  
  ;; Disable version control for TRAMP files (can cause freezing)
  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp
                tramp-file-name-regexp)))
(setq delete-by-moving-to-trash "~/.Trash/" )
