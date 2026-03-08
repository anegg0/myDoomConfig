;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq doom-theme 'leuven-dark)
(setq display-line-numbers-type t)


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

(map! :leader
      :desc "Filter agenda interactive"
      "o f" #'my/org-agenda-filter-interactive)

(map! :leader
      :desc "Sorted org-todo-list"
      "o t" #'my/org-sorted-todo-list)

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
(add-to-list 'default-frame-alist '(alpha 95 95))

;; Set initial frame size and position
(setq initial-frame-alist
      (append initial-frame-alist
              '((top . 1) (left . 1) (width . 170) (height . 140))))

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

(setq org-directory "~/Nextcloud/orgmode" )

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
        '((sequence "TODO(t)" "NEXT(n)" "IN-PROGRESS(i)" "IN-REVIEW(r)" "BACKLOG(b)" "BLOCKED(l)" "|" "DONE(d)" "CANCELED(c)")))

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
                          (expand-file-name "articles" org-directory)))

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

  (defun my/org-sorted-todo-list ()
    "Display TODO list sorted: TODO first, NEXT second, IN-PROGRESS third,
remaining states (IN-REVIEW, BACKLOG, BLOCKED) split equally."
    (interactive)
    (let* ((remaining-states '("IN-REVIEW" "BACKLOG" "BLOCKED"))
           (state-counts
            (mapcar (lambda (state)
                      (length (org-map-entries
                               (lambda () t)
                               (concat "/" state)
                               'agenda)))
                    remaining-states))
           (total-remaining (apply #'+ state-counts))
           (per-state (max 1 (ceiling total-remaining (length remaining-states))))
           (org-agenda-custom-commands
            `(("Z" "Sorted TODO"
               ((todo "TODO"
                      ((org-agenda-overriding-header "TODO\n")))
                (todo "NEXT"
                      ((org-agenda-overriding-header "NEXT\n")))
                (todo "IN-PROGRESS"
                      ((org-agenda-overriding-header "IN-PROGRESS\n")))
                ,@(mapcar
                   (lambda (state)
                     `(todo ,state
                       ((org-agenda-overriding-header ,(concat state "\n"))
                        (org-agenda-max-entries ,per-state))))
                   remaining-states))))))
      (when (fboundp 'my/run-linear-emacs-list-issues-before-todo)
        (my/run-linear-emacs-list-issues-before-todo))
      (org-agenda nil "Z")))

  ;; Interactive org-agenda filter by tags and TODO state
  (defun my/extract-todo-keywords ()
    "Extract flat list of TODO keywords from org-todo-keywords.
Strips keyboard shortcuts like '(t)' and returns clean strings."
    (let ((keywords '()))
      (dolist (sequence org-todo-keywords)
        (when (listp sequence)
          (dolist (item (cdr sequence))  ; Skip 'sequence symbol
            (when (stringp item)
              (let ((keyword (replace-regexp-in-string "(.*)" "" item)))
                (unless (string-match-p "|" keyword)
                  (push (string-trim keyword) keywords)))))))
      (nreverse keywords)))

  (defun my/completing-read-tag (prompt)
    "Read a tag with completion from all agenda files.
PROMPT is the string displayed to user. Returns empty string if no input."
    (let* ((all-tags (delete-dups
                      (apply #'append
                             (org-map-entries
                              (lambda () (org-get-tags))
                              nil
                              'agenda))))
           (tag (completing-read prompt all-tags nil nil)))
      (if (string-empty-p tag) "" tag)))

  (defun my/completing-read-todo (prompt)
    "Read a TODO state with completion from org-todo-keywords.
PROMPT is the string displayed to user. Requires valid selection."
    (let* ((todo-keywords (my/extract-todo-keywords))
           (state (completing-read prompt todo-keywords nil t)))
      state))

  (defun my/build-skip-function (tag1 tag2 todo-state)
    "Build skip function that combines tag and TODO filtering.
TAG1 and TAG2 are optional tag filters (empty string means skip).
TODO-STATE is required TODO keyword to match.
Returns a lambda suitable for org-agenda-skip-function."
    (lambda ()
      (let ((should-skip nil))
        ;; First check TODO state (skip if doesn't match)
        (when (and todo-state (not (string-empty-p todo-state)))
          (setq should-skip
                (org-agenda-skip-entry-if 'nottodo (list todo-state))))

        ;; Then check tags (skip if doesn't have required tags)
        (unless should-skip
          (let ((entry-tags (org-get-tags nil t)))
            ;; Check first tag
            (when (and tag1 (not (string-empty-p tag1)))
              (unless (member tag1 entry-tags)
                (setq should-skip (save-excursion
                                    (or (outline-next-heading)
                                        (point-max))))))
            ;; Check second tag (only if first tag passed)
            (when (and tag2 (not (string-empty-p tag2)) (not should-skip))
              (unless (member tag2 entry-tags)
                (setq should-skip (save-excursion
                                    (or (outline-next-heading)
                                        (point-max))))))))
        should-skip)))

  (defun my/org-agenda-filter-interactive ()
    "Interactively filter org-agenda by tags and TODO status.
Prompts for:
  1. First tag (optional)
  2. Second tag (optional)
  3. TODO state (required)
Displays agenda entries matching ALL criteria (AND logic)."
    (interactive)
    (let* ((tag1 (my/completing-read-tag "First tag (optional): "))
           (tag2 (my/completing-read-tag "Second tag (optional): "))
           (todo-state (my/completing-read-todo "TODO state: "))
           (org-agenda-skip-function
            (my/build-skip-function tag1 tag2 todo-state)))
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
  (setq org-roam-db-location (expand-file-name "org-roam.db" doom-cache-dir)
        org-roam-directory (file-truename "~/Nextcloud/orgmode/")
        ;; Exclude GTD task files: these are managed by linear-emacs/org-agenda,
        ;; not org-roam. Without this, autosync watches linear.org and conflict
        ;; copies, writing back org-id properties and triggering Nextcloud
        ;; re-sync in a feedback loop.
        org-roam-file-exclude-regexp "gtd/"
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
          ("f" "fin" plain
           "%?"
           :if-new (file+head "fin/${slug}.org"
                              "#+title: ${title}\n#+TAGS: :\n#+FILETAGS: :fin:\n")
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

(auto-save-visited-mode +1)

;; Disable in-buffer text completion globally
(after! corfu
  (global-corfu-mode -1)
  ;; Ensure corfu is disabled by default in all buffers
  (setq global-corfu-modes nil))

(after! company
  (global-company-mode -1))

;; French accent menu for repeated key presses
;; Thanks to Alvaro Ramirez's solution for accentuated characters on Emacs/MacOS: https://xenodium.com/an-accentuated-emacs-experiment/
(use-package accent
  :ensure t
  :init
  :config
  ;; French accent configuration
  (setq accent-diacritics '((a (à â))
                            (e (é è ê ë))
                            (i (î ï))
                            (o (ô))
                            (u (ù û ü))
                            (c (ç))
                            (A (À Â))
                            (E (É È Ê Ë))
                            (I (Î Ï))
                            (O (Ô))
                            (U (Ù Û Ü))
                            (C (Ç))))
  (defvar accent-menu-monitor--last-edit-time nil)
  
  (defun accent-menu-reset-timer ()
    "Reset the accent menu timer when exiting insert/replace state."
    (setq accent-menu-monitor--last-edit-time nil))

  (define-minor-mode accent-menu-mode
    "Toggle `accent-menu' if repeated keys are detected."
    :lighter " accent-menu mode"
    (if accent-menu-mode
        (progn
          (remove-hook 'after-change-functions #'accent-menu-monitor--text-change t)
          (add-hook 'after-change-functions #'accent-menu-monitor--text-change 0 t)
          ;; Add evil-mode specific hooks if evil is available
          (when (bound-and-true-p evil-mode)
            (add-hook 'evil-insert-state-exit-hook #'accent-menu-reset-timer nil t)
            (add-hook 'evil-replace-state-exit-hook #'accent-menu-reset-timer nil t)))
      (remove-hook 'after-change-functions #'accent-menu-monitor--text-change t)
      (when (bound-and-true-p evil-mode)
        (remove-hook 'evil-insert-state-exit-hook #'accent-menu-reset-timer t)
        (remove-hook 'evil-replace-state-exit-hook #'accent-menu-reset-timer t))))

  (defun accent-menu-monitor--text-change (beginning end length)
    "Monitors text change BEGINNING, END, and LENGTH."
    (let ((last-edit-time accent-menu-monitor--last-edit-time)
          (edit-time (float-time)))
      (when (and (> end beginning)
                 (eq length 0)
                 last-edit-time
                 (not undo-in-progress)
                 ;; Only trigger in insert or replace states when evil-mode is active
                 (or (not (bound-and-true-p evil-mode))
                     (memq evil-state '(insert replace)))
                 ;; 0.27 seems to work for my macOS keyboard settings.
                 ;; Key Repeat: Fast | Delay Until Repeat: Short.
                 (< (- edit-time last-edit-time) 0.17)
                 (float-time (time-subtract (current-time) edit-time))
                 (accent-menu-monitor--buffer-char-string (1- beginning))
                 (seq-contains-p (mapcar (lambda (item)
                                           (symbol-name (car item)))
                                         accent-diacritics)
                                 (accent-menu-monitor--buffer-char-string beginning))
                 (string-equal (accent-menu-monitor--buffer-char-string (1- beginning))
                               (accent-menu-monitor--buffer-char-string beginning)))
        ;; Temporarily disable org-element cache during accent menu operations
        (let ((org-element-use-cache-backup (when (boundp 'org-element-use-cache)
                                              org-element-use-cache)))
          (when (derived-mode-p 'org-mode)
            (setq-local org-element-use-cache nil))
          ;; Delete the repeated character
          (delete-char -1)
          ;; Show accent menu
          (ignore-error quit
            (accent-menu))
          ;; Restore org-element cache setting
          (when (derived-mode-p 'org-mode)
            (setq-local org-element-use-cache org-element-use-cache-backup))))
      (setq accent-menu-monitor--last-edit-time edit-time)))

  (defun accent-menu-monitor--buffer-char-string (at)
    (when (and (>= at (point-min))
               (< at (point-max)))
      (buffer-substring-no-properties at (+ at 1))))
  
  ;; Manual command to toggle accent menu in current buffer
  (defun my/toggle-accent-menu-buffer ()
    "Toggle accent-menu-mode in current buffer."
    (interactive)
    (accent-menu-mode (if accent-menu-mode -1 1))
    (message "Accent menu %s in %s" 
             (if accent-menu-mode "enabled" "disabled")
             (buffer-name)))) ; End of use-package accent

;; Keybindings for accent-menu-mode
(map! :leader
      :desc "Toggle accent menu (global)"
      "t a" #'my/toggle-accent-menu-global)
(map! :leader
      :desc "Toggle accent menu (buffer)"
      "t A" #'my/toggle-accent-menu-buffer)

;; Project-wide occur function that returns a list of matching buffers:
(defun my/smart-project-occur (regexp)
  "Search project files with occur, only opening files that match."
  (interactive "sRegexp: ")
  (let* ((project-root (projectile-project-root))
         (matching-files (projectile-files-with-string regexp project-root))
         (buffers '()))
    (dolist (file matching-files)
      (push (find-file-noselect file) buffers))
    (if buffers
        (multi-occur buffers regexp)
      (message "No files found containing: %s" regexp))))

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

;; claude-code-emacs — run Claude Code CLI sessions inside Emacs
;; Autoloads register claude-code-run from claude-code-core, so we hook
;; into that feature to pull in the rest (ui, mcp, prompt, commands).
(after! claude-code-core
  (require 'claude-code)
  (defadvice! my/claude-code-run-guard (&rest _)
    :before #'claude-code-run
    (unless (projectile-project-root)
      (user-error "Not in a project — open a file inside a git repo first"))))

(map! :leader
      :prefix ("A" . "claude-code")
      "c" #'claude-code-run
      "t" #'claude-code-transient)

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
  )

;; Disable dired-omit-mode globally
(remove-hook 'dired-mode-hook 'dired-omit-mode)

;; Enable auto-revert for dired buffers globally
(setq dired-auto-revert-buffer t)

;; Enable global auto-revert mode to automatically refresh buffers when files change
(global-auto-revert-mode 1)

;;; ---------------------------------------------------------------------------
;;; Evil-surround customization
;;; ---------------------------------------------------------------------------
;; Remove automatic spacing when surrounding text with opening brackets.
;; Default behavior: { adds "{ text }", } adds "{text}"
;; Modified behavior: BOTH { and } add "{text}" (no spaces)
(after! evil-surround
  ;; Override opening bracket pairs to not add spaces
  (setf (alist-get ?\( evil-surround-pairs-alist) '("(" . ")"))
  (setf (alist-get ?\[ evil-surround-pairs-alist) '("[" . "]"))
  (setf (alist-get ?\{ evil-surround-pairs-alist) '("{" . "}")))

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
        (expand-file-name "~/Nextcloud/orgmode/emacs-everywhere-msgs/"))
  
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
;;; PROJECTILE
;;; =========================================================================
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
  (my/linear-load-api-key-from-auth-source)
  (setq linear-emacs-org-file-path (expand-file-name "gtd/linear.org" org-directory))

  ;; Configure async behavior (linear-emacs now uses async-first architecture)
  (setq linear-emacs-async-default t)       ; Use async API calls by default (non-blocking)
  (setq linear-emacs-progress-messages t)   ; Show progress during long operations
  ;; Improved synchronization function that only updates the changed issue
  (defun my/linear-sync-single-issue-at-point ()
    "Sync only the current issue at point to Linear API."
    (interactive)
    (save-excursion
      ;; Move to the beginning of the current heading
      (org-back-to-heading t)
      ;; Check if this is a Linear issue heading
      (when (looking-at "^\\*\\*\\* \\(TODO\\|IN-PROGRESS\\|IN-REVIEW\\|BACKLOG\\|BLOCKED\\|DONE\\)")
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
            ;; Map org TODO state to Linear state using the package's mapping system
            ;; This respects the linear-emacs-issues-state-mapping customization
            ;; Map org TODO state to Linear state
            (let ((linear-emacs-state (cond
                                       ((string= todo-state "TODO") "Todo")
                                       ((string= todo-state "IN-PROGRESS") "In Progress")
                                       ((string= todo-state "IN-REVIEW") "In Review")
                                       ((string= todo-state "BACKLOG") "Backlog")
                                       ((string= todo-state "BLOCKED") "Blocked")
                                       ((string= todo-state "DONE") "Done")
                                       ((string= todo-state "CANCELED") "Canceled")
                                       ;; Non-Linear states - don't sync to Linear
                                       ((member todo-state '("NEXT")) nil)
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
  ;; NOTE: This function now truly returns immediately and the todo list shows right away
  (defun my/run-linear-emacs-list-issues-before-todo (&rest _)
    "Trigger async Linear issues fetch without blocking org-todo-list.
The Linear issues will update in the background while the todo list displays."
    (when (and (featurep 'linear-emacs)
               (fboundp 'linear-emacs-list-issues))
      (message "Updating Linear issues in background...")
      ;; Run async without blocking - just fire and forget
      (condition-case err
          (run-with-idle-timer 0.1 nil
                               (lambda ()
                                 (when (boundp 'linear-emacs--active-requests)
                                   (linear-emacs-list-issues))))
        (error (message "Error scheduling Linear update: %s" (error-message-string err))))))

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
      (auto-revert-mode 1)
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
      ;; Acknowledge external file changes from async Linear sync to prevent
      ;; "has changed since visited or saved" prompt
      (unless (verify-visited-file-modtime)
        (set-visited-file-modtime))
      (save-buffer)
      (message "Auto-saved %s after todo state change" (file-name-nondirectory buffer-file-name))))

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
;;; SSH
;;; =========================================================================

(setq tramp-default-method "ssh")
;; SSH ControlMaster configuration for better connection reuse
(setq tramp-ssh-controlmaster-options
      "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=yes")
;; Enable connection sharing for better performance
(after! tramp-sh
  (setq tramp-use-connection-share t
        tramp-chunksize 500))  ;; Reduced chunk size for better performance (was 2000)

(after! tramp
  ;; Use bash for TRAMP connections
  (setenv "SHELL" "/bin/bash")

  ;; Improved shell prompt pattern for better compatibility
  (setq tramp-shell-prompt-pattern
        "\\(?:^\\|\n\\|\x0d\\)[^]#$%>\n]*#?[]#$%>] *\\(\e\\[[0-9;]*[a-zA-Z] *\\)*")
  
  ;; Additional prompt patterns for various shells
  (setq tramp-terminal-prompt-regexp "[pP]assword\\|Enter passphrase")

  ;; Additional TRAMP optimizations
  (setq tramp-default-method "ssh"
        tramp-copy-size-limit nil
        tramp-use-connection-share t  ;; Enable connection sharing (consistent with tramp-sh setting)
        tramp-verbose 1  ;; Reduced verbosity (was 6 - too noisy)
        tramp-connection-timeout 15  ;; Increased connection timeout (was 10)
        password-cache-expiry nil  ;; Keep passwords in cache
        tramp-completion-reread-directory-timeout 60)  ;; Re-enabled with reasonable timeout (was nil)

  ;; Ensure clean remote environment
  (setq tramp-remote-shell "/bin/bash"
        tramp-remote-shell-args '("-c"))
  
  ;; Selective version control settings for TRAMP files
  ;; Instead of completely disabling, we optimize for better performance
  (setq vc-handled-backends '(Git)  ;; Only use Git for version control
        vc-git-resolve-symlinks nil  ;; Don't resolve symlinks
        vc-follow-symlinks t)  ;; But follow them when needed
  
  ) ;; Close (after! tramp block

(setq delete-by-moving-to-trash "~/.Trash/" )

