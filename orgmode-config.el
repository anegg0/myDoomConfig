;;; orgmode-config.el -*- lexical-binding: t; -*-

;;; =========================================================================
;;; ORG MODE CONFIGURATION
;;; =========================================================================

;; Base settings
(setq org-directory "/Users/allup/Library/CloudStorage/ProtonDrive-gael.blanchemain@protonmail.com-folder"
      load-prefer-newer t
      search-highlight t
      search-whitespace-regexp ".*?"
      org-ellipsis " ▼ "
      org-adapt-indentation nil
      org-habit-show-habits-only-for-today t)

;; Make sure org directory subdirectories exist
(dolist (dir '("main" "daily" "reference" "articles"))
  (make-directory (expand-file-name dir org-directory) t))

;; Clear any existing org-agenda-files setting
(setq org-agenda-files nil)

;; Prevent custom.el from saving wrong agenda files
(defun my/fix-org-agenda-files-in-custom (&rest _)
  "Prevent custom.el from saving the wrong org-agenda-files."
  (when (boundp 'org-agenda-files)
    (put 'org-agenda-files 'standard-value
         `((quote ,(mapcar (lambda (dir)
                             (expand-file-name dir org-directory))
                           '("main" "daily" "reference" "articles")))))))

;; Add our advice to custom-save-all
(advice-add 'custom-save-all :before #'my/fix-org-agenda-files-in-custom)

;; Define function to set agenda files only if directories exist
(defun my/set-org-agenda-files ()
  "Set org-agenda-files to directories that actually exist."
  (let ((dirs (mapcar (lambda (dir)
                        (expand-file-name dir org-directory))
                      '("main" "daily" "reference" "articles"))))
    (setq org-agenda-files
          (cl-remove-if-not #'file-exists-p dirs))))

;; Set org-agenda-files after org has loaded
(after! org
  ;; Set agenda files now
  (my/set-org-agenda-files)

  ;; Force reload agenda files
  (org-agenda-file-to-front nil)

  ;; Custom TODO keywords
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(i)" "IN-REVIEW(r)" "|" "BACKLOG(b)" "BLOCKED(l)" "DONE(d)" "CANCELED(c)" "DUPLICATE(p)")))

  ;; Make agenda always include todo items
  (setq org-agenda-todo-list-sublevels t)

  ;; Optional: Add custom faces for your TODO states
  (setq org-todo-keyword-faces
        '(("TODO" . (:foreground "red" :weight bold))
          ("IN-PROGRESS" . (:foreground "blue" :weight bold))
          ("IN-REVIEW" . (:foreground "orange" :weight bold))
          ("BACKLOG" . (:foreground "green" :weight bold))
          ("BLOCKED" . (:foreground "green" :weight bold))
          ("DONE" . (:foreground "green" :weight bold))
          ("CANCELLED" . (:foreground "gray" :weight bold))
          ("DUPLICATE" . (:foreground "gray" :weight bold))))

  ;; Enable refile targets to include agenda files
  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))

  ;; Configure org-mode for inline images
  (setq org-startup-with-inline-images t)  ; Show inline images when opening org files
  (setq org-image-actual-width nil)        ; Use image size specifications in org files
  (add-hook 'org-mode-hook #'org-display-inline-images)) ; Auto-display images in org buffers

;;; =========================================================================
;;; ORG-ROAM CONFIGURATION
;;; =========================================================================

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
  (setq org-roam-directory (file-truename "/Users/allup/Library/CloudStorage/ProtonDrive-gael.blanchemain@protonmail.com-folder")
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

;; Make sure Org-roam uses the same TODO keywords
(after! org-roam
  ;; Add this line to ensure org-roam uses your custom TODO states
  (setq org-roam-todo-keywords org-todo-keywords))

;; Org-roam dailies
(setq org-roam-dailies-directory "daily/")
(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %?"
         :target (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n"))))

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

;;; =========================================================================
;;; CITAR AND BIBLIOGRAPHY MANAGEMENT
;;; =========================================================================

;; Citar for bibliography management
(require 'citar)
(use-package citar
  :no-require
  :custom
  (org-cite-global-bibliography '("/Users/allup/Library/CloudStorage/ProtonDrive-gael.blanchemain@protonmail.com-folder"))
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

;;; =========================================================================
;;; DEFT CONFIGURATION FOR QUICK NOTE ACCESS
;;; =========================================================================

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

;;; =========================================================================
;;; LINEAR.APP ORG INTEGRATION
;;; =========================================================================

;; Set the location for Linear org file
(setq linear-org-file (expand-file-name "main/linear.org" org-directory))

;; Mapping between Linear states and org TODO keywords
(setq linear-org-state-mapping
      '(("Todo" . "TODO")
        ("In Progress" . "IN-PROGRESS")
        ("In Review" . "IN-REVIEW")
        ("Backlog" . "BACKLOG")
        ("Blocked" . "BLOCKED")
        ("Done" . "DONE")
        ("Canceled" . "CANCELED")
        ("Duplicate" . "DUPLICATE")))

;; Mapping between Linear priority values and org priorities
(setq linear-org-priority-mapping
      '((0 . nil)   ; No priority
        (1 . "A")   ; Urgent
        (2 . "B")   ; High
        (3 . "C")   ; Medium
        (4 . "D"))) ; Low

;; Property names for storing Linear metadata
(setq linear-org-issue-id-property "LINEAR_ID"
      linear-org-team-id-property "LINEAR_TEAM"
      linear-org-modified-property "LINEAR_MODIFIED"
      linear-org-url-property "LINEAR_URL")

;; Update org-capture-templates for Linear tasks
(after! org
  (add-to-list 'org-capture-templates
               '("L" "Linear Task" entry
                 (file+headline linear-org-file "OCL")
                 "** TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n"
                 :immediate-finish nil
                 :jump-to-captured t
                 :after-finalize (lambda ()
                                   (with-current-buffer (find-buffer-visiting linear-org-file)
                                     (save-excursion
                                       (goto-char (point-max))
                                       (org-back-to-heading t)
                                       (call-interactively 'linear-org-capture-to-linear)))))))

;;; =========================================================================
;;; DITAA CONFIGURATION
;;; =========================================================================

;; Ditaa configuration
(setq org-ditaa-jar-path "/opt/homebrew/bin/ditaa.jar")

;; Provide the module
(provide 'orgmode-config)
