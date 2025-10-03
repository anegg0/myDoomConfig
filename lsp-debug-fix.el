;;; lsp-debug-fix.el -*- lexical-binding: t; -*-
;;; Comprehensive LSP debugging and fixing for Doom Emacs
;;; 
;;; This file helps diagnose and fix LSP server crashes

;;; =========================================================================
;;; DIAGNOSTIC FUNCTIONS
;;; =========================================================================

(defun my/lsp-diagnose-current-server ()
  "Diagnose which LSP server is failing in the current buffer."
  (interactive)
  (cond
   ;; Check if using lsp-mode
   ((bound-and-true-p lsp-mode)
    (if (lsp-workspaces)
        (let* ((workspace (car (lsp-workspaces)))
               (server (when workspace (lsp--workspace-client workspace))))
          (if server
              (message "LSP Mode - Server: %s, Status: %s"
                       (lsp--client-server-id server)
                       (if (lsp--workspace-shutdown-action workspace)
                           "Shutdown requested"
                         "Running"))
            (message "LSP Mode - No server information available")))
      (message "LSP Mode - No active workspace")))
   
   ;; Check if using eglot
   ((bound-and-true-p eglot--managed-mode)
    (let ((server (eglot-current-server)))
      (if server
          (message "Eglot - Server: %s, Status: %s"
                   (eglot--project-nickname server)
                   (if (jsonrpc-running-p server) "Running" "Dead"))
        (message "Eglot - No active server"))))
   
   (t (message "No LSP client is active in this buffer"))))

(defun my/lsp-check-all-servers ()
  "Check status of all LSP servers across all buffers."
  (interactive)
  (let ((lsp-buffers '())
        (eglot-buffers '()))
    
    ;; Check all buffers
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (cond
         ((bound-and-true-p lsp-mode)
          (push (cons buf (lsp-workspaces)) lsp-buffers))
         ((bound-and-true-p eglot--managed-mode)
          (push (cons buf (eglot-current-server)) eglot-buffers)))))
    
    ;; Report findings
    (with-current-buffer (get-buffer-create "*LSP Status Report*")
      (erase-buffer)
      (insert "=== LSP Status Report ===\n\n")
      
      ;; LSP Mode servers
      (insert "LSP Mode Servers:\n")
      (if lsp-buffers
          (dolist (entry lsp-buffers)
            (insert (format "  Buffer: %s\n" (buffer-name (car entry))))
            (if (cdr entry)
                (dolist (workspace (cdr entry))
                  (let ((client (lsp--workspace-client workspace)))
                    (insert (format "    - Server: %s\n" 
                                    (lsp--client-server-id client)))))
              (insert "    - No workspace\n")))
        (insert "  None found\n"))
      
      (insert "\n")
      
      ;; Eglot servers
      (insert "Eglot Servers:\n")
      (if eglot-buffers
          (dolist (entry eglot-buffers)
            (insert (format "  Buffer: %s\n" (buffer-name (car entry))))
            (if (cdr entry)
                (insert (format "    - Server: %s (%s)\n"
                                (eglot--project-nickname (cdr entry))
                                (if (jsonrpc-running-p (cdr entry))
                                    "Running" "Dead")))
              (insert "    - No server\n")))
        (insert "  None found\n"))
      
      (display-buffer (current-buffer)))))

;;; =========================================================================
;;; FIX FUNCTIONS
;;; =========================================================================

(defun my/lsp-restart-server ()
  "Restart the LSP server for the current buffer."
  (interactive)
  (cond
   ;; For lsp-mode
   ((bound-and-true-p lsp-mode)
    (message "Restarting LSP Mode server...")
    (lsp-restart-workspace))
   
   ;; For eglot
   ((bound-and-true-p eglot--managed-mode)
    (message "Restarting Eglot server...")
    (call-interactively #'eglot-reconnect))
   
   (t (message "No LSP client is active in this buffer"))))

(defun my/lsp-kill-all-servers ()
  "Kill all LSP servers (both lsp-mode and eglot)."
  (interactive)
  (when (y-or-n-p "Kill all LSP servers? ")
    ;; Kill lsp-mode servers
    (when (fboundp 'lsp-shutdown-workspace)
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (when (bound-and-true-p lsp-mode)
            (lsp-shutdown-workspace)))))
    
    ;; Kill eglot servers
    (when (fboundp 'eglot-shutdown-all)
      (eglot-shutdown-all))
    
    (message "All LSP servers have been killed")))

(defun my/lsp-switch-to-eglot ()
  "Switch current buffer from lsp-mode to eglot."
  (interactive)
  (when (bound-and-true-p lsp-mode)
    (lsp-mode -1))
  (eglot-ensure)
  (message "Switched to Eglot"))

(defun my/lsp-switch-to-lsp-mode ()
  "Switch current buffer from eglot to lsp-mode."
  (interactive)
  (when (bound-and-true-p eglot--managed-mode)
    (eglot-shutdown (eglot-current-server)))
  (lsp)
  (message "Switched to LSP Mode"))

;;; =========================================================================
;;; LANGUAGE-SPECIFIC FIXES
;;; =========================================================================

(defun my/fix-rust-analyzer ()
  "Fix common rust-analyzer issues."
  (interactive)
  (when (derived-mode-p 'rust-mode 'rust-ts-mode)
    ;; Ensure cargo.toml exists
    (let ((cargo-file (locate-dominating-file default-directory "Cargo.toml")))
      (if cargo-file
          (progn
            ;; Clear rust-analyzer cache
            (shell-command "rm -rf target/.rustc_info.json")
            ;; Restart server
            (my/lsp-restart-server)
            (message "Rust-analyzer restarted with cache cleared"))
        (message "No Cargo.toml found - rust-analyzer needs a Rust project")))))

(defun my/fix-typescript-lsp ()
  "Comprehensive fix for TypeScript LSP issues."
  (interactive)
  (when (or (derived-mode-p 'typescript-mode 'typescript-tsx-mode)
            (derived-mode-p 'js-mode 'js2-mode 'rjsx-mode 'web-mode))
    (let ((project-root (or (locate-dominating-file default-directory "tsconfig.json")
                           (locate-dominating-file default-directory "package.json")
                           (locate-dominating-file default-directory ".git"))))
      (if project-root
          (progn
            (message "Fixing TypeScript LSP for project at %s" project-root)

            ;; Step 1: Clear TypeScript server cache
            (let ((cache-dirs '(".tsbuildinfo" "node_modules/.cache" ".eslintcache")))
              (dolist (dir cache-dirs)
                (let ((cache-path (expand-file-name dir project-root)))
                  (when (file-exists-p cache-path)
                    (delete-directory cache-path t)
                    (message "Cleared cache: %s" cache-path)))))

            ;; Step 2: Check and update TypeScript language server
            (when (y-or-n-p "Check/update TypeScript language server? ")
              (let ((volta-installed (executable-find "volta")))
                (if volta-installed
                    (shell-command "volta install typescript-language-server typescript")
                  (shell-command "npm install -g typescript-language-server typescript"))))

            ;; Step 3: Kill any existing TypeScript server processes
            (shell-command "pkill -f tsserver || true")
            (shell-command "pkill -f 'typescript-language-server' || true")
            (sit-for 1)  ; Wait for processes to die

            ;; Step 4: Restart LSP
            (my/lsp-restart-server)

            (message "TypeScript LSP fix completed"))
        (message "No TypeScript project found - looking for tsconfig.json or package.json")))))

(defun my/fix-python-lsp ()
  "Fix common Python LSP issues."
  (interactive)
  (when (derived-mode-p 'python-mode 'python-ts-mode)
    ;; Check which Python LSP server to use
    (let ((server (completing-read "Which Python LSP server? "
                                   '("pyright" "pylsp" "jedi-language-server"))))
      (pcase server
        ("pyright"
         (shell-command "npm install -g pyright")
         (setq lsp-pyright-use-library-code-for-types t))
        ("pylsp"
         (shell-command "pip install --user python-lsp-server[all]"))
        ("jedi-language-server"
         (shell-command "pip install --user jedi-language-server")))
      (my/lsp-restart-server)
      (message "%s installed/updated and LSP restarted" server))))

(defun my/typescript-lsp-debug ()
  "Debug TypeScript LSP server issues with detailed diagnostics."
  (interactive)
  (let ((report-buffer (get-buffer-create "*TypeScript LSP Debug*")))
    (with-current-buffer report-buffer
      (erase-buffer)
      (insert "=== TypeScript LSP Debug Report ===\n")
      (insert (format-time-string "Generated at: %Y-%m-%d %H:%M:%S\n\n"))

      ;; Check file type
      (insert (format "Current mode: %s\n" major-mode))
      (insert (format "File: %s\n\n" (or buffer-file-name "No file")))

      ;; Check TypeScript installation
      (insert "TypeScript Installation:\n")
      (insert (format "  typescript-language-server: %s\n"
                      (or (executable-find "typescript-language-server") "NOT FOUND")))
      (insert (format "  tsserver: %s\n"
                      (or (executable-find "tsserver") "NOT FOUND")))
      (insert (format "  typescript: %s\n\n"
                      (or (executable-find "tsc") "NOT FOUND")))

      ;; Check project configuration
      (insert "Project Configuration:\n")
      (let ((tsconfig (locate-dominating-file default-directory "tsconfig.json"))
            (package-json (locate-dominating-file default-directory "package.json")))
        (insert (format "  tsconfig.json: %s\n"
                        (if tsconfig (expand-file-name "tsconfig.json" tsconfig) "NOT FOUND")))
        (insert (format "  package.json: %s\n\n"
                        (if package-json (expand-file-name "package.json" package-json) "NOT FOUND"))))

      ;; Check LSP status
      (insert "LSP Status:\n")
      (cond
       ((bound-and-true-p eglot--managed-mode)
        (let ((server (eglot-current-server)))
          (if server
              (insert (format "  Eglot server: %s (%s)\n"
                              (eglot--project-nickname server)
                              (if (jsonrpc-running-p server) "Running" "Dead")))
            (insert "  No Eglot server active\n"))))
       ((bound-and-true-p lsp-mode)
        (if (lsp-workspaces)
            (dolist (workspace (lsp-workspaces))
              (let ((client (lsp--workspace-client workspace)))
                (insert (format "  LSP Mode server: %s\n"
                                (lsp--client-server-id client)))))
          (insert "  No LSP Mode workspace active\n")))
       (t (insert "  No LSP client active\n")))

      ;; Check running processes
      (insert "\nRunning TypeScript processes:\n")
      (let ((processes (shell-command-to-string "ps aux | grep -E '(tsserver|typescript-language-server)' | grep -v grep")))
        (if (string-empty-p processes)
            (insert "  None found\n")
          (insert processes)))

      (display-buffer report-buffer))))

(defun my/typescript-lsp-monitor ()
  "Monitor TypeScript LSP server in real-time."
  (interactive)
  (when (bound-and-true-p eglot--managed-mode)
    ;; Enable Eglot event logging temporarily
    (setq eglot-events-buffer-size 100000)
    (eglot-events-buffer (eglot-current-server))
    (message "Monitoring Eglot events - check *EGLOT events* buffer"))
  (when (bound-and-true-p lsp-mode)
    (setq lsp-log-io t)
    (lsp-workspace-show-log)
    (message "Monitoring LSP Mode - check *lsp-log* buffer")))

;;; =========================================================================
;;; ENHANCED CONFIGURATION
;;; =========================================================================

(defun my/lsp-safe-config ()
  "Apply safe LSP configuration to prevent crashes."
  (interactive)

  ;; For Eglot (since you're using it as per your config)
  (after! eglot
    ;; Increase timeout for slow servers
    (setq eglot-sync-connect 3
          eglot-connect-timeout 30
          eglot-autoshutdown t
          eglot-send-changes-idle-time 0.5)

    ;; Better error handling
    (setq eglot-events-buffer-size 0)  ; Disable events buffer for performance

    ;; TypeScript-specific Eglot configuration
    (add-to-list 'eglot-server-programs
                 '((typescript-mode typescript-tsx-mode js-mode js2-mode rjsx-mode)
                   . ("typescript-language-server" "--stdio"
                      :initializationOptions
                      (:preferences
                       (:includeInlayParameterNameHints "none"
                        :includeInlayParameterNameHintsWhenArgumentMatchesName nil
                        :includeInlayFunctionParameterTypeHints nil
                        :includeInlayVariableTypeHints nil
                        :includeInlayPropertyDeclarationTypeHints nil
                        :includeInlayFunctionLikeReturnTypeHints nil
                        :includeInlayEnumMemberValueHints nil)
                       :typescript
                       (:inlayHints
                        (:includeInlayParameterNameHints "none"
                         :includeInlayParameterNameHintsWhenArgumentMatchesName nil
                         :includeInlayFunctionParameterTypeHints nil
                         :includeInlayVariableTypeHints nil
                         :includeInlayPropertyDeclarationTypeHints nil
                         :includeInlayFunctionLikeReturnTypeHints nil
                         :includeInlayEnumMemberValueHints nil)
                        :suggest
                        (:autoImports t
                         :includeCompletionsForModuleExports t)
                        :format
                        (:enable t))))))

    ;; Add restart advice for automatic recovery
    (defadvice! my/eglot-auto-restart-on-crash (orig-fn &rest args)
      :around #'eglot--server-capable
      (condition-case err
          (apply orig-fn args)
        (jsonrpc-error
         (message "Eglot server crashed, attempting restart...")
         (eglot-reconnect)
         nil))))

  ;; For LSP Mode (in case you switch)
  (after! lsp-mode
    ;; Increase timeouts
    (setq lsp-response-timeout 30
          lsp-idle-delay 0.5
          lsp-log-io nil  ; Disable logging for performance
          lsp-enable-file-watchers nil  ; Reduce resource usage
          lsp-enable-folding nil
          lsp-enable-text-document-color nil
          lsp-enable-on-type-formatting nil)

    ;; TypeScript-specific LSP Mode settings
    (setq lsp-typescript-suggest-auto-imports t
          lsp-typescript-preferences-include-inlay-parameter-name-hints "none"
          lsp-typescript-preferences-include-inlay-parameter-name-hints-when-argument-matches-name nil
          lsp-typescript-preferences-include-inlay-function-parameter-type-hints nil
          lsp-typescript-preferences-include-inlay-variable-type-hints nil
          lsp-typescript-preferences-include-inlay-property-declaration-type-hints nil
          lsp-typescript-preferences-include-inlay-function-like-return-type-hints nil
          lsp-typescript-preferences-include-inlay-enum-member-value-hints nil
          lsp-javascript-suggest-auto-imports t)

    ;; Better error recovery
    (setq lsp-restart 'auto-restart
          lsp-keep-workspace-alive nil)

    ;; Limit features to reduce crashes
    (setq lsp-lens-enable nil
          lsp-headerline-breadcrumb-enable nil
          lsp-modeline-diagnostics-enable nil))

  (message "Safe LSP configuration applied"))

;;; =========================================================================
;;; KEYBINDINGS
;;; =========================================================================

;; Keybindings removed - call functions directly with M-x

;;; =========================================================================
;;; TYPESCRIPT-SPECIFIC HOOKS
;;; =========================================================================

(defun my/typescript-mode-lsp-setup ()
  "Setup LSP for TypeScript with crash prevention."
  ;; Ensure we're using Eglot (as configured in your setup)
  (when (and (featurep 'eglot)
             (not (bound-and-true-p eglot--managed-mode)))
    (eglot-ensure))

  ;; Apply TypeScript-specific workarounds
  (setq-local eglot-stay-out-of '(company))  ; Don't let eglot manage company
  (setq-local eglot-ignored-server-capabilities '(:documentHighlightProvider)) ; Disable problematic features

  ;; Set buffer-local completion settings if cape is available
  (when (fboundp 'cape-capf-super)
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'eglot-completion-at-point
                       #'cape-file)))))

;; Apply to all TypeScript/JavaScript modes
(dolist (hook '(typescript-mode-hook
                typescript-tsx-mode-hook
                js-mode-hook
                js2-mode-hook
                rjsx-mode-hook
                web-mode-hook))
  (add-hook hook #'my/typescript-mode-lsp-setup))

;;; =========================================================================
;;; AUTO-RECOVERY SYSTEM
;;; =========================================================================

(defvar my/lsp-crash-count 0
  "Counter for LSP crashes to prevent infinite restart loops.")

(defvar my/lsp-last-crash-time 0
  "Time of last LSP crash.")

(defun my/lsp-auto-recover ()
  "Automatically recover from LSP crashes with rate limiting."
  (let ((current-time (float-time)))
    ;; Reset counter if it's been more than 5 minutes since last crash
    (when (> (- current-time my/lsp-last-crash-time) 300)
      (setq my/lsp-crash-count 0))

    (setq my/lsp-last-crash-time current-time)
    (setq my/lsp-crash-count (1+ my/lsp-crash-count))

    ;; Only auto-restart if we haven't crashed too many times
    (if (< my/lsp-crash-count 3)
        (progn
          (message "LSP crashed (attempt %d/3), auto-recovering..." my/lsp-crash-count)
          (sit-for 2)  ; Wait 2 seconds before restart
          (my/lsp-restart-server))
      (message "LSP crashed too many times. Use SPC d l r to manually restart."))))

;; Hook into Eglot's error handling
(after! eglot
  (defadvice! my/eglot-handle-crash (&rest _)
    :after #'eglot-shutdown
    "Auto-recover from Eglot crashes."
    (when (and (not (eglot-current-server))
               (or (derived-mode-p 'typescript-mode 'typescript-tsx-mode)
                   (derived-mode-p 'js-mode 'js2-mode 'rjsx-mode)))
      (my/lsp-auto-recover))))

;;; =========================================================================
;;; AUTO-LOAD
;;; =========================================================================

;; Automatically apply safe configuration
(my/lsp-safe-config)

(provide 'lsp-debug-fix)