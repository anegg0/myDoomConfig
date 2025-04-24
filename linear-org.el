;;; linear-org.el --- Bidirectional sync between Linear.app and org-mode -*- lexical-binding: t; -*-

;;; Commentary:
;; This package provides bidirectional synchronization between Linear.app tasks
;; and org-mode TODO items.

;;; Code:

(require 'linear)
(require 'org)
(require 'json)

(defgroup linear-org nil
  "Synchronization between Linear and org-mode."
  :group 'org
  :prefix "linear-org-")

(defcustom linear-org-file (expand-file-name "main/linear.org" org-directory)
  "Path to the org file where Linear tasks are stored."
  :type 'file
  :group 'linear-org)

(defcustom linear-org-sync-interval 3600
  "Interval in seconds for automatic synchronization from Linear to org."
  :type 'number
  :group 'linear-org)

(defcustom linear-org-state-mapping
  '(("Todo" . "TODO")
    ("In Progress" . "NEXT")
    ("In Review" . "NEXT")
    ("Backlog" . "HOLD")
    ("Blocked" . "HOLD")
    ("Done" . "DONE")
    ("Canceled" . "Cancelled"))
  "Mapping between Linear states and org TODO keywords."
  :type '(repeat (cons string string))
  :group 'linear-org)

(defvar linear-org-timer nil
  "Timer for automatic synchronization.")

(defvar linear-org-last-sync-time nil
  "Timestamp of the last synchronization.")

(defvar linear-org-issue-id-property "LINEAR_ID"
  "Property name for storing Linear issue IDs in org entries.")

(defvar linear-org-team-id-property "LINEAR_TEAM"
  "Property name for storing Linear team IDs in org entries.")

(defvar linear-org-modified-property "LINEAR_MODIFIED"
  "Property name for storing Linear modification time in org entries.")

(defvar linear-org-url-property "LINEAR_URL"
  "Property name for storing Linear issue URL in org entries.")

(defvar linear-org-priority-mapping
  '((0 . nil)   ; No priority
    (1 . "A")   ; Urgent
    (2 . "B")   ; High
    (3 . "C")   ; Medium
    (4 . "D"))  ; Low
  "Mapping between Linear priority values and org priorities.")

(defun linear-org-api-query-assigned-issues ()
  "Query Linear API for assigned issues."
  (linear--log "Fetching assigned issues for org sync")
  (let* ((query "query {
                  viewer {
                    assignedIssues {
                      nodes {
                        id
                        identifier
                        title
                        description
                        state {
                          id
                          name
                          type
                        }
                        team {
                          id
                          name
                        }
                        priority
                        url
                        updatedAt
                      }
                    }
                  }
                }")
         (response (linear--graphql-request query)))
    (when response
      (cdr (assoc 'nodes (assoc 'assignedIssues (assoc 'viewer (assoc 'data response))))))))

(defun linear-org-linear-to-org-state (linear-state)
  "Convert LINEAR-STATE to org TODO state."
  (or (cdr (assoc linear-state linear-org-state-mapping)) "TODO"))

(defun linear-org-org-to-linear-state (org-state)
  "Convert ORG-STATE to Linear state."
  (car (rassoc org-state linear-org-state-mapping)))

(defun linear-org-linear-to-org-priority (priority)
  "Convert LINEAR numeric PRIORITY to org priority."
  (cdr (assoc priority linear-org-priority-mapping)))

(defun linear-org-ensure-linear-file ()
  "Ensure the Linear org file exists with proper structure."
  (unless (file-exists-p linear-org-file)
    (with-temp-file linear-org-file
      (insert "#+TITLE: Linear Tasks\n")
      (insert "#+FILETAGS: :linear:\n")
      (insert "#+TODO: TODO NEXT HOLD | DONE Cancelled\n\n"))))

(defun linear-org-find-issue-heading (issue-id)
  "Find the org heading for the specified ISSUE-ID.
Returns marker position of the heading or nil if not found."
  (with-current-buffer (find-file-noselect linear-org-file)
    (org-with-wide-buffer
     (goto-char (point-min))
     (when (re-search-forward (format ":%s: *%s" linear-org-issue-id-property issue-id) nil t)
       (org-back-to-heading t)
       (point-marker)))))

(defun linear-org-format-issue-heading (issue)
  "Format the heading for ISSUE."
  (let* ((identifier (cdr (assoc 'identifier issue)))
         (title (cdr (assoc 'title issue)))
         (state-name (cdr (assoc 'name (assoc 'state issue))))
         (org-state (linear-org-linear-to-org-state state-name))
         (priority (cdr (assoc 'priority issue)))
         (org-priority (linear-org-linear-to-org-priority priority)))

    (concat
     (if org-priority
         (format "* %s [#%s] %s: %s" org-state org-priority identifier title)
       (format "* %s %s: %s" org-state identifier title)))))

(defun linear-org-format-issue-properties (issue)
  "Format the properties for ISSUE."
  (let* ((id (cdr (assoc 'id issue)))
         (team-id (cdr (assoc 'id (assoc 'team issue))))
         (team-name (cdr (assoc 'name (assoc 'team issue))))
         (url (cdr (assoc 'url issue)))
         (updated-at (cdr (assoc 'updatedAt issue))))

    (concat
     ":PROPERTIES:\n"
     (format ":%s: %s\n" linear-org-issue-id-property id)
     (format ":%s: %s\n" linear-org-team-id-property team-id)
     (format ":%s: %s\n" "LINEAR_TEAM_NAME" team-name)
     (format ":%s: %s\n" linear-org-url-property url)
     (format ":%s: %s\n" linear-org-modified-property updated-at)
     ":END:\n")))

(defun linear-org-sync-issue (issue)
  "Synchronize a single ISSUE from Linear to org."
  (let* ((id (cdr (assoc 'id issue)))
         (title (cdr (assoc 'title issue)))
         (description (or (cdr (assoc 'description issue)) ""))
         (existing (linear-org-find-issue-heading id)))

    (with-current-buffer (find-file-noselect linear-org-file)
      (org-with-wide-buffer
       (if existing
           ;; Update existing entry
           (progn
             (goto-char existing)
             (delete-region (point) (line-end-position))
             (insert (linear-org-format-issue-heading issue))

             ;; Update properties
             (org-end-of-meta-data)
             (let ((properties-end (point)))
               (org-back-to-heading t)
               (re-search-forward ":PROPERTIES:" properties-end t)
               (beginning-of-line)
               (delete-region (point)
                              (save-excursion
                                (re-search-forward ":END:" nil t)
                                (line-end-position)))
               (insert (linear-org-format-issue-properties issue)))

             ;; Update content if necessary
             (org-end-of-meta-data t)
             (when (org-at-heading-p)
               ;; No content yet, add the description
               (insert "\n" description)))

         ;; Create new entry
         (goto-char (point-max))
         (unless (bolp) (insert "\n"))
         (insert (linear-org-format-issue-heading issue) "\n")
         (insert (linear-org-format-issue-properties issue))
         (when (and description (not (string-empty-p description)))
           (insert "\n" description))))
      (save-buffer))))

;;;###autoload
(defun linear-org-sync-from-linear ()
  "Synchronize issues from Linear to org file."
  (interactive)
  (linear-org-ensure-linear-file)
  (let ((issues (linear-org-api-query-assigned-issues)))
    (if issues
        (progn
          (message "Syncing %d issues from Linear to org..." (length issues))
          (dolist (issue issues)
            (linear-org-sync-issue issue))
          (setq linear-org-last-sync-time (current-time))
          (message "Linear-org sync completed"))
      (message "No issues found or failed to retrieve issues"))))

(defun linear-org-extract-issue-id ()
  "Extract Linear issue ID from the current org entry."
  (org-entry-get (point) linear-org-issue-id-property))

(defun linear-org-extract-team-id ()
  "Extract Linear team ID from the current org entry."
  (org-entry-get (point) linear-org-team-id-property))

(defun linear-org-update-linear-issue (id team-id title state description)
  "Update a Linear issue with ID, TEAM-ID, TITLE, STATE, and DESCRIPTION."
  (linear--log "Updating Linear issue %s" id)
  (let* ((query "mutation UpdateIssue($id: String!, $input: IssueUpdateInput!) {
                  issueUpdate(id: $id, input: $input) {
                    success
                    issue {
                      id
                      identifier
                      title
                      updatedAt
                    }
                  }
                }")
         (state-id (when state
                     (let* ((states-query "query GetStates($teamId: String!) {
                                            team(id: $teamId) {
                                              states {
                                                nodes {
                                                  id
                                                  name
                                                }
                                              }
                                            }
                                          }")
                            (variables `(("teamId" . ,team-id)))
                            (response (linear--graphql-request states-query variables))
                            (states (when response
                                      (cdr (assoc 'nodes (assoc 'states (assoc 'team (assoc 'data response))))))))
                       (when states
                         (cdr (assoc 'id
                                     (seq-find (lambda (s)
                                                 (string= (cdr (assoc 'name s)) state))
                                               states)))))))

         (input `(("title" . ,title)
                  ,@(when description
                      `(("description" . ,description)))
                  ,@(when state-id
                      `(("stateId" . ,state-id)))))

         (variables `(("id" . ,id)
                      ("input" . ,input)))

         (response (linear--graphql-request query variables)))

    (if (and response (assoc 'data response))
        (progn
          (message "Updated Linear issue: %s"
                   (cdr (assoc 'identifier
                               (assoc 'issue
                                      (assoc 'issueUpdate
                                             (assoc 'data response))))))
          t)
      (message "Failed to update Linear issue")
      nil)))

;;;###autoload
(defun linear-org-sync-to-linear ()
  "Sync the current org entry to Linear."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (let* ((issue-id (linear-org-extract-issue-id))
           (team-id (linear-org-extract-team-id)))

      (if (and issue-id team-id)
          (let* ((title (org-get-heading t t t t))
                 ;; Extract the identifier prefix if present
                 (title (if (string-match "\\([A-Z]+-[0-9]+\\): \\(.*\\)" title)
                            (match-string 2 title)
                          title))
                 (todo-state (org-get-todo-state))
                 (linear-state (linear-org-org-to-linear-state todo-state))
                 ;; Get description from content
                 (description (save-excursion
                                (org-end-of-meta-data t)
                                (when (org-at-heading-p)
                                  "")
                                (let ((start (point)))
                                  (if (org-goto-sibling)
                                      (buffer-substring-no-properties start (line-beginning-position))
                                    (buffer-substring-no-properties start (point-max)))))))

            (if (linear-org-update-linear-issue issue-id team-id title linear-state description)
                (progn
                  (message "Synchronized org entry to Linear")
                  (org-entry-put (point) linear-org-modified-property
                                 (format-time-string "%Y-%m-%dT%H:%M:%SZ" (current-time))))
              (message "Failed to sync to Linear")))

        (message "Current entry is not a Linear task")))))

;;;###autoload
(defun linear-org-capture-to-linear ()
  "Capture a new Linear issue from an org entry."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (let* ((title (org-get-heading t t t t))
           (todo-state (org-get-todo-state))
           (linear-state (linear-org-org-to-linear-state todo-state))
           ;; Get description from content
           (description (save-excursion
                          (org-end-of-meta-data t)
                          (when (org-at-heading-p)
                            "")
                          (let ((start (point)))
                            (if (org-goto-sibling)
                                (buffer-substring-no-properties start (line-beginning-position))
                              (buffer-substring-no-properties start (point-max))))))

           ;; Ask for the team
           (teams (linear-get-teams))
           (team-options (mapcar (lambda (team)
                                   (cons (cdr (assoc 'name team))
                                         team))
                                 teams))
           (selected-team-name (completing-read "Team: " team-options nil t))
           (selected-team (cdr (assoc selected-team-name team-options)))
           (team-id (cdr (assoc 'id selected-team)))

           ;; Create the issue in Linear
           (query "mutation CreateIssue($input: IssueCreateInput!) {
                    issueCreate(input: $input) {
                      success
                      issue {
                        id
                        identifier
                        title
                        url
                        updatedAt
                      }
                    }
                  }")

           (input `(("title" . ,title)
                    ("description" . ,description)
                    ("teamId" . ,team-id)))

           (variables `(("input" . ,input)))
           (response (linear--graphql-request query variables)))

      (if (and response (assoc 'data response))
          (let* ((issue-data (assoc 'issue (assoc 'issueCreate (assoc 'data response))))
                 (id (cdr (assoc 'id issue-data)))
                 (identifier (cdr (assoc 'identifier issue-data)))
                 (url (cdr (assoc 'url issue-data)))
                 (updated-at (cdr (assoc 'updatedAt issue-data))))

            ;; Set properties on the org entry
            (org-entry-put (point) linear-org-issue-id-property id)
            (org-entry-put (point) linear-org-team-id-property team-id)
            (org-entry-put (point) "LINEAR_TEAM_NAME" (cdr (assoc 'name selected-team)))
            (org-entry-put (point) linear-org-url-property url)
            (org-entry-put (point) linear-org-modified-property updated-at)

            ;; Update the heading to include the identifier
            (let ((new-heading (if (string-match "^\\* \\(TODO\\|NEXT\\|HOLD\\|DONE\\|Cancelled\\) " title)
                                   (replace-match (format "* %s %s: " (match-string 1 title) identifier) t t title)
                                 (format "* TODO %s: %s" identifier title))))
              (delete-region (line-beginning-position) (line-end-position))
              (insert new-heading))

            (message "Created Linear issue: %s" identifier)
            (save-buffer))

        (message "Failed to create Linear issue")))))

;;;###autoload
(defun linear-org-open-issue ()
  "Open the current Linear issue in browser."
  (interactive)
  (let ((url (org-entry-get (point) linear-org-url-property)))
    (if url
        (browse-url url)
      (message "No Linear URL found for this entry"))))

(defun linear-org-start-auto-sync ()
  "Start automatic synchronization from Linear to org."
  (interactive)
  (when linear-org-timer
    (cancel-timer linear-org-timer))

  (setq linear-org-timer
        (run-with-timer 0 linear-org-sync-interval 'linear-org-sync-from-linear))
  (message "Linear-org auto-sync started"))

(defun linear-org-stop-auto-sync ()
  "Stop automatic synchronization."
  (interactive)
  (when linear-org-timer
    (cancel-timer linear-org-timer)
    (setq linear-org-timer nil)
    (message "Linear-org auto-sync stopped")))

(defun linear-org-after-todo-state-change ()
  "Hook function to sync todo state changes to Linear."
  (when (and (buffer-file-name)
             (string= (expand-file-name (buffer-file-name)) (expand-file-name linear-org-file))
             (org-entry-get (point) linear-org-issue-id-property))
    (linear-org-sync-to-linear)))

;; Add hook for todo state changes
(add-hook 'org-after-todo-state-change-hook 'linear-org-after-todo-state-change)

;; Define keybindings
(after! linear
  (map! :map org-mode-map
        :localleader
        (:prefix ("L" . "Linear")
         :desc "Sync from Linear" "s" #'linear-org-sync-from-linear
         :desc "Sync to Linear" "p" #'linear-org-sync-to-linear
         :desc "Capture to Linear" "c" #'linear-org-capture-to-linear
         :desc "Open in browser" "o" #'linear-org-open-issue
         :desc "Start auto-sync" "a" #'linear-org-start-auto-sync
         :desc "Stop auto-sync" "S" #'linear-org-stop-auto-sync))

  (map! :leader
        (:prefix ("L" . "Linear")
         :desc "Sync from Linear" "s" #'linear-org-sync-from-linear
         :desc "Start auto-sync" "a" #'linear-org-start-auto-sync
         :desc "Stop auto-sync" "S" #'linear-org-stop-auto-sync)))

;; Capture template for Linear tasks
(after! org
  (add-to-list 'org-capture-templates
               '("L" "Linear Task" entry
                 (file+headline linear-org-file "Tasks")
                 "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n"
                 :immediate-finish nil
                 :jump-to-captured t
                 :after-finalize (lambda ()
                                   (with-current-buffer (find-buffer-visiting linear-org-file)
                                     (save-excursion
                                       (goto-char (point-max))
                                       (org-back-to-heading t)
                                       (call-interactively 'linear-org-capture-to-linear)))))))

(provide 'linear-org)
;;; linear-org.el ends here
