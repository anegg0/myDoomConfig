# This is my Doom Emacs Config

I customized this config mostly around Software Technical Writing, which is my day-job. 
If you like to work with vim and Orgmode, you might like this config.

Note that it integrates with:
- Linear.app (you will need to clone https://github.com/anegg0/linear-emacs for that feature to be available)
- Jethro Kuan's fabulous https://github.com/org-roam/org-roam
- Alvaro Ramirez's perfect solutions for accentuated characters on Emacs/MacOS: https://xenodium.com/an-accentuated-emacs-experiment/ 

## Key Features
- **Enhanced Org-mode**: Org-roam integration, custom export templates, Linear.app synchronization
- **Development Tools**: LSP support (Eglot), Copilot integration, syntax highlighting for multiple languages
- **UI/UX**: Leuven-dark theme, Vertico completion, visual enhancements with Treemacs
- **Productivity**: GTD workflows, custom keybindings, Emacs Everywhere for system-wide editing

## Core Modules

### Completion & UI
- Corfu with orderless for code completion
- Vertico for minibuffer completion
- Doom modeline with icons
- Treemacs for project navigation

### Languages & Tools
- Emacs Lisp, JavaScript, Python, Rust, Solidity
- LaTeX with custom templates
- Markdown/MDX support
- Magit for Git integration

### Org Ecosystem
- Org-roam for knowledge management
- Custom capture templates
- Linear.app two-way sync
- HTML/PDF export configurations

## Notable Packages

- `copilot`: GitHub Copilot integration
- `claudemacs`: Claude AI integration
- `linear-emacs`: Linear issue tracking
- `eat`: Terminal emulator
- `evil`: Vim emulation

## Configuration Files

- `init.el`: Module declarations and feature toggles
- `config.el`: Main configuration with keybindings and settings
- `packages.el`: Package declarations and custom recipes

## Custom Configuration Details

### EDITOR
- **French Accent Support**: Integrated `accent` package with custom configuration for French diacritics (à, â, é, è, ê, ë, î, ï, ô, ù, û, ü, ç). Double-tap keys quickly to insert accented characters. Includes org-mode cache fix to prevent conflicts.
Special thank you to Alvaro Ramirez from xenodium.com!
- **Project-wide Search**: Custom `my/smart-project-occur` function for efficient regex searches across project files using projectile
- **Eglot Modeline**: Custom status indicator `+modeline-eglot-status` showing LSP connection state
- **Enhanced Undo Limits**: Increased to 64MB/96MB/960MB for handling large operations
- **Copilot Chat**: Extended configuration with leader key shortcuts for code review, documentation, and fixes

**Custom Functions:**
- `my/smart-project-occur`: Project-wide occur search with file filtering
- `my/enable-accent-menu`: Manually enable accent menu in current buffer
- `my/drag-n-drop-handler`: Handle files dropped onto Emacs frame
- `my/eat-popup`: Open eat terminal in popup window
- `my/setup-custom-font-fallbacks-mac`: Configure font fallbacks for symbols and emojis on macOS

### ORGMODE
- **Custom TODO States**: Extended workflow states (IN-PROGRESS, IN-REVIEW, BACKLOG, BLOCKED, NEXT, HOLD, WAITING-ON) with color-coded faces
- **Letter-style LaTeX Class**: Custom document class with modified title handling, removes default title page for clean letter format
- **Visible-only Markdown Export**: `my/org-md-export-to-markdown-visible-only` exports only visible content, removes anchor tags and TOC
- **HTML Hot-reload**: `my/org-hot-reload-toggle` enables auto-export on save with live browser preview
- **Custom Export Functions**: `my/org-export-pdf-and-open` and `my/org-export-html-and-open` with leader key bindings
- **Jethro's GTD Capture**: Inbox and slipbox templates with quick capture keybindings (C-c TAB, C-c SPC)
- **ProtonDrive Integration**: Org directory configured to sync with ProtonDrive cloud storage

**Custom Functions:**
- `my/org-md-export-to-markdown-visible-only`: Export visible content to markdown, remove anchor tags and TOC
- `my/org-hot-reload-html-export`: Enable hot-reload for HTML export
- `my/org-hot-reload-toggle`: Toggle HTML auto-export on save
- `my/org-export-html-and-open`: Export to HTML and open in browser (SPC 7)
- `my/org-export-pdf-and-open`: Export to PDF and open viewer (SPC e p)
- `my/org-todo-list-all-by-tag`: Display all TODO states filtered by tag
- `my/auto-save-org-files-on-todo-change`: Auto-save linear.org and inbox.org on TODO changes

### ORGROAM
- **ProtonDrive Sync**: Database and notes stored in cloud-synced ProtonDrive folder
- **Custom Capture Templates**: 
  - Quote notes with :quote: filetag
  - CATB project notes in dedicated folder
  - OCL documentation with OCL_ prefix
  - Prompts with twai_ prefix
  - Hugo-compatible article template with export settings
- **Tag-based Node Finder**: `my/org-roam-node-find-by-tag` function with leader key binding (SPC r F)
- **Missing ID Scanner**: `my/org-roam-find-files-without-id` generates report of org files without roam IDs
- **Enhanced Node Display**: Shows node type (directory) and tags in completion interface

**Custom Functions:**
- `my/org-roam-node-has-tag`: Filter function to check if node has specified tag
- `my/org-roam-node-find-by-tag`: Find and open nodes based on tag (SPC r F)
- `my/org-roam-find-files-without-id`: Generate report of org files missing roam IDs (SPC r m)

### LINEAR
- **Auth-source API Storage**: `my/linear-load-api-key-from-auth-source` loads API key securely from .authinfo
- **Single-issue Sync**: `my/linear-sync-single-issue-at-point` updates only the changed issue to minimize API calls
- **Toggle Auto-sync**: `my/toggle-linear-auto-sync` controls whether Linear updates before todo list display
- **Auto-save on State Change**: Hooks automatically save linear.org and inbox.org when TODO states change
- **Custom Leader Keys**: 
  - SPC l s: Sync all Linear issues
  - SPC l t: Toggle auto-sync
  - SPC l n: Create new issue
  - SPC l c: Test connection
  - SPC l d: Toggle debug mode

**Custom Functions:**
- `my/linear-load-api-key-from-auth-source`: Load Linear API key from auth-source
- `my/linear-sync-single-issue-at-point`: Sync only current issue to Linear API
- `my/toggle-linear-auto-sync`: Toggle automatic Linear sync before todo list
- `my/run-linear-emacs-list-issues-before-todo`: Run Linear sync before org-todo-list
- `my/enable-linear-org-sync`: Auto-enable sync when linear.org is opened
- `my/auto-save-org-files-on-todo-change`: Auto-save linear.org and inbox.org on TODO state changes
- `my/magit-submodule-update-init-recursive`: Run git submodule update (SPC 6)
- `my/magit-checkout-advice`: Invalidate projectile cache after git checkout
