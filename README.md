# This is my Doom Emacs Config

I customized this config mostly around Software Technical Writing, which is my day job. 
If you like to work with Vim and Orgmode, you might find elements to work with.

Note that it integrates with:
- Linear.app (you will need to clone https://github.com/anegg0/linear-emacs for that feature to be available)
- Jethro Kuan's fabulous https://github.com/org-roam/org-roam
- Alvaro Ramirez's perfect solution for accentuated characters on Emacs/MacOS: https://xenodium.com/an-accentuated-emacs-experiment/ 

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
- **French Accent Support**: Integrated `accent` package with custom configuration for French diacritics (à, â, é, è, ê, ë, î, ï, ô, ù, û, ü, ç). **Disabled by default** - must be manually enabled.
  - **Usage**: When enabled, double-tap keys quickly (within 0.17s) to insert accented characters
  - **Toggle Commands**:
    - `SPC t a`: Toggle accent menu globally (all text/org/markdown buffers)
    - `SPC t A`: Toggle accent menu for current buffer only
  - **Example**: Type "ee" quickly → menu appears with (é è ê ë) options
  - Includes org-mode cache fix to prevent conflicts
  - Special thank you to Alvaro Ramirez from xenodium.com!
- **Project-wide Search**: Custom `my/smart-project-occur` function for efficient regex searches across project files using projectile
- **Eglot Modeline**: Custom status indicator `+modeline-eglot-status` showing LSP connection state
- **Enhanced Undo Limits**: Increased to 64MB/96MB/960MB for handling large operations
- **Copilot Chat**: Extended configuration with leader key shortcuts for code review, documentation, and fixes

**Custom Functions:**
- `my/smart-project-occur`: Project-wide occur search with file filtering
- `my/toggle-accent-menu-global`: Toggle accent menu in all supported buffers
- `my/toggle-accent-menu-buffer`: Toggle accent menu in current buffer only
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

---

## Usage

### Custom Functions Reference

All custom functions in this configuration use the `my/` prefix for easy identification.

#### Org-mode Functions
- **`my/org-export-pdf-and-open`**: Export current org file to PDF and open it
- **`my/org-md-export-to-markdown-visible-only`**: Export visible content to markdown, removes anchor tags and TOC
- **`my/org-hot-reload-html-export`**: Enable hot-reload for HTML export
- **`my/org-hot-reload-toggle`**: Toggle HTML auto-export on save
- **`my/org-export-html-and-open`**: Export to HTML and open in browser
- **`my/org-todo-list-all-by-tag`**: Display all TODO states filtered by a specific tag
- **`my/org-agenda-filter-interactive`**: Interactively filter org-agenda by up to 2 tags and TODO state (with Vertico completion)
- **`my/extract-todo-keywords`**: Extract clean TODO keyword list from configuration
- **`my/completing-read-tag`**: Tag completion helper for agenda files
- **`my/completing-read-todo`**: TODO state completion helper
- **`my/build-skip-function`**: Build dynamic skip function for agenda filtering

#### Org-roam Functions
- **`my/org-roam-node-has-tag`**: Filter function to check if node has specified tag
- **`my/org-roam-node-find-by-tag`**: Find and open nodes based on tag
- **`my/org-roam-find-files-without-id`**: Generate report of org files missing roam IDs

#### Linear Integration Functions
- **`my/linear-load-api-key-from-auth-source`**: Load Linear API key from auth-source
- **`my/linear-sync-single-issue-at-point`**: Sync only current issue to Linear API
- **`my/toggle-linear-auto-sync`**: Toggle automatic Linear sync before todo list
- **`my/run-linear-emacs-list-issues-before-todo`**: Run Linear sync before org-todo-list
- **`my/enable-linear-org-sync`**: Auto-enable sync when linear.org is opened
- **`my/auto-save-org-files-on-todo-change`**: Auto-save linear.org and inbox.org on TODO state changes

#### Editor Functions
- **`my/toggle-accent-menu-buffer`**: Toggle accent menu in current buffer only
- **`my/smart-project-occur`**: Project-wide occur search with file filtering
- **`my/drag-n-drop-handler`**: Handle files dropped onto Emacs frame
- **`my/emacs-everywhere-markdown-filename`**: Generate markdown filename for emacs-everywhere

#### Git/Magit Functions
- **`my/magit-submodule-update-init-recursive`**: Run git submodule update --init --recursive
- **`my/magit-checkout-advice`**: Invalidate projectile cache after git checkout

### Keybindings Reference

All custom keybindings use the leader key (`SPC` in normal mode).

#### Navigation & Basic Operations
| Keybinding | Function | Description |
|------------|----------|-------------|
| `SPC j` | `evil-avy-goto-line` | Jump to line with avy |
| `SPC k` | `evil-avy-goto-char` | Jump to character with avy |
| `SPC y` | `yank-from-kill-ring` | Yank from kill ring |
| `SPC z` | `treemacs-add-and-display-current-project-exclusively` | Show project in treemacs |
| `SPC a` | `avy-move-line` | Move line with avy |
| `SPC 8` | `consult-buffer` | Switch buffer with consult |
| `SPC 9` | `dired-jump` | Jump to dired |
| `SPC ]` | `other-window` | Switch to other window |

#### Registers & Clipboard
| Keybinding | Function | Description |
|------------|----------|-------------|
| `SPC 1` | `xah-copy-to-register-1` | Copy to register 1 |
| `SPC 2` | `xah-paste-from-register-1` | Paste from register 1 |
| `SPC 3` | `copy-to-register` | Copy to register (prompted) |
| `SPC 4` | `insert-register` | Insert from register (prompted) |

#### Org-mode
| Keybinding | Function | Description |
|------------|----------|-------------|
| `SPC 5` | `org-id-get-create` | Create/get org ID for heading |
| `SPC 7` | `my/org-export-html-and-open` | Export to HTML and open |
| `SPC o e` | `my/eat-popup` | Open eat terminal |
| `SPC o f` | `my/org-agenda-filter-interactive` | Filter agenda by tags + TODO (interactive) |
| `SPC e m` | `my/org-md-export-to-markdown-visible-only` | Export visible to markdown |
| `SPC e p` | `my/org-export-pdf-and-open` | Export to PDF and open |
| `C-c TAB` | `jethro/org-capture-inbox` | Capture to inbox |
| `C-c SPC` | `jethro/org-capture-slipbox` | Capture to slipbox |

#### Org-roam (prefix: `SPC r`)
| Keybinding | Function | Description |
|------------|----------|-------------|
| `SPC r l` | `org-roam-buffer-toggle` | Toggle org-roam buffer |
| `SPC r i` | `org-roam-node-insert` | Insert org-roam node |
| `SPC r f` | `org-roam-node-find` | Find org-roam node |
| `SPC r F` | `my/org-roam-node-find-by-tag` | Find node by tag |
| `SPC r r` | `org-roam-ref-find` | Find by reference |
| `SPC r g` | `org-roam-show-graph` | Show graph |
| `SPC r T` | `org-roam-dailies-capture-today` | Capture daily note |
| `SPC r t` | `org-roam-dailies-goto-today` | Go to today's daily |
| `SPC r TAB` | `jethro/org-capture-slipbox` | Capture to slipbox |
| `SPC r c` | `org-roam-capture` | Org-roam capture |
| `SPC r m` | `my/org-roam-find-files-without-id` | Find files without IDs |

#### Linear (prefix: `SPC l`)
| Keybinding | Function | Description |
|------------|----------|-------------|
| `SPC l s` | `linear-emacs-list-issues` | Sync all Linear issues |
| `SPC l t` | `my/toggle-linear-auto-sync` | Toggle Linear auto-sync |
| `SPC l n` | `linear-emacs-new-issue` | Create new Linear issue |
| `SPC l c` | `linear-emacs-test-connection` | Test Linear connection |
| `SPC l C` | `linear-emacs-check-setup` | Check Linear setup |
| `SPC l d` | `linear-emacs-toggle-debug` | Toggle Linear debug mode |

#### Git/Development
| Keybinding | Function | Description |
|------------|----------|-------------|
| `SPC 6` | `my/magit-submodule-update-init-recursive` | Update git submodules |
| `SPC 0` | `aidermacs-run` | Run aidermacs |

#### Editor Settings
| Keybinding | Function | Description |
|------------|----------|-------------|
| `SPC t a` | `my/toggle-accent-menu-global` | Toggle accent menu globally |
| `SPC t A` | `my/toggle-accent-menu-buffer` | Toggle accent menu (buffer) |
| `SPC C C` | `company-mode` | Toggle company-mode |
| `SPC P P` | `copilot-mode` | Toggle copilot-mode |

#### Copilot (in copilot-completion-map)
| Keybinding | Function | Description |
|------------|----------|-------------|
| `C-TAB` | `copilot-accept-completion` | Accept copilot completion |
| `TAB` | `copilot-accept-completion-by-word` | Accept completion by word |
