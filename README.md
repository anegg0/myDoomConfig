# This is my Doom Emacs Config

I customized this config mostly around Software Technical Writing, which is my day-job. 
If you like to work with vim and Orgmode, you might like this config.

Note that it integrates with:
- Linear.app (you will need to clone https://github.com/anegg0/linear-emacs for that feature to be available)
- Jethro Kuan's fabulous https://github.com/org-roam/org-roam

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
