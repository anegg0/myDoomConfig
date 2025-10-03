# LSP Debugging Guide for TypeScript in Doom Emacs

## Quick Diagnosis Commands

Use `SPC d l` prefix for all LSP debugging commands:

- `SPC d l d` - Diagnose current LSP server (shows which server is running)
- `SPC d l c` - Check all LSP servers across all buffers
- `SPC d l T` - Debug TypeScript LSP (detailed diagnostic report)
- `SPC d l m` - Monitor TypeScript LSP in real-time

## Quick Fix Commands

- `SPC d l t` - Fix TypeScript LSP (comprehensive fix)
- `SPC d l r` - Restart LSP server
- `SPC d l k` - Kill all LSP servers
- `SPC d l s` - Apply safe LSP configuration

## Switching LSP Backends

- `SPC d l e` - Switch to Eglot (recommended, currently configured)
- `SPC d l l` - Switch to LSP Mode

## Manual Commands (M-x)

- `M-x my/fix-typescript-lsp` - Comprehensive TypeScript LSP fix
- `M-x my/typescript-lsp-debug` - Generate debug report
- `M-x my/typescript-lsp-monitor` - Monitor LSP events
- `M-x my/lsp-diagnose-current-server` - Check current server status
- `M-x my/lsp-restart-server` - Restart the current LSP server

## What the Fix Does

When you run the TypeScript LSP fix (`SPC d l t`), it:

1. **Clears cache files**: Removes `.tsbuildinfo`, `node_modules/.cache`, `.eslintcache`
2. **Checks TypeScript server**: Optionally updates `typescript-language-server` and `typescript`
3. **Kills zombie processes**: Terminates any stuck `tsserver` or `typescript-language-server` processes
4. **Restarts LSP**: Cleanly restarts the language server

## Auto-Recovery Features

The configuration includes:

- **Automatic crash recovery**: Attempts to restart LSP up to 3 times after a crash
- **Rate limiting**: Resets crash counter after 5 minutes
- **Safe configuration**: Disables problematic features that can cause crashes
- **TypeScript-specific optimizations**: Disables inlay hints and other heavy features

## Troubleshooting Steps

If TypeScript LSP keeps crashing:

1. **First, diagnose the issue**:
   ```
   SPC d l T  (Debug TypeScript LSP)
   ```

2. **Try the automatic fix**:
   ```
   SPC d l t  (Fix TypeScript LSP)
   ```

3. **If still having issues, kill all servers and restart**:
   ```
   SPC d l k  (Kill all servers)
   SPC d l r  (Restart server in current buffer)
   ```

4. **Monitor the server to see error messages**:
   ```
   SPC d l m  (Monitor TypeScript LSP)
   ```
   Check the `*EGLOT events*` buffer for error details

5. **As a last resort, switch LSP backends**:
   ```
   SPC d l l  (Try LSP Mode instead of Eglot)
   ```

## Common Issues and Solutions

### Issue: "Server keeps dying and restarting"
**Solution**: Run `SPC d l t` to clear cache and restart cleanly

### Issue: "No completion/diagnostics in TypeScript files"
**Solution**:
1. Check if server is running: `SPC d l d`
2. If dead, restart: `SPC d l r`
3. If still broken, run full fix: `SPC d l t`

### Issue: "TypeScript server using too much CPU/memory"
**Solution**: The safe configuration automatically disables heavy features like inlay hints

### Issue: "Can't find tsserver or typescript-language-server"
**Solution**: The fix command (`SPC d l t`) will offer to install/update them

## Project Requirements

For TypeScript LSP to work properly, your project needs:
- `package.json` file (minimum requirement)
- `tsconfig.json` file (recommended for best experience)
- Node modules installed (`npm install` or `yarn install`)

## Manual Server Installation

If automatic installation fails:

```bash
# With Volta (detected on your system)
volta install typescript-language-server typescript

# Or with npm
npm install -g typescript-language-server typescript

# Or with yarn
yarn global add typescript-language-server typescript
```

## Checking Logs

- **Eglot logs**: Check `*EGLOT events*` buffer
- **LSP Mode logs**: Check `*lsp-log*` buffer
- **Messages**: Check `*Messages*` buffer for error messages

## Configuration Location

All LSP debugging and fixing code is in:
- `/Users/allup/.config/doom/lsp-debug-fix.el`

The configuration is automatically loaded via your `config.el`.