# Persp-Mode Magit Buffer Restoration Fix

## Problem
When loading perspective sessions, persp-mode attempted to restore magit buffers saved in old session files. This caused errors because magit status buffers use a special initialization macro (`def-magit-status-buffer`) that cannot be restored from saved session data.

The error looked like:
```
Symbol's function definition is void: def-magit-status-buffer
```

## Previous Approach (Failed)
The first attempt used `persp-buffer-restore-functions` to intercept buffer restoration. This approach failed because:
1. It ran DURING the restoration process, not before
2. The built-in magit restoration handler still ran and errored out
3. The error occurred before custom handlers could prevent it

## Current Solution
The new approach uses `:around` advice on `persp-load-state-from-file` to:

1. **Intercept the loading process** BEFORE any buffer restoration begins
2. **Filter the loaded data structure** to remove magit buffer entries
3. **Allow normal restoration to proceed** with the filtered data

This ensures persp-mode never even attempts to restore magit buffers.

## Implementation Details

### Function: `my/persp-filter-magit-buffers-advice`
Located at: `/Users/allup/.config/doom/config.el:1054-1090`

This advice function:
- Wraps `persp-load-state-from-file` to intercept perspective loading
- Processes each loaded perspective's buffer list
- Filters out any buffer entries matching magit patterns:
  - Contains `def-magit-status-buffer` in the serialized data
  - Has "magit" in the buffer name
  - Uses a magit-related major mode
- Updates the perspective data structure with filtered buffer lists
- Logs how many buffers were filtered for transparency

### Detection Patterns
The function identifies magit buffers by checking:
```elisp
;; Check serialized buffer data
(string-match-p "def-magit-status-buffer" buf-string)

;; Check buffer name
(string-match-p "magit" buf-name)

;; Check major mode
(string-match-p "magit-.*-mode" buf-string)
```

## Testing Instructions

### 1. Reload Configuration
```elisp
M-x doom/reload
```
or restart Emacs.

### 2. Load a Session with Magit Buffers
Try loading your session that previously caused errors:
```elisp
M-x my/load-complete-session
```

### 3. Expected Behavior
You should see messages like:
```
[persp-mode] Filtered 2 magit buffer(s) from perspective 'workspace-name'
Loaded session 'session-name'
```

No errors should occur, and the session should load successfully with all buffers except magit ones.

### 4. Verify Magit Still Works
After loading the session:
1. Open a git project: `SPC p p`
2. Open magit status: `SPC g g`
3. Magit should work normally - it just won't be restored from old sessions

## Why This Works

### Data Flow:
```
1. User loads session file
   ↓
2. persp-load-state-from-file reads file
   ↓
3. [OUR ADVICE RUNS HERE]
   ↓
4. Filter magit entries from loaded data
   ↓
5. Return filtered data
   ↓
6. Normal restoration proceeds (no magit buffers to restore)
   ↓
7. Success!
```

### Key Differences from Previous Approach:
| Aspect | Previous Approach | Current Approach |
|--------|-------------------|------------------|
| **Timing** | During restoration | Before restoration |
| **Scope** | Per-buffer handler | Entire data structure |
| **Prevention** | Tried to skip restoration | Removes from data completely |
| **Built-in handlers** | Still ran and errored | Never see magit buffers |

## Prevention for Future Sessions

The configuration also includes (unchanged):
1. **Buffer save filtering** (`persp-filter-save-buffers-functions`) - prevents NEW magit buffers from being saved
2. **Pre-save cleanup** (`my/pre-save-workspace-cleanup`) - removes magit buffers before saving

This ensures future sessions won't contain magit buffers, while the new advice handles old session files that already contain them.

## Files Modified
- `/Users/allup/.config/doom/config.el` (lines 1052-1090)

## Configuration Location
The fix is in the `(after! persp-mode ...)` block, specifically the "Filter magit buffers from perspective data BEFORE restoration begins" section.

## Troubleshooting

### If you still see errors:
1. Check that advice was applied:
   ```elisp
   (advice-member-p #'my/persp-filter-magit-buffers-advice 'persp-load-state-from-file)
   ;; Should return t
   ```

2. Enable debugging to see what's being filtered:
   ```elisp
   (setq persp-verbose 3)
   ```

3. Check the messages buffer after loading: `SPC b m`

### To remove the advice (for debugging):
```elisp
(advice-remove 'persp-load-state-from-file #'my/persp-filter-magit-buffers-advice)
```

### To reapply manually:
```elisp
(advice-add 'persp-load-state-from-file :around #'my/persp-filter-magit-buffers-advice)
```

## Additional Notes

- **Performance**: The filtering adds negligible overhead (only runs during session load)
- **Safety**: Uses `cl-remove-if` which creates a new list, leaving original data intact in memory
- **Compatibility**: Works with all Doom Emacs versions that include persp-mode
- **Magit functionality**: Completely unaffected - magit buffers just aren't persisted/restored

## Related Functions
- `my/save-workspace-with-project` - Enhanced workspace saving
- `my/load-complete-session` - Session loading with error handling
- `my/cleanup-magit-buffers-in-workspace` - Manual cleanup utility
- `my/pre-save-workspace-cleanup` - Pre-save buffer filtering
