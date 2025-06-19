;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("10e5d4cc0f67ed5cafac0f4252093d2119ee8b8cb449e7053273453c1a1eb7cc"
     "0325a6b5eea7e5febae709dab35ec8648908af12cf2d2b569bedc8da0a3a81c1" default))
 '(ignored-local-variable-values '((flycheck-disabled-checkers proselint textlint)))
 '(package-selected-packages '(async org-preview-html undo-fu undo-fu-session))
 '(safe-local-variable-values
   '((eval add-hook 'after-save-hook 'org-html-export-to-html t t)
     (checkdoc-allow-quoting-nil-and-t . t)
     (elisp-lint-indent-specs (describe . 1) (it . 1) (thread-first . 0)
                              (cl-flet . 1) (cl-flet* . 1)
                              (org-element-map . defun)
                              (org-roam-dolist-with-progress . 2)
                              (org-roam-with-temp-buffer . 1)
                              (org-with-point-at . 1)
                              (magit-insert-section . defun)
                              (magit-section-case . 0) (org-roam-with-file . 2))
     (elisp-lint-ignored-validators "byte-compile" "package-lint"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
