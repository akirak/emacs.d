(fset 'yes-or-no-p 'y-or-n-p)           ; Ask y or n rather than yes or no
(global-auto-revert-mode 1)             ; Automatically revert the buffer when the file is changed on disk
(save-place-mode 1)                     ; Save per-file cursor positions
(recentf-mode 1)                        ; Keep a list of recent files
;; Exclude *-autoloads.el mostly generated by straight.el from recent files
(add-to-list 'recentf-exclude
             (expand-file-name "~/\.emacs\.d/straight/build/.+-autoloads\.el"))
(add-to-list 'recentf-exclude
             (expand-file-name "~/\.emacs\.d/\.cache\/") t)
(setq recentf-max-saved-items 200)
(show-paren-mode 1)                     ; Highlight matching parentheses
(electric-pair-mode 1)                  ; Insert parentheses in pair
(tool-bar-mode -1)                      ; Disable UI features
(scroll-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(winner-mode 1)                         ; Enable winner-mode for undoing/redoing the window configuration
(blink-cursor-mode 0)

;; Default variables
(setq-default version-control t
              vc-follow-symlinks t
              vc-make-backup-files t
              buffer-file-coding-system 'utf-8
              sentence-end-double-space nil
              default-fill-column 80
              indent-tabs-mode nil
              indicate-empty-lines t
              truncate-lines t
              backup-by-copying t
              delete-by-moving-to-trash t
              delete-old-versions t)

(defun akirak/turn-on-indent-tabs-mode ()
  (interactive)
  (setq indent-tabs-mode 1))

(dolist (mode-hook '(makefile-mode-hook
                     org-mode-hook))
  (add-hook mode-hook 'akirak/turn-on-indent-tabs-mode))

(setq inhibit-startup-screen t
      ring-bell-function 'ignore
      x-gtk-use-system-tooltips nil
      use-dialog-box nil
      ediff-window-setup-function #'ediff-setup-windows-plain)

(setq bookmark-default-file "~/.emacs-bookmarks")

(add-hook 'prog-mode-hook (lambda () (hl-line-mode 1)))
(add-hook 'text-mode-hook (lambda () (hl-line-mode 1)))

(cond
 ((eq system-type 'windows-nt)
  (setq shell-file-name (executable-find "bash"))))

(add-hook 'java-mode 'subword-mode)

(provide 'setup-defaults)
;;; setup-defaults.el ends here
