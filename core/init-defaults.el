;;; ak-defaults.el --- Settings without external dependencies

;; Ask y or n rather than yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;; Automatically revert the buffer when the file is changed on disk
(global-auto-revert-mode 1)

;; Save per-file cursor positions
(save-place-mode 1)

;; Keep a list of recent files
(recentf-mode 1)

;; Exclude *-autoloads.el mostly generated by straight.el from recent files
(add-to-list 'recentf-exclude
             (expand-file-name "~/\.emacs\.d/straight/build/.+-autoloads\.el"))

(add-to-list 'recentf-exclude
             (expand-file-name "~/\.emacs\.d/\.cache\/") t)

(setq recentf-max-saved-items 200)

;; Highlight matching parentheses
(show-paren-mode 1)

;; Insert parentheses in pair
(electric-pair-mode 1)

;; Disable UI features
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)

;; Enable winner-mode for undoing/redoing the window configuration
(winner-mode 1)

;; Default variables
(setq-default version-control t
              vc-follow-symlinks t
              vc-make-backup-files t
              coding-system-for-read 'utf-8
              coding-system-for-write 'utf-8
              sentence-end-double-space nil
              default-fill-column 80
              indent-tabs-mode nil
              indicate-empty-lines t
              truncate-lines t
              backup-by-copying t
              delete-by-moving-to-trash t
              delete-old-versions t)

(setq inhibit-startup-screen t
      ring-bell-function 'ignore)

(setq ediff-window-setup-function #'ediff-setup-windows-plain)

;;;; Truncating lines and visual-lines-mode
;; Call this function in each major modes
(defun turn-on-visual-line-mode ()
  (interactive)
  (visual-line-mode 1))
(add-hook 'markdown-mode-hook #'turn-on-visual-line-mode)

(add-hook 'text-mode-hook (lambda () (setq-local truncate-lines nil)))

;;;; Scrolling

;; Scroll up/down by half screen
(defun scroll-half-page-advice (&optional arg)
  (or arg (/ (window-body-height) 2)))

(general-advice-add '(scroll-up scroll-down)
                    :filter-args #'scroll-half-page-advice)

(setq-default scroll-preserve-screen-position 1)

(provide 'init-defaults)
