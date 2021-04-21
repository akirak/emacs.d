(fset 'yes-or-no-p 'y-or-n-p)           ; Ask y or n rather than yes or no
(save-place-mode 1)                     ; Save per-file cursor positions
(recentf-mode 1)                        ; Keep a list of recent files
;; Exclude machine-generated files in user-emacs-directory
(require 'rx)
(add-to-list 'recentf-exclude
             (rx-to-string `(and bol ,(expand-file-name user-emacs-directory)
                                 (or "straight/build/"
                                     "var/"
                                     ".cache/")))
             t)
(setq recentf-max-saved-items 200)
(show-paren-mode 1)                     ; Highlight matching parentheses

(tooltip-mode -1)
(winner-mode 1)                         ; Enable winner-mode for undoing/redoing the window configuration
(blink-cursor-mode 0)

(use-package autorevert
  :straight (:type built-in)
  :custom
  (global-auto-revert-mode t))

;; Default variables
(setq-default version-control t
              vc-follow-symlinks t
              vc-make-backup-files t
              buffer-file-coding-system 'utf-8
              sentence-end-double-space nil
              default-fill-column 80
              indent-tabs-mode nil
              recenter-positions '(top middle bottom)
              indicate-empty-lines t
              truncate-lines t
              backup-by-copying t
              delete-by-moving-to-trash t
              delete-old-versions t
              view-read-only t
              compilation-auto-jump-to-first-error t
              compilation-scroll-output t
              sentence-end-double-space nil
              ;; lock files will kill `npm start'
              create-lockfiles nil
              ;; For lsp-mode
              read-process-output-max (* 1024 1024))

(set-language-environment "UTF-8")

(defun akirak/turn-on-indent-tabs-mode ()
  (interactive)
  (setq indent-tabs-mode 1))

(dolist (mode-hook '(makefile-mode-hook))
  (add-hook mode-hook 'akirak/turn-on-indent-tabs-mode))

(setq ring-bell-function 'ignore
      x-gtk-use-system-tooltips nil
      use-dialog-box nil
      ediff-window-setup-function #'ediff-setup-windows-plain)

(defun akirak/scroll-half-height (&optional window)
  (/ (1- (window-height (or window (selected-window)))) 2))

(general-def
  [remap scroll-up-command]
  (defun akirak/scroll-half-page-forward (&optional arg)
    (interactive "P")
    (if (numberp arg)
        (scroll-up arg)
      (scroll-up (akirak/scroll-half-height))))
  [remap scroll-down-command]
  (defun akirak/scroll-half-page-backward (&optional arg)
    (interactive "P")
    (if (numberp arg)
        (scroll-down arg)
      (scroll-down (akirak/scroll-half-height))))
  ;; TODO: scroll-other-window and scroll-other-window-down
  )

(use-package view
  :straight (:type built-in)
  :general
  (:keymaps 'View-mode-map
            [remap scroll-up-command] #'View-scroll-half-page-forward
            [remap scroll-down-command] #'View-scroll-half-page-backward)
  :custom
  (view-inhibit-help-message t))

(general-add-hook '(prog-mode-hook
                    text-mode-hook)
                  (defun akirak/turn-off-hl-line-mode ()
                    (hl-line-mode 1)))

(cond
 ((eq system-type 'windows-nt)
  (setq shell-file-name (executable-find "bash"))))

(use-package compile
  :straight (:type built-in)
  :config
  (setq-default compilation-error-regexp-alist
                (list
                 ;; eslint
                 (list (rx bol (group (or "ERROR" "WARNING"))
                           " in "
                           (group (*? anything))
                           ":" (group (+ digit))
                           ":" (group (+ digit)))
                       2 3 4 '(1 . 1))
                 ;; eslint --fix
                 (list (rx bol (group "/home/" (+ nonl)) "\n"
                           (+ space) (group (+ digit)) ":" (group (+ digit))
                           (+ space) (group (or "error" "WARNING")))
                       1 2 3 '(4 . 4))
                 ;; prettier
                 (list (rx bol "[" (group (or "error" "WARNING")) "] "
                           (group (*? anything))
                           ": "
                           (+ anything)
                           "(" (group (+ digit)) ":" (group (+ digit)) ")")
                       2 3 4 '(1 . 1)))))

(provide 'setup-defaults)
;;; setup-defaults.el ends here
