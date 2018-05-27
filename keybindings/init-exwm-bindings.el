(require 'exwm-input)

(require 'init-frame-map)

(use-package window-go
  :straight (window-go :host github :repo "akirak/emacs-window-go"))
(use-package exwm-window-go
  :straight window-go)

(defmacro akirak/exwm-bind-keys (&rest bindings)
  "Bind input keys in EXWM.

BINDINGS is a list of cons cells containing a key (string) and a command."
  `(progn
     ,@(cl-loop for (key . cmd) in bindings
                collect `(exwm-input-set-key ,(cond
                                               ((stringp key) (kbd key))
                                               (t key))
                                             (quote ,cmd)))))

(akirak/exwm-bind-keys
 ("s-A" . akirak/org-optimistic-agenda)
 ("s-C" . akirak/screenshot)
 ("s-F" . (lambda () (interactive) (let ((current-prefix-arg 4))
                                     (call-interactively 'counsel-ag))))
 ("s-H" . exwm-window-go-shrink)
 ("s-L" . exwm-window-go-grow)
 ;; ("s-S" . exwm-workspace-move-window)
 ("s-X" . frame-workflow-select-action)
 ("s-Z" . counsel-org-offtime)
 ("s-a" . org-agenda)
 ("s-b" . exwm-workspace-switch-to-buffer)
 ("s-c" . org-capture)
 ("s-d" . (lambda () (interactive) (org-journal-new-entry t)))
 ("s-e" . ivy-bookmarked-directory)
 ("s-f" . counsel-locate)
 ("s-g" . akirak/frame-map)
 ("s-h" . exwm-window-go-shrink-horizontally)
 ("s-j" . other-window)
 ("s-k" . window-go-previous)
 ("s-l" . exwm-window-go-grow-horizontally)
 ("s-m" . window-go-master)
 ("s-n" . akirak/exwm-next-workspace)
 ("s-o" . (lambda () (interactive) (switch-to-buffer (other-buffer))))
 ("s-p" . akirak/exwm-previous-workspace)
 ("s-s" . frame-workflow-switch-frame)
 ("s-u" . exwm-reset)
 ("s-v" . toggle-window-split)
 ("s-w" . akirak/exwm-goto-browser)
 ("s-x" . frame-workflow-action-map)
 ("s-y" . window-go-other-buffer-in-split-window)
 ("s-z" . counsel-org-clock-context)
 ("s-6" . window-go-first-file-window)
 ("s-7" . windmove-left)
 ("s-8" . windmove-right)
 ("s-9" . window-go-bottom)
 ("s-," . winner-undo)
 ("s-." . winner-redo)
 ("s-[" . fm-left-frame)
 ("s-]" . fm-right-frame)
 ("s-\\" . katawa-ivy-exwm)
 ("s--" . delete-other-windows)
 ("s-S-SPC" . balance-windows)
 ("s-SPC" . akirak/exwm-window-command-map)
 ("M-<f2>" . akirak/counsel-external-command)
 ("M-S-<f2>" . counsel-linux-app)
 ("M-<f4>" . kill-this-buffer-and-its-window)
 ("<s-return>" . window-go-swap-master)
 ("<s-insert>" . frame-workflow-make-frame)
 ("<s-delete>" . exwm-workspace-delete)
 ;; ("<print>" . akirak/screenshot)
 ("s-0" . delete-window)
 ("s-/" . counsel-wmctrl))

;;;; Keybindings in exwm-mode-map 
(general-def exwm-mode-map
  "s-i" #'exwm-input-release-keyboard)

;;;; Keymap for commands on the current X window
(define-prefix-command 'akirak/exwm-window-command-map)

(general-def akirak/exwm-window-command-map
  "f" #'exwm-layout-toggle-fullscreen
  "t" #'exwm-floating-toggle-floating
  "h" #'exwm-floating-hide)

;;;; Simulation key

(defvar jf/default-simulation-keys
  '(
    ;; movement
    ([?\C-b] . left)
    ([?\M-b] . C-left)
    ([?\C-f] . right)
    ([?\M-f] . C-right)
    ([?\C-p] . up)
    ([?\C-n] . down)
    ([?\C-a] . home)
    ([?\C-e] . end)
    ([?\M-v] . prior)
    ([?\C-v] . next)
    ([?\C-d] . delete)
    ([?\C-k] . (S-end delete))
    ([?\M-d] . (C-S-right delete))
    ;; cut/paste.
    ([?\C-w] . ?\C-x)
    ([?\M-w] . ?\C-c)
    ([?\C-y] . ?\C-v)
    ;; search
    ([?\C-s] . ?\C-f)
    ))

(with-eval-after-load 'exwm-input
  (exwm-input-set-simulation-keys jf/default-simulation-keys))

(defun akirak/exwm-next-workspace (&optional inc)
  (interactive)
  (let* ((inc (or inc 1))
         (index (+ exwm-workspace-current-index inc))
         (len (length exwm-workspace--list)))
    (exwm-workspace-switch (cond
                            ((< index 0) (+ index len))
                            ((< index len) index)
                            (t (- index len))))))

(defun akirak/exwm-previous-workspace (&optional inc)
  (interactive)
  (akirak/exwm-next-workspace (- (or inc 1))))

(provide 'init-exwm-bindings)
