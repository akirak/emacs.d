(require 'exwm-input)
(require 'init-search-map)

(use-package window-go
  :straight (window-go :host github :repo "akirak/emacs-window-go"))
(use-package exwm-window-go
  :straight window-go
  :custom
  (exwm-window-go-cycle-visible-workspaces nil))

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
 ("s-C" . counsel-org-capture)
 ("s-F" . (lambda () (interactive) (let ((current-prefix-arg 4))
                                     (call-interactively 'counsel-ag))))
 ("s-H" . exwm-window-go-shrink)
 ("s-L" . exwm-window-go-grow)
 ("s-P" . exwm-window-go-next-hidden-workspace)
 ("s-S" . frame-workflow-exwm-swap-workspaces)
 ;; ("s-X" . frame-workflow-select-action)
 ("s-Z" . counsel-org-offtime)
 ("s-b" . helm-mini)
 ("s-c" . org-capture)
 ("s-e" . ivy-bookmarked-directory)
 ("s-f" . counsel-locate)
 ("s-g" . akirak/frame-map)
 ("s-h" . exwm-window-go-shrink-horizontally)
 ("s-j" . other-window)
 ("s-k" . window-go-previous)
 ("s-l" . exwm-window-go-grow-horizontally)
 ("s-m" . window-go-master)
 ("s-n" . counsel-org-capture-string)
 ("s-o" . (lambda () (interactive) (switch-to-buffer (other-buffer))))
 ("s-u" . exwm-reset)
 ("s-p" . exwm-window-go-previous-hidden-workspace)
 ("s-r" . frame-workflow-reload-layout)
 ("s-s" . helm-frame-workflow)
 ("s-v" . toggle-window-split)
 ("s-w" . akirak/exwm-goto-browser)
 ("s-y" . window-go-other-buffer-in-split-window)
 ("s-z" . counsel-org-clock-context)
 ("s-," . winner-undo)
 ("s-." . winner-redo)
 ("s-[" . exwm-window-go-previous-visible-workspace)
 ("s-]" . exwm-window-go-next-visible-workspace)
 ("s-\\" . katawa-ivy-exwm)
 ("s--" . delete-other-windows)
 ("s-=" . balance-windows)
 ("s-SPC" . akirak/switch-window)
 ("M-<f2>" . akirak/counsel-external-command)
 ("M-S-<f2>" . counsel-linux-app)
 ("M-<f4>" . kill-this-buffer-and-its-window)
 ("<s-return>" . switch-window-then-swap-buffer)
 ("<s-backspace>" . keyboard-quit)
 ("s-0" . delete-window))

;; Bind s-g N to Nth EXWM workspace
(require 'init-frames)
(dolist (i (number-sequence 0 9))
  (define-key akirak/frame-map (int-to-string i)
    `(lambda () (interactive) (exwm-workspace-switch ,i))))

;;;; Keybindings in exwm-mode-map
(general-def exwm-mode-map
  "s-i" #'exwm-input-release-keyboard)

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

(provide 'init-exwm-bindings)
