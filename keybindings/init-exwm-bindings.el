(require 'exwm-input)
(require 'init-capture-map)

(use-package window-go
  :straight (window-go :host github :repo "akirak/emacs-window-go"))

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
 ("s-H" . window-go-shrink)
 ("s-L" . window-go-grow)
 ("s-S" . exwm-workspace-move-window)
 ("s-b" . exwm-workspace-switch-to-buffer)
 ("s-c" . akirak/capture-map)
 ("s-d" . (lambda () (interactive) (org-clock-goto) (delete-other-windows)))
 ("s-h" . window-go-shrink-horizontally)
 ("s-j" . other-window)
 ("s-k" . akirak/previous-window)
 ("s-l" . window-go-grow-horizontally)
 ("s-m" . akirak/select-master-window)
 ("s-n" . akirak/exwm-next-workspace)
 ("s-o" . (lambda () (interactive) (switch-to-buffer (other-buffer))))
 ("s-p" . akirak/exwm-previous-workspace)
 ("s-s" . exwm-workspace-switch)
 ("s-t" . multi-term-dedicated-open)
 ("s-u" . exwm-reset)
 ("s-v" . toggle-window-split)
 ("s-w" . akirak/exwm-goto-browser)
 ("s-x" . akirak/exwm-run-shell-command)
 ("s-y" . (lambda () (interactive) (window-go-split-sensibly '(16))))
 ("s-^" . treemacs)
 ("s-6" . akirak/first-file-window)
 ("s-7" . windmove-left)
 ("s-8" . windmove-right)
 ("s-9" . akirak/bottom-window)
 ("s-," . winner-undo)
 ("s-." . winner-redo)
 ("s--" . delete-other-windows)
 ("s-S-SPC" . balance-windows)
 ("s-SPC" . akirak/exwm-window-command-map)
 ("M-<f2>" . counsel-linux-app)
 ("M-<f4>" . kill-this-buffer-and-its-window)
 ("<s-return>" . akirak/swap-master-window)
 ("<s-insert>" . exwm-workspace-add)
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
