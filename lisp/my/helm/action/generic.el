(require 'helm)
(require 'generic)

(cl-defgeneric akirak/helm-action-switch-same-window (obj)
  "Switch to OBJ in the same window.")

(cl-defgeneric akirak/helm-action-switch-select-1 (obj)
  "Switch to OBJ in the same window or in a selected window."
  (when current-prefix-arg
    (ace-window nil))
  (akirak/helm-action-switch-same-window obj))

(cl-defgeneric akirak/helm-action-switch-other-window (obj)
  "Switch to OBJ in another window.")

(cl-defgeneric akirak/helm-action-switch-ace-window (obj)
  "Run `ace-window' and then switch to OBJ."
  (ace-window nil)
  (akirak/helm-switch-same-window obj))

(cl-defgeneric akirak/helm-action-dired-jump (obj))

(cl-defgeneric akirak/helm-action-find-file-interactively (obj))

(defconst akirak/helm-switch-actions-default-same-window
  (helm-make-actions
   "Switch in the same window"
   #'akirak/helm-action-switch-same-window
   "Switch in other window"
   #'akirak/helm-action-switch-same-window
   "Dired find file"
   #'akirak/helm-action-dired-jump
   "Find file interactively"
   #'akirak/helm-action-find-file-interactively))

(defconst akirak/helm-switch-actions-default-switch-select
  (helm-make-actions
   "Switch in the same window or in a selected window"
   #'akirak/helm-action-switch-select-1
   "Switch in other window"
   #'akirak/helm-action-switch-same-window
   "Dired find file"
   #'akirak/helm-action-dired-jump
   "Find file interactively"
   #'akirak/helm-action-find-file-interactively))

(defvar akirak/helm-file-like-source-keymap
  (let ((m (make-composed-keymap nil helm-map)))
    (define-key m (kbd "C-x C-j")
      (lambda ()
        (interactive)
        (helm-exit-and-execute-action #'akirak/helm-action-dired-jump)))
    (define-key m (kbd "C-x C-f")
      (lambda ()
        (interactive)
        (helm-exit-and-execute-action #'akirak/helm-action-find-file-interactively)))
    m))

(provide 'my/helm/action/generic)
