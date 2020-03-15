(require 'helm)
(require 'generic)

(cl-defgeneric akirak/helm-action-switch-same-window (obj)
  "Switch to OBJ in the same window.")

(cl-defgeneric akirak/helm-action-switch-select-1 (obj)
  "Switch to OBJ in the same window or in a selected window."
  (when current-prefix-arg
    (ace-window nil))
  (akirak/helm-action-switch-same-window))

(cl-defgeneric akirak/helm-action-switch-other-window (obj)
  "Switch to OBJ in another window.")

(cl-defgeneric akirak/helm-action-switch-ace-window (obj)
  "Run `ace-window' and then switch to OBJ."
  (ace-window nil)
  (akirak/helm-switch-same-window obj))

(defconst akirak/helm-switch-actions-default-same-window
  (helm-make-actions
   "Switch in the same window"
   #'akirak/helm-action-switch-same-window
   "Switch in other window"
   #'akirak/helm-action-switch-same-window))

(defconst akirak/helm-switch-actions-default-switch-select
  (helm-make-actions
   "Switch in the same window or in a selected window"
   #'akirak/helm-action-switch-select-1
   "Switch in other window"
   #'akirak/helm-action-switch-same-window))

(provide 'my/helm/action/generic)
