(general-def :keymaps 'org-mode-map :package 'org
  "M-n" 'org-metadown
  "M-p" 'org-metaup
  "M-H" 'org-shiftmetaleft
  "M-L" 'org-shiftmetaright
  "C-1" 'counsel-org-tag
  ;; TODO: Create hydra commands
  ;; "C-2" 'org-time-hydra
  ;; "C-3" 'org-edna-hydra
  "C-4" 'org-starter-refile-by-key
  "C-6" 'akirak/org-export-subtree-to-hugo-dwim
  "C-8" 'org-insert-hydra/body
  "C-9" #'org-tree-to-indirect-buffer)

(defun akirak/pop-up-org-clocking-task ()
  (interactive)
  (split-window-below)
  (other-window 1)
  (org-clock-goto))

(provide 'init-org-bindings)
