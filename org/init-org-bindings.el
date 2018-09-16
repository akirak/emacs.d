(general-def :keymaps 'org-mode-map :package 'org
  ;; I don't use any of these bindings and want to use them for other purposes
  "C-c [" nil
  "C-c ]" nil
  "M-n" 'org-metadown
  "M-p" 'org-metaup
  "M-H" 'org-shiftmetaleft
  "M-L" 'org-shiftmetaright
  "C-1" 'counsel-org-tag
  ;; TODO: Create hydra commands
  ;; "C-2" 'org-time-hydra
  ;; "C-3" 'org-edna-hydra
  "C-4" 'org-starter-refile-by-key
  "C-6" 'akirak/org-export-hydra/body
  "C-8" 'org-insert-hydra/body
  "C-9" #'org-tree-to-indirect-buffer)

(defun akirak/pop-up-org-clocking-task ()
  (interactive)
  (split-window-below)
  (other-window 1)
  (org-clock-goto))

(defhydra akirak/org-export-hydra (:exit t :hint nil)
  "
^^Documents  ^^Graph
^^---------  ^^-------------------
_h_ hugo     _gb_ mindmap (buffer)
^^           _gt_ mindmap (tree)
"
  ("h" akirak/org-export-subtree-to-hugo-dwim)
  ("gb" org-mind-map-write)
  ("gt" org-mind-map-write-current-tree))

(provide 'init-org-bindings)
