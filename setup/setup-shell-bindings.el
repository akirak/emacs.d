(use-package akirak/edit
  :straight nil)

(use-package akirak/org-edit
  :straight nil)

(general-def
  "C-h" 'backward-delete-char
  "C-w" 'akirak/kill-region-or-backward-kill-word)

(general-def prog-mode-map
  "C-a" 'akirak/back-to-indentation-or-beginning-of-line)

(general-def :keymaps 'org-mode-map :package 'org
  "C-a" #'org-beginning-of-line
  "C-e" #'org-end-of-line)

(general-def minibuffer-local-map
  "C-u" 'backward-kill-sentence)

(general-def ivy-minibuffer-map :package 'ivy
  "C-r" 'counsel-minibuffer-history
  "C-w" 'ivy-backward-kill-word
  "C-u" 'ivy-backward-kill-sentence)

(defun ivy-backward-kill-sentence ()
  (interactive)
  (if ivy--directory
      (progn (ivy--cd "/")
             (ivy--exhibit))
    (if (bolp)
        (kill-region (point-min) (point))
      (backward-kill-sentence))))

(provide 'setup-shell-bindings)
