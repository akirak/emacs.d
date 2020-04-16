(general-def
  "C-h" 'backward-delete-char
  "C-w"
  (defun akirak/kill-region-or-backward-kill-word (&optional arg)
    "If a region is active, run `kill-region'. Otherwise, run `backward-kill-word'."
    (interactive "p")
    (if (region-active-p)
        (kill-region (region-beginning) (region-end))
      (backward-kill-word arg))))

(general-def prog-mode-map
  "C-a"
  (defun akirak/back-to-indentation-or-beginning-of-line (&optional arg)
    (interactive "P")
    (if (and (not arg)
             (or (looking-at "^")
                 (string-match-p (rx (not (any space)))
                                 (buffer-substring-no-properties
                                  (line-beginning-position)
                                  (point)))))
        (back-to-indentation)
      (beginning-of-line))))

(general-def :keymaps 'org-mode-map :package 'org
  "C-a" #'org-beginning-of-line
  "C-e" #'org-end-of-line)

(general-def minibuffer-local-map
  "C-u" #'backward-kill-sentence
  "C-w" #'backward-kill-word)

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
