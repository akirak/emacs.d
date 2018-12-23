(defun akirak/kill-region-or-backward-kill-word ()
  "If a region is active, run `kill-region'. Otherwise, run `backward-kill-word'."
  (lambda (&optional arg) (interactive "p")
    (if (region-active-p)
        (kill-region (region-beginning) (region-end))
      (backward-kill-word arg))))

(defun akirak/back-to-indentation-or-beginning-of-line ()
  (lambda (&optional arg) (interactive "P")
    (if (or arg (bolp))
        (back-to-indentation)
      (beginning-of-line))))

(general-def
  "C-h" 'backward-delete-char
  "C-w" 'akirak/kill-region-or-backward-kill-word)

(general-def prog-mode-map
  "C-a" 'akirak/back-to-indentation-or-beginning-of-line)

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
