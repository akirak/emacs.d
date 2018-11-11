(require 'bind-key)

(bind-keys* ("C-h" . backward-delete-char)
            ;; If a region is active, run `kill-region'.
            ;; Otherwise, run `backward-kill-word'.
            ("C-w" . (lambda (&optional arg) (interactive "p")
                       (if (region-active-p)
                           (kill-region (region-beginning) (region-end))
                         (backward-kill-word arg)))))

(bind-keys :map prog-mode-map
           ("C-a" . (lambda (&optional arg) (interactive "P")
                      (if (or arg (bolp))
                          (back-to-indentation)
                        (beginning-of-line)))))

(bind-keys :map minibuffer-local-map
           ("C-u" . backward-kill-sentence))

(with-eval-after-load 'ivy
  (bind-keys :map ivy-minibuffer-map
             ("C-r" . counsel-minibuffer-history)
             ("C-w" . ivy-backward-kill-word)
             ("C-u" . ivy-backward-kill-sentence)))

(provide 'init-shell-bindings)
