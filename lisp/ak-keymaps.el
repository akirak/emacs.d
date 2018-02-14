;;;; Shell-like keybindings
(general-define-key "C-h" 'backward-delete-char
                    ;; If a region is active, run `kill-region'.
                    ;; Otherwise, run `backward-kill-word'.
                    "C-w" (general-predicate-dispatch 'backward-kill-word
                            (region-active-p) 'kill-region)
                    "C-a" (general-predicate-dispatch 'beginning-of-line
                            (bolp) 'back-to-indentation))

(general-define-key :keymaps 'minibuffer-local-map
                    "C-u" 'backward-kill-sentence
                    "C-w" 'backward-kill-word
                    "C-h" 'backward-delete-char)

(general-define-key :keymaps 'ivy-minibuffer-map :package 'ivy
                    "C-r" 'counsel-minibuffer-history
                    "C-w" 'ivy-backward-kill-word
                    "C-u" 'ivy-backward-kill-sentence)

(general-define-key :keymaps 'counsel-find-file-map :package 'counsel
                    "C-h" 'ivy-backward-delete-char)

(general-define-key :keymaps 'helm-map :package 'helm
                    "C-u" 'backward-kill-sentence
                    "C-w" 'backward-kill-word
                    "C-k" 'kill-line)

(provide 'ak-keymaps)
