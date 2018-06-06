(use-package crux)

(use-package fwb-cmds
  :straight (fwb-cmds :host github :repo "tarsius/fwb-cmds"))

(use-package restart-emacs)

(use-package ivy-bookmarked-directory
  :straight (ivy-bookmarked-directory :host github
                                      :repo "akirak/ivy-bookmarked-directory")
  :commands (ivy-bookmarked-directory)
  :config
  (ivy-add-actions 'ivy-bookmarked-directory
                   '(("m" (lambda (cand)
                            (let ((default-directory cand))
                              (multi-term)))
                      "multi-term")
                     ("f" (lambda (cand)
                            (frame-purpose-make-directory-frame cand))
                      "Make a frame with frame-purpose")
                     ("s" (lambda (cand)
                            (counsel-ag nil cand))
                      "Run ag")
                     ("!" (lambda (cand)
                            (let ((default-directory cand))
                              (call-interactively 'shell-command)))
                      "Run shell command"))))

(require 'init-helpful)

(provide 'init-commands)
