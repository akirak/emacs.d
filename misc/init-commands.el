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
                              (multi-term))) "multi-term"))))

(require 'init-helpful)

(provide 'init-commands)
