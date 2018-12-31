(use-package ivy-bookmarked-directory
  :straight (ivy-bookmarked-directory :host github
                                      :repo "akirak/ivy-bookmarked-directory")
  :commands (ivy-bookmarked-directory)
  :config
  (ivy-add-actions 'ivy-bookmarked-directory
                   '(("f" counsel-find-file
                      "find-file")
                     ("m" (lambda (cand)
                            (let ((default-directory cand))
                              (akirak/shell-new)))
                      "term")
                     ("F" (lambda (cand)
                            (frame-purpose-make-directory-frame cand))
                      "Make a frame with frame-purpose")
                     ("p" (lambda (cand)
                            (let ((default-directory cand)
                                  (projectile-cached-buffer-file-name nil)
                                  (projectile-cached-project-root nil))
                              (projectile-find-file)))
                      "projectile-find-file")
                     ("s" (lambda (cand)
                            (counsel-ag nil cand))
                      "Run ag")
                     ("!" (lambda (cand)
                            (let ((default-directory cand))
                              (call-interactively 'shell-command)))
                      "Run shell command"))))

(provide 'init-commands)
