(general-def :keymaps 'swiper-map :package 'swiper
  ;; Allow dispatching occur from a swiper session.
  "C-M-o" (defun akirak/swiper-occur ()
            (interactive)
            (ivy-exit-with-action
             (lambda (_) (occur ivy-text)))))

(general-def :keymaps 'counsel-ag-map :package 'counsel
  ;; Allow dispatching noccur from a counsel-rg session.
  "C-M-o" (defun akirak/counsel-ag-noccur ()
            (interactive)
            (ivy-exit-with-action
             (lambda (_)
               (message (ivy-state-directory ivy-last))
               (let ((akirak/deadgrep-root-directory (ivy-state-directory ivy-last)))
                 (deadgrep ivy-text))))))

(use-package noccur
  :disabled t)

(use-package deadgrep
  :commands (deadgrep)
  :config
  (advice-add #'deadgrep-visit-result
              :override
              (defun akirak/deadgrep-visit-result ()
                "Goto the search result at point."
                (interactive)
                (deadgrep--visit-result #'find-file-other-window)))

  ;; Allow explicitly setting the root directory of a deadgrep session.
  (defvar akirak/deadgrep-root-directory nil
    "Variable used to explicitly set the root directory.")
  (setq deadgrep-project-root-function
        (lambda ()
          (or akirak/deadgrep-root-directory
              (deadgrep--project-root))))

  ;; Add a command to replace matches in a deadgrep buffer.
  (defvar deadgrep-replace-history nil)
  (defun deadgrep-replace ()
    (interactive)
    (cl-assert (derived-mode-p 'deadgrep-mode))
    (let ((search-term deadgrep--search-term)
          (search-type deadgrep--search-type)
          (search-case deadgrep--search-case)
          (case-fold-search (cl-ecase deadgrep--search-case
                              ;; smart is not properly supported on Emacs.
                              (smart t)
                              (sensitive nil)
                              (ignore t)))
          files
          (initial-directory default-directory)
          (to-string (read-string "Replace with string: " nil deadgrep-replace-history)))
      (ignore-errors
        (save-excursion
          (goto-char (point-min))
          (while t
            (deadgrep-forward-filename)
            (push (deadgrep--filename) files))))
      (dolist (file (mapcar (lambda (file)
                              (expand-file-name file initial-directory))
                            files))
        (find-file file)
        (save-restriction
          (widen)
          (save-excursion
            (goto-char (point-min))
            (cl-ecase search-type
              (string
               (query-replace search-term to-string))
              (words
               (query-replace-regexp (rx-to-string
                                      `(bow ,search-term eow))
                                     to-string))
              (regexp
               (query-replace-regexp search-term to-string)))))
        (save-buffer))))

  (akirak/bind-mode :package 'magit :keymaps 'magit-status-mode-map
    "g" #'deadgrep)

  :general
  (:keymaps 'deadgrep-mode-map
            "R" #'deadgrep-replace))

(provide 'setup-occur)
