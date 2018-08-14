;; Import utilities for org-capture
(require 'init-org-capture)

(use-package org-starter
  :straight (org-starter :host github :repo "akirak/org-starter")
  :functions (org-starter-define-directory org-starter-define-file)
  :config
  (defun helm-org-rifle-known-files ()
    (interactive)
    (helm-org-rifle-files org-starter-known-files))
  ;; Based on http://www.howardism.org/Technical/Emacs/capturing-content.html
  (general-setq
   org-starter-initial-capture-templates
   `(("i" "Add an item to the clocked task" item
      (clock)
      "%i%?" :empty-lines 1)
     ("@" "To the clocked task")
     ("@t" "Sub-task of the clocked task" entry
      (clock)
      ,(akirak/org-capture-entry-template-1 "%i%?" ""
                                            :todo "TODO")
      :clock-in t :clock-resume t)
     ("d" "To the default notes file")
     ("dt" "Task in the default notes file" entry
      (file nil)
      ,(akirak/org-capture-entry-template-1 "%i%?" ""
                                            :todo "TODO"))
     ;; ("K" "Memorize the kill-ring content (to the clock)" plain
     ;;  (clock)
     ;;  "%c" :immediate-finish t :empty-lines 1)
     ;; ("A" "Append text to the clocked task" plain
     ;;  (clock)
     ;;  "%i" :immediate-finish t :empty-lines 1)
     ))
  (setq org-capture-templates-contexts
        `(("t" "@t" (org-clocking-p))
          ("t" "dt" ((lambda () (not (org-clocking-p)))))
          ;; Disable templates with the clock target when not clocking in
          ("@" (org-clocking-p))
          ,@(cl-loop for (key _ _ target) in org-starter-initial-capture-templates
                     when (equal target '(clock))
                     collect `(,key (org-clocking-p)))))
  :custom
  (org-starter-exclude-from-recentf '(known-files path))
  (org-starter-alternative-find-function #'helm-org-rifle-files)
  (org-starter-extra-find-file-map
   '(("j" (lambda () (interactive) (counsel-ag nil "~/personal/org-journal")) "search journal")))
  (org-starter-extra-refile-map
   '(("j" org-refile-to-journal "journal")
     ("'" avy-org-refile-as-child "avy")
     ("?" org-refile-same-buffer "in-buffer")
     ("o" org-refile-other-window-files "other window buffers")
     ("@" (lambda () (interactive) (org-refile 2)) "clock")))
  (org-starter-extra-alternative-find-file-map
   '(("SPC" helm-org-rifle-known-files "all"))))

(use-package helm-org-starter
  :straight org-starter
  :commands (helm-org-starter)
  :defines (helm-org-starter-known-file-source))

(use-package counsel-org-starter
  :straight org-starter
  :commands (counsel-org-starter counsel-org-starter-known-file)
  :config
  (ivy-add-actions 'counsel-org-starter
                   '(("r"
                      (lambda (file)
                        (helm-org-rifle-files (org-starter-locate-file file nil t)))
                      "rifle"))))

(provide 'init-org-starter)
