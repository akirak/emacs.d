;; -*- lexical-binding: t; -*-
(defvar akirak/org-agenda-buffer-source
  (helm-build-sync-source " Org Agenda / Org Ql Search buffers"
    :candidates
    (->> (buffer-list)
         (-filter (lambda (buf)
                    (eq 'org-agenda-mode (buffer-local-value 'major-mode buf))))
         (-map (lambda (buf) (cons (buffer-name buf) buf))))
    :action
    (helm-make-actions
     "Switch to buffer" #'switch-to-buffer
     "Switch to buffer in other window" #'switch-to-buffer-other-window
     "Switch to buffer in other tab" #'switch-to-buffer-other-tab)))

(provide 'my/helm/source/org-agenda)
