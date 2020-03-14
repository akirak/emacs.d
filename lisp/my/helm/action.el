(defvar akirak/switch-buffer-helm-actions
  (quote (("Switch to buffer" .
           (lambda (buffer)
             (when current-prefix-arg
               (ace-window nil))
             (switch-to-buffer buffer)))
          ("Kill buffer" . kill-buffer)
          ;; TODO: Add find-file-dired action (C-x C-j is better)
          )))

(defvar akirak/find-file-helm-actions
  (quote (("Find file" .
           (lambda (file)
             (when current-prefix-arg
               (ace-window nil))
             (find-file file)))
          ;; TODO: Add find-file-dired action (C-x C-j is better)
          )))

(provide 'my/helm/action)
