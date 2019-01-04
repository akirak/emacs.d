(use-package helm-org-rifle
  :after (helm org)
  :config
  ;; Define Helm actions to insert a link.
  ;; Note that these actions are effective only in org-mode and its
  ;; derived modes.
  (add-to-list 'helm-org-rifle-actions
               '("Insert link"
                 . helm-org-rifle--insert-link)
               t)
  (add-to-list 'helm-org-rifle-actions
               '("Insert link with custom ID"
                 . helm-org-rifle--insert-link-with-custom-id)
               t)
  (add-to-list 'helm-org-rifle-actions
               '("Store link"
                 . helm-org-rifle--store-link)
               t)
  (add-to-list 'helm-org-rifle-actions
               '("Store link with custom ID"
                 . helm-org-rifle--store-link-with-custom-id)
               t)
  (defun helm-org-rifle--store-link (candidate &optional use-custom-id)
    "Store a link to CANDIDATE."
    (-let (((buffer . pos) candidate))
      (with-current-buffer buffer
        (org-with-wide-buffer
         (goto-char pos)
         (when (and use-custom-id
                    (not (org-entry-get nil "CUSTOM_ID")))
           (org-set-property "CUSTOM_ID"
                             (read-string (format "Set CUSTOM_ID for %s: "
                                                  (substring-no-properties
                                                   (org-format-outline-path
                                                    (org-get-outline-path t nil))))
                                          (helm-org-rifle--make-default-custom-id
                                           (nth 4 (org-heading-components))))))
         (call-interactively 'org-store-link)))))
  (defun helm-org-rifle--store-link-with-custom-id (candidate)
    "Store a link to CANDIDATE with a custom ID.."
    (helm-org-rifle--store-link candidate 'use-custom-id))
  (defun helm-org-rifle--insert-link (candidate &optional use-custom-id)
    "Insert a link to CANDIDATE."
    (unless (derived-mode-p 'org-mode)
      (user-error "Cannot insert a link into a non-org-mode"))
    (let ((orig-marker (point-marker)))
      (helm-org-rifle--store-link candidate use-custom-id)
      (-let (((dest label) (pop org-stored-links)))
        (org-goto-marker-or-bmk orig-marker)
        (org-insert-link nil dest label)
        (message "Inserted a link to %s" dest))))
  (defun helm-org-rifle--make-default-custom-id (title)
    (downcase (replace-regexp-in-string "[[:space:]]" "-" title)))
  (defun helm-org-rifle--insert-link-with-custom-id (candidate)
    "Insert a link to CANDIDATE with a custom ID."
    (helm-org-rifle--insert-link candidate t))
  :general
  (:keymaps 'org-mode-map
            "M-s r" #'helm-org-rifle-current-buffer)
  :custom
  (helm-org-rifle-directories-recursive nil)
  (helm-org-rifle-show-path t)
  (helm-org-rifle-test-against-path t))

;;;; helm-org-rifle + org-recent-headings

;; This feature is currently disabled.
;; TODO: Defer loading
;; (require 'init-org-recent-headings)
;; (require 'helm-org-rifle)
;; (helm-org-rifle-define-command
;;  "agenda-files-with-recent-headings" ()
;;  "Rifile with org-recent-headings prepended."
;;  :sources (cons helm-source-org-recent-headings
;;                 (mapcar 'helm-org-rifle-get-source-for-file (org-agenda-files))))

(provide 'setup-helm-org-rifle)
