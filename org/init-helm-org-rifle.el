(require 'init-helm)

(use-package helm-org-rifle
  :after (helm org)
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

(provide 'init-helm-org-rifle)
