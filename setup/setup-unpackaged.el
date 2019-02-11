(use-package unpackaged
  :straight (unpackaged :host github :repo "alphapapa/unpackaged.el")
  :config
  (unpackaged/magit-log-date-headers-mode 1))

(defun akirak/ad-before-org-return (&optional _indent)
  ;; Empty row: end the table.
  ;; Stolen from a corresponding part in unpackaged/org-return-dwim by alphapapa.
  ;; https://github.com/alphapapa/unpackaged.el#org-return-dwim
  (when (and (org-at-table-p)
             (save-excursion
               (beginning-of-line)
               ;; See `org-table-next-field'.
               (cl-loop with end = (line-end-position)
                        for cell = (org-element-table-cell-parser)
                        always (equal (org-element-property :contents-begin cell)
                                      (org-element-property :contents-end cell))
                        while (re-search-forward "|" end t))))
    (delete-region (line-beginning-position) (line-end-position))))

(advice-add 'org-return :before #'akirak/ad-before-org-return)

(provide 'setup-unpackaged)
