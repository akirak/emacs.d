(require 'cl-lib)
(require 'org)

(defun akirak//org-propertize-path (segs)
  (cl-loop for seg being the elements of segs using (index i)
           concat (concat
                   "/"
                   (if (and seg (< i 7))
                       (propertize seg
                                   'face
                                   (intern (concat "org-level-"
                                                   (int-to-string (1+ i)))))
                     seg))))

(defun akirak/ad-filter-org-refile-targets (targets)
  (if (eq org-refile-use-outline-path 'full-file-path)
      (cl-loop for (path filename . rest) in targets
               collect (cons (let* ((nondir (file-name-nondirectory filename))
                                    (pos (string-match (regexp-quote nondir) path))
                                    (path-sans-file (substring path (+ pos (seq-length nondir))))
                                    (segs (unless (string-empty-p path-sans-file)
                                            (cdr (split-string path-sans-file "/")))))
                               (concat
                                (abbreviate-file-name (file-name-directory filename))
                                (propertize nondir 'face 'bold)
                                (akirak//org-propertize-path segs)))
                             (cons filename rest)))
    targets))

(advice-add #'org-refile-get-targets :filter-return #'akirak/ad-filter-org-refile-targets)

(provide 'akirak-org-refile-path)
