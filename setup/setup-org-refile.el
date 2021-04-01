;; (require 'cl-lib)
;; (require 'org)
(require 'dash)

(defun akirak//org-propertize-path (segs)
  "Build a propertized refile path from SEGS."
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
  "Modify TARGETS of `org-refile' for better presentation."
  ;; Modify the targets if and only if you use full file paths.
  (if (eq org-refile-use-outline-path 'full-file-path)
      ;; Modify the path (the first element) in each candidate.
      ;; Its file name is used as a hint, and the rest are irrelevant.
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

;; Stop advising `org-refile-targets', as the table is used later for creating
;; a new node. Advise `completing-read' temporarily instead.
;; (advice-add #'org-refile-get-targets :filter-return #'akirak/ad-filter-org-refile-targets)

(defun akirak/ad-around-org-refile-completing-read (oldfun prompt candidates &rest r)
  "Advice function for `completing-read' for use inside `org-refile-get-location'."
  (if (eq org-refile-use-outline-path 'full-file-path)
      (let* ((modified-candidates (akirak/ad-filter-org-refile-targets candidates))
             ;; Store the mapping to support symlinked targets
             (alist (-zip (mapcar #'car modified-candidates)
                          (mapcar #'car candidates)))
             (raw (apply oldfun prompt modified-candidates r)))
        (or (cdr (assoc raw alist))
            (expand-file-name (substring-no-properties raw))))
    (apply oldfun prompt candidates r)))

(defun akirak/ad-around-org-refile-get-location (oldfun &rest r)
  "Around advice for `org-refile-get-location'."
  (advice-add #'completing-read :around #'akirak/ad-around-org-refile-completing-read)
  (unwind-protect
      (apply oldfun r)
    (advice-remove #'completing-read #'akirak/ad-around-org-refile-completing-read)))

(advice-add #'org-refile-get-location :around #'akirak/ad-around-org-refile-get-location)

(defun akirak/org-refile-target-verify-function ()
  (let ((element (org-element-headline-parser (line-end-position))))
    (not (or (eq (org-element-property :todo-type element) 'done)
             (equal (org-element-property :todo-keyword element) "COMMENT")
             (member (org-element-property :tags element) '("ARCHIVE"
                                                            "link"))))))

(setq-default org-refile-target-verify-function #'akirak/org-refile-target-verify-function)

(provide 'setup-org-refile)
