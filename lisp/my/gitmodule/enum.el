;; -*- lexical-binding: t -*-

;; TODO: Use git-config command to retrieve the data
(defun akirak/git-submodule-alist (config-file)
  (when (f-exists-p config-file)
    (with-temp-buffer
      (insert-file-contents config-file)
      (let (positions result start)
        (goto-char (point-max))
        (while (re-search-backward (rx bol "[submodule" (+ space)) nil t)
          (push (point) positions))
        (while (setq start (pop positions))
          (goto-char start)
          (let* ((bound (car positions))
                 (url (save-excursion
                        (re-search-forward (rx bol (+ space) "url" (* space) "="
                                               (* space))
                                           bound)
                        (buffer-substring-no-properties (point) (line-end-position))))
                 (path (progn
                         (re-search-forward (rx bol (+ space) "path" (* space) "="
                                                (* space))
                                            bound)
                         (buffer-substring-no-properties (point) (line-end-position)))))
            (push (cons path url)
                  result)))
        (nreverse result)))))

(defsubst akirak/git-all-submodule-alist (root)
  (akirak/git-submodule-alist (f-join root ".gitmodules")))

(defsubst akirak/git-available-submodule-alist (root)
  (akirak/git-submodule-alist (f-join root ".git" "config")))

(defun akirak/git-submodules-parse (&optional file)
  (->> (apply #'process-lines "git" "config" "--list"
              (when file
                (list "--file" file)))
       (-map (lambda (s)
               (-some-> (s-match (rx bol
                                     "submodule."
                                     (group (+ (not (char "="))))
                                     "."
                                     (group (+ (not (char "="))))
                                     "="
                                     (group (+ anything))
                                     eol)
                                 s)
                 (cdr))))
       (delq nil)
       (-group-by #'car)
       (-map (lambda (xs) (cons (car xs)
                                (cl-loop for (_name key value) in (cdr xs)
                                         collect (cons (intern key)
                                                       value)))))))

(defun akirak/git-submodules-full-alist (root)
  "Return both active and inactive submodules in ROOT."
  (let ((default-directory root)
        (modules-file ".gitmodules"))
    (when (f-exists-p modules-file)
      (let* ((all-alist (akirak/git-submodules-parse modules-file))
             (active-alist (akirak/git-submodules-parse))
             (f (-partial (lambda (active-alist module-cell)
                            (let* ((name (car module-cell))
                                   (props1 (cdr module-cell))
                                   (props2 (cdr-safe (assoc name active-alist)))
                                   (location (f-join root (alist-get 'path props1))))
                              (cons name
                                    (append `((location . ,location)
                                              (name . ,name)
                                              (root . ,root))
                                            props2
                                            props1))))
                          active-alist)))
        (-map f all-alist)))))

(defun akirak/toplevel-repos-submodules ()
  (->> (f-directories "~" (lambda (dir)
                            (and (not (string-match-p "^\\." (f-filename dir)))
                                 (f-exists-p (f-join dir ".git")))))
       (-map #'akirak/git-submodules-full-alist)
       (-flatten-n 1)
       (-map #'cdr)))

(provide 'my/gitmodule/enum)
