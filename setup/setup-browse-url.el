;; -*- lexical-binding: t -*-

;;;; URL bookmarks
(defvar akirak/browse-url-bookmarks
  '(("Syncthing" . "http://localhost:8384")))

(defvar akirak/browse-url-bookmarks-read nil)

(defcustom akirak/browse-url-bookmarks-file
  (no-littering-expand-var-file-name "browse-url/bookmarks.el")
  "File name of the bookmark file."
  :type 'file)

(defun akirak/browse-url-bookmarks ()
  (or akirak/browse-url-bookmarks
      (unless akirak/browse-url-bookmarks-read
        (prog1 (setq akirak/browse-url-bookmarks
                     (ignore-errors
                       (with-temp-buffer
                         (insert-file-contents akirak/browse-url-bookmarks-file)
                         (read (point-min)))))
          (setq akirak/browse-url-bookmarks-read t)))))

(defun akirak/browse-url-bookmarks-add (url)
  (unless (and (akirak/browse-url-bookmarks)
               (rassoc url akirak/browse-url-bookmarks))
    (let* ((html (org-web-tools--get-url url))
           (title (string-trim
                   (read-string "Title of the bookmark (empty to skip): "
                                (org-web-tools--html-title html)))))
      (unless (string-empty-p title)
        (add-to-list 'akirak/browse-url-bookmarks (cons title url))
        (with-temp-buffer
          (princ akirak/browse-url-bookmarks (current-buffer))
          (write-file akirak/browse-url-bookmarks-file))))))

;;;; URL history

(defcustom akirak/browse-url-history-file
  (no-littering-expand-var-file-name "browse-url/history")
  "History"
  :type 'file)

(defun akirak/browse-url-history-get ()
  (when (file-exists-p akirak/browse-url-history-file)
    (ignore-errors
      (with-temp-buffer
        (insert-file-contents akirak/browse-url-history-file)
        (nreverse (split-string (buffer-substring-no-properties
                                 (point-min) (point-max))
                                "\n"))))))

(defun akirak/browse-url-history-add (url)
  (let ((dir (file-name-directory akirak/browse-url-history-file)))
    (unless (file-directory-p dir)
      (make-directory dir)))
  (with-temp-buffer
    (insert url "\n")
    (append-to-file (point-min) (point-max)
                    akirak/browse-url-history-file)))

;;;; browse-url function

(defun akirak/browse-url (url &optional _new-window)
  (akirak/browse-url-history-add url)
  (when akirak/to-be-run-as-exwm
    (message "Opening %s. Choose a target window" url)
    (ace-window nil))
  (cond
   ((akirak/file-url-p url)
    (akirak/browse-url-for-referencing url))
   ((akirak/local-url-p url)
    (akirak/browse-url-bookmarks-add url)
    (akirak/browse-url-incognito url))
   ((akirak/incognito-url-p url)
    (akirak/browse-url-incognito url))
   (browse-url-generic-program
    (browse-url-generic url))
   ((eq system-type 'gnu/linux)
    (browse-url-xdg-open url))))

(defvar akirak/read-url-history nil)

(defun akirak/browse-url-or-query (url-or-query)
  (interactive (list (pcase current-prefix-arg
                       ('(16) (completing-read "URL from history and bookmarks: "
                                               (append (akirak/browse-url-history-get)
                                                       akirak/browse-url-bookmarks)))
                       (_ (let ((url (akirak/parse-web-query
                                      (completing-read "Port or URL: "
                                                       (akirak/browse-url-bookmarks)
                                                       nil nil
                                                       (and (region-active-p)
                                                            (buffer-substring-no-properties
                                                             (region-beginning) (region-end)))
                                                       akirak/read-url-history))))
                            (when (or (akirak/local-url-p url)
                                      current-prefix-arg)
                              (akirak/browse-url-bookmarks-add url))
                            url)))))
  (akirak/browse-url (akirak/parse-web-query url-or-query)))

(defvar akirak/web-query-history nil)

(defun akirak/parse-web-query (query)
  (add-to-list 'akirak/web-query-history query)
  (let* ((port (ignore-errors (string-to-number query)))
         (github-repo-path-pattern (rx (group (+ (any alnum "-")))
                                       "/"
                                       (group (+ (any alnum "-_."))))))
    (cond
     ((and (numberp port) (/= 0 port))
      (format "http://localhost:%d%s" port (read-string "Path: ")))
     ((ffap-url-p query)
      query)
     ((string-match-p github-repo-path-pattern query)
      (concat "https://github.com/" query))
     ((string-match-p akirak/https-url-shorthand-pattern query)
      (concat "https://" query))
     ((string-match (rx bol "@" (group (+ (any alnum "-_"))) eol) query)
      (concat "https://github.com/" (match-string 1 query)))
     ((string-match (rx bol "/r/") query)
      (concat "https://reddit.com" query))
     (t
      (let ((words (split-string query)))
        (format "https://duckduckgo.com/?q=%s"
                (mapconcat #'url-hexify-string words "+")))))))

(defconst akirak/local-url-pattern
  (rx bol
      (or "file:"
          (and "http://" (or "penguin.linux.test" "localhost"
                             (or "127.0.0.1"
                                 (and "192" (repeat 3 (and "." (+ digit))))))
               (?  ":" (+ digit))
               (?  "/" (* anything)) eol))))

(defun akirak/local-url-p (url)
  (string-match-p akirak/local-url-pattern url))

(defconst akirak/https-url-shorthand-pattern
  (rx bol (+? (+ (any alnum "-")) ".") (+ (any alpha))
      (or eol "/")))

(setq-default browse-url-generic-program
              (cond
               ((and (akirak/running-on-crostini-p)
                     (not akirak/to-be-run-as-exwm))
                "garcon-url-handler")
               (t
                ;; (or (executable-find "brave")
                ;;     (executable-find "chromium"))
                "xdg-open")))

(setq-default browse-url-browser-function 'akirak/browse-url)

;;;; Variations of browse-url functions

;;;;; Incognito
(defalias 'akirak/browse-localhost 'akirak/browse-url)

(defun akirak/browse-url-incognito (url)
  (let ((browse-url-chromium-arguments '("--incognito" "--new-window")))
    (browse-url-chromium url)))

(defcustom akirak/incognito-domain-list nil
  "List of domains that should be browsed in incognito."
  :type '(repeat string))

(defun akirak/incognito-url-p (url)
  (and akirak/incognito-domain-list
       (string-match-p (rx bol "https://" (? "www.")
                           (eval `(or ,@akirak/incognito-domain-list)))
                       url)))

;;;;; Keyboard-oriented browser for reading documentation

(defcustom akirak/browse-url-generic-program-for-referencing
  (or (executable-find "next")
      (executable-find "qutebrowser")
      browse-url-generic-program)
  "Web browser program for reading referential documentation."
  :type 'string)

(defcustom akirak/browse-url-generic-args-for-referencing
  (let ((program akirak/browse-url-generic-program-for-referencing))
    (cond
     ((and program
           (string-match-p (rx (or (and bol "next" eol)
                                   (and "/next" eol)))
                           program))
      '("--no-session"))))
  "Command line arguments for `akirak/browse-url-generic-program-for-referencing'."
  :type '(repeat string))

(defun akirak/browse-url-for-referencing (url)
  (let ((browse-url-generic-program akirak/browse-url-generic-program-for-referencing)
        (browse-url-generic-args
         akirak/browse-url-generic-args-for-referencing))
    (browse-url-generic url)))

(defun akirak/file-url-p (url)
  (string-match-p (rx bol "file://") url))

(provide 'setup-browse-url)
