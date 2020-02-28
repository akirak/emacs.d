;;;; URL bookmarks

(defvar akirak/browse-url-bookmarks
  '("http://localhost:8384"))

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
  (let ((orig-length (length akirak/browse-url-bookmarks)))
    (add-to-list 'akirak/browse-url-bookmarks url)
    (unless (eql orig-length (length akirak/browse-url-bookmarks))
      (with-temp-buffer
        (princ akirak/browse-url-bookmarks (current-buffer))
        (write-file akirak/browse-url-bookmarks-file)))))

;;;; URL history

(defcustom akirak/browse-url-history-file
  (no-littering-expand-var-file-name "browse-url/history")
  "History"
  :type 'file)

(defun akirak/browse-url-history-get ()
  (ignore-errors
    (with-temp-buffer
      (insert-file-contents akirak/browse-url-history-file)
      (nreverse (split-string (buffer-substring-no-properties
                               (point-min) (point-max))
                              "\n")))))

(defun akirak/browse-url-history-add (url)
  (with-temp-buffer
    (insert url "\n")
    (append-to-file (point-min) (point-max)
                    akirak/browse-url-history-file)))

;;;; browse-url function

(defun akirak/browse-url (url &optional _new-window)
  (interactive (list (pcase current-prefix-arg
                       ('(16) (completing-read "URL from history and bookmarks: "
                                               (append (akirak/browse-url-history-get)
                                                       akirak/browse-url-bookmarks)))
                       (_ (let ((url (akirak/read-url)))
                            (when (or (akirak/local-url-p url)
                                      current-prefix-arg)
                              (akirak/browse-url-bookmarks-add url))
                            url)))))
  (akirak/browse-url-history-add url)
  (when akirak/to-be-run-as-exwm
    (message "Opening %s. Choose a target window" url)
    (ace-window nil))
  (cond
   ((akirak/local-url-p url)
    (add-to-list 'akirak/localhost-url-list url)
    (browse-url-chromium url))
   (t (browse-url-generic url))))

(akirak/bind-search "M-g" #'akirak/browse-url)

(defvar akirak/read-url-history nil)

(defun akirak/read-url ()
  (let* ((port-or-url (completing-read "Port or URL: "
                                       (akirak/browse-url-bookmarks)
                                       nil nil
                                       (and (region-active-p)
                                            (buffer-substring-no-properties
                                             (region-beginning) (region-end)))
                                       akirak/read-url-history))
         (port (ignore-errors (string-to-number port-or-url))))
    (cond
     ((and (numberp port) (/= 0 port))
      (format "http://localhost:%d%s" port (read-string "Path: ")))
     ((ffap-url-p port-or-url)
      port-or-url)
     ((string-match-p akirak/github-repo-path-pattern port-or-url)
      (concat "https://github.com/" port-or-url))
     ((string-match-p akirak/https-url-shorthand-pattern port-or-url)
      (concat "https://" port-or-url))
     ((string-match (rx bol "@" (group (+ (any alnum "-_"))) eol) port-or-url)
      (concat "https://github.com/" (match-string 1 port-or-url)))
     ((string-match (rx bol "/r/") port-or-url)
      (concat "https://reddit.com" port-or-url))
     (t
      (let ((words (split-string port-or-url)))
        (format "https://duckduckgo.com/?q=%s"
                (mapconcat #'url-hexify-string words "+")))))))

(defconst akirak/local-url-pattern
  (rx bol "http://" (or "penguin.linux.test" "localhost")
      (?  ":" (+ digit))
      (?  "/" (* anything)) eol))

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
                (or (executable-find "brave")
                    (executable-find "chromium")))))

(setq-default browse-url-browser-function 'akirak/browse-url)

(defalias 'akirak/browse-localhost 'akirak/browse-url)

(provide 'setup-browse-url)
