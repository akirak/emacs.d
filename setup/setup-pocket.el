(use-package pocket-reader
  :custom
  ;; Don't archive articles on visiting
  (pocket-reader-archive-on-open nil))

(defun akirak/pocket-reader (query)
  "Run `pocket-reader' with optional initial query."
  (interactive (list (read-string "Query for pocket-reader: ")))
  (let ((query (and (not (string-empty-p query))
                    query)))
    (cond
     ((and query
           (eq major-mode 'pocket-reader-mode))
      (pocket-reader-search query))
     ((eq major-mode 'pocket-reader-mode)
      (pocket-reader))
     (query
      (let ((pocket-reader-default-queries (list query)))
        (pocket-reader)))
     (t
      (pocket-reader)))))

(akirak/bind-search "M-t" #'akirak/pocket-reader)

(major-mode-hydra-define pocket-reader-mode
  (:title "pocket-reader")
  ("Navigation"
   (("n" next-line)
    ("p" previous-line)
    ("m" pocket-reader-toggle-mark "Mark")
    ("M" pocket-reader-mark-all "Mark all"))
   "List view"
   (("E" pocket-reader-excerpt-all "Excerpt all")
    ("s" pocket-reader-search "Search")
    ("o" pocket-reader-more "More"))
   "Read"
   (("RET" pocket-reader-open-url "Open")
    ("b" pocket-reader-open-in-external-browser "External browser")
    ("c" pocket-reader-copy-url "Copy URL"))
   "Status"
   (("f" pocket-reader-toggle-favorite "Toggle favorite")
    ("a" pocket-reader-toggle-archived "Toggle archived")
    ("D" pocket-reader-delete "Delete"))
   "Tags"
   (("ts" pocket-reader-tag-search "Tag search")
    ("ta" pocket-reader-add-tags "Add tags")
    ("ts" pocket-reader-set-tags "Set tags")
    ("tr" pocket-reader-remove-tags "Remove tags"))))

(provide 'setup-pocket)
