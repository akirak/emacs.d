(use-package clipurl
  :straight (clipurl :host github :repo "akirak/clipurl.el"))

(cl-defmacro akirak/def-org-capture-url-fn (location-exp
                                            &key
                                            todo
                                            default-title
                                            manual-tags
                                            as-body
                                            props)
  (declare (indent 1))
  `(lambda (url)
     (let* ((location ,location-exp)
            (html (with-timeout (3)
                    (org-web-tools--get-url url)))
            (title ,(or (when default-title
                          `(when html
                             (org-web-tools--html-title html)))
                        `(read-string (format "Title of %s: " url)
                                      (when html
                                        (org-web-tools--html-title html))
                                      nil nil t)))
            (link (org-link-make-string url title))
            (drawer "\n:PROPERTIES:\n:CREATED_TIME: %U\n:END:\n")
            (template ,(if as-body
                           `(concat "* "
                                    ,(if todo "TODO " "")
                                    "%^{Title of the entry}" drawer "%?\n\n" link)
                         `(concat "* " ,(if todo "TODO " "") link " :link:" ,(if manual-tags "%^g" "")
                                  drawer "\n%?")))
            (org-capture-entry (list "b" "Bookmark" 'entry
                                     (cl-typecase location
                                       (marker (list 'function
                                                     `(lambda () (org-goto-marker-or-bmk ,location))))
                                       (function (list 'function location))
                                       (otherwise location))
                                     template
                                     ,@props)))
       (org-capture))))

(use-package ivy-clipurl
  :straight clipurl
  :commands (ivy-clipurl)
  :config
  (ivy-add-actions 'ivy-clipurl
                   `(("c"
                      ,(akirak/def-org-capture-url-fn
                           (list 'file+function
                                 (or (org-starter-locate-file "global.org" nil t)
                                     (error "Cannot find global.org"))
                                 #'org-reverse-datetree-goto-date-in-file)
                         :default-title t
                         :manual-tags t)
                      "Capture to global.org")
                     ("n"
                      ,(akirak/def-org-capture-url-fn
                           (list 'file+function
                                 (or (org-starter-locate-file "news.org" nil t)
                                     (error "Cannot find news.org"))
                                 #'org-reverse-datetree-goto-date-in-file)
                         :default-title t
                         :manual-tags t
                         :props (:clock-in t :clock-resume t))
                      "Capture to news.org")
                     ("a"
                      ,(akirak/def-org-capture-url-fn
                           (avy-with avy-goto-line
                             (avy-jump (rx bol (1+ "*") (1+ space)))
                             :action (point-marker))
                         :default-title t
                         ;; Unlike the case of a bookmark destination, this
                         ;; finishes immediately, because the entry is already
                         ;; visible.
                         :props (:immediate-finish t))
                      "Capture to avy")
                     ("@"
                      ,(akirak/def-org-capture-url-fn
                           (or (and (org-clocking-p)
                                    (markerp org-clock-marker))
                               (user-error "No running clock"))
                         :default-title t
                         :props (:immediate-finish t))
                      "Capture immediately to clock")
                     ("d"
                      org-download-image
                      "Insert as an image into Org"))))

(provide 'setup-clipurl)
