(defun akirak/recent-remote-identifiers ()
  (->> recentf-list
       (-map #'file-remote-p)
       (delq nil)
       (cl-remove-duplicates)))

(provide 'my/remote/enum)
