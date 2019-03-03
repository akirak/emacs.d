;;; org-refile.el --- synopsis -*- lexical-binding: t -*-

;;;###autoload
(defun akirak/org-refile-same-buffer (arg)
  "org-refile to a heading in the same buffer."
  (interactive "P")
  (let ((org-refile-targets `((nil . ,(cl-typecase arg
                                        (number `(:level . ,arg))
                                        (t '(:maxlevel . 9)))))))
    (call-interactively #'org-refile)))

(provide 'akirak/org-refile)
;;; org-refile.el ends here
