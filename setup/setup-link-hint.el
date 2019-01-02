;; link-hint seems to be more convenient than ace-link, as it allows you to
;; visit a link in any visible windows.

(use-package link-hint)

(defun akirak/link-hint-open-link (&optional arg)
  "A variant of `link-hint-open-link' that supports a prefix argument.

If no prefix is given, this function opens a link (`link-hint-open-link').
 If a prefix is given, it opens multiple link (`link-hint-open-multiple-links'. "
  (interactive "P")
  (require 'link-hint)
  (if arg
      (link-hint-open-multiple-links)
    (link-hint-open-link)))

(provide 'setup-link-hint)
