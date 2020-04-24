(require 'json)

(defun akirak/npm-package-json-commands (file)
  "Return a list of npm script commands from FILE."
  (let* ((json-object-type 'alist)
         (package (with-temp-buffer
                    (insert-file-contents file)
                    (goto-char (point-min))
                    (json-read-object))))
    (alist-get 'scripts package)))

(provide 'my/compile/npm)
