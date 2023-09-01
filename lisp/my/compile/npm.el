(require 'json)

(defun akirak/npm-package-json-commands (file)
  "Return a list of npm script commands from FILE."
  (let* ((json-object-type 'alist)
         (package (with-temp-buffer
                    (insert-file-contents file)
                    (goto-char (point-min))
                    (json-read-object))))
    (alist-get 'scripts package)))

(defvar akirak/npm-toplevel-commands nil)

(defun akirak/npm-toplevel-commands ()
  (or akirak/npm-toplevel-commands
      (setq akirak/npm-toplevel-commands
            (let ((buffer (generate-new-buffer "*npm*")))
              (unwind-protect
                  (if (zerop (call-process "nix-shell"
                                           nil (list buffer nil) nil
                                           "-p" "nodejs" "--run" "npm help"))
                      (with-current-buffer buffer
                        (goto-char (point-min))
                        (re-search-forward (rx bol "where " (+ nonl) eol))
                        (let ((start (point)))
                          (re-search-forward (rx bol (* space) eol))
                          (--> (buffer-substring-no-properties start (point))
                               (split-string it ",")
                               (-map #'string-trim it)
                               (cl-delete-if #'string-empty-p it))))
                    (error "nix-shell returned non-zero"))
                (kill-buffer buffer))))))

(provide 'my/compile/npm)
