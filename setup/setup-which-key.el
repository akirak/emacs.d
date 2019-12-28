(use-package which-key
  ;; :diminish which-key-mode
  :init
  (which-key-mode)
  :config
  (which-key-setup-side-window-bottom))

(defmacro akirak/which-key-add-stripped-prefix (prefix)
  "Add PREFIX as a stripped prefix to `which-key-replacement-alist'."
  `(add-to-list 'which-key-replacement-alist
                (quote ((nil . ,prefix) .
                        (lambda (kb)
                          (cons (car kb)
                                (string-remove-prefix ,prefix (cdr kb))))))))

(akirak/which-key-add-stripped-prefix "akirak/")

(provide 'setup-which-key)
