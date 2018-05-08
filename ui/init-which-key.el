(use-package which-key
  :diminish which-key-mode
  :init
  (which-key-mode)
  :config
  (which-key-setup-side-window-bottom)  ; Display a popup window at bottom
  ;; Remove 'akirak/' prefix from descriptions
  (add-to-list 'which-key-replacement-alist
               `((nil . "akirak/") .
                 (lambda (kb)
                   (cons (car kb)
                         (string-remove-prefix "akirak/" (cdr kb)))))))

(provide 'init-which-key)
