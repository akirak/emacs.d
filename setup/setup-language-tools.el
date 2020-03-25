;;;; Language-agnostic tools

(use-package google-translate
  :commands (google-translate-at-point
             google-translate-query-translate)
  :functions (google-translate-translate)
  :config
  (akirak/bind-language
    "t" #'google-translate-at-point))

;;;; Tools for specific languages

;;;;; Japanese
(use-package katawa
  :straight (katawa :host github :repo "akirak/katawa.el")
  :commands (katawa-ivy katawa-ivy-at-point)
  :config
  (akirak/bind-language
    "j" #'katawa-ivy-at-point
    "J" #'katawa-ivy)
  (ivy-add-actions 'katawa-ivy
                   '(("t" akirak/org-capture-japanese-to-translate "org-capture"))))

(defun akirak/org-capture-japanese-to-translate (inp)
  (let* ((org-capture-entry `("x" "xxx" entry
                              (file+function ,(org-starter-locate-file "japanese.org" nil t)
                                             org-reverse-datetree-goto-date-in-file)
                              ;; TODO: Automatically insert translations
                              ,(concat "* " inp "\n"))))
    (org-capture)))

(provide 'setup-language-tools)
