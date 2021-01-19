;;;; Language-agnostic tools

(use-package google-translate
  :commands (google-translate-at-point
             google-translate-query-translate)
  :functions (google-translate-translate)
  :config
  ;; Changes needed because of changes in the Translate API:
  ;; https://github.com/atykhonov/google-translate/issues/52
  (defun google-translate--search-tkk ()
    "Search TKK." (list 430675 2721866130))
  (setq google-translate-backend-method 'curl))

;;;; Tools for specific languages

;;;;; Japanese
(use-package katawa
  :straight (katawa :host github :repo "akirak/katawa.el")
  :commands (katawa-ivy katawa-ivy-at-point))

(defun akirak/google-translate-helm-candidates (json)
  (when-let (detailed (google-translate-json-detailed-translation json))
    (->> detailed
         (seq-map (lambda (x)
                    (let ((pos (seq-elt x 0))
                          (candidates (seq-elt x 2)))
                      (seq-map (lambda (x)
                                 (let ((k (seq-elt x 0)))
                                   (cons (concat (propertize pos
                                                             'face
                                                             'font-lock-constant-face)
                                                 " "
                                                 k
                                                 " "
                                                 (propertize (string-join (seq-elt x 1) " / ")
                                                             'face
                                                             'font-lock-comment-face))
                                         k)))
                               candidates))))
         (-flatten-n 1))))

(defun akirak/insert-japanese-from-english (inp)
  (interactive "sEnglish: ")
  (let ((json (google-translate-request "en" "ja" inp)))
    (if-let (candidates (akirak/google-translate-helm-candidates json))
        (if (> (length candidates) 1)
            (helm :sources (helm-build-sync-source "Translations:"
                             :candidates candidates
                             :action #'insert))
          (insert (cdar candidates)))
      (insert (google-translate-json-translation json)))))

(provide 'setup-language-tools)
