;; Inspired by http://www.howardism.org/Technical/Emacs/capturing-content.html

;;;; Plain content
(org-starter-def-capture "p" "Plain content to the clock"
  plain (clock)
  "%?%i" :unnarrowed t :empty-lines 1 :no-save t)

(org-starter-def-capture "P" "Plain content to the clock (immediate finish)"
  plain (clock)
  "%i" :immediate-finish t :empty-lines 1 :no-save t)

(defsubst akirak/org-capture-plain (&rest paragraphs)
  (org-capture-string (mapconcat #'identity (delq nil paragraphs)
                                 "\n\n")
                      "P"))

(defsubst akirak/org-capture-plain-popup (&rest paragraphs)
  (org-capture-string (concat "  "
                              (mapconcat #'identity (delq nil paragraphs)
                                         "\n\n"))
                      "p"))

;;;; Item
(org-starter-def-capture "i" "Item to the clock"
  item (clock)
  "%?"  :no-save t :unnarrowed t)

(org-starter-def-capture "I" "Item to the clock (immediate finish)"
  item (clock)
  "%i" :immediate-finish t :no-save t)

(defsubst akirak/org-capture-item (input)
  (org-capture-string input "I"))

(org-starter-def-capture "b" "Check item to the clock"
  checkitem (clock)
  "[ ] %?"  :no-save t :unnarrowed t)

;;;; Child entries
(org-starter-def-capture "n" "Subtree"
  entry (clock)
  "* %^{Title}
:PROPERTIES:
:CREATED_TIME: %U
:END:
%?"
  :clock-in t :clock-resume t :no-save t :empty-lines 1)

(org-starter-def-capture "N" "Subtree (immediate finish, clock-in)"
  entry (clock)
  "* ^{Title}
:PROPERTIES:
:CREATED_TIME: %U
:END:"
  :clock-in t :immediate-finish t :no-save t :empty-lines 1)

(org-starter-def-capture "H" "Heading"
  entry (clock)
  "* %i"
  :clock-in t :immediate-finish t :no-save t)

;;;; Interactive functions

;;;;; Blocks

;;;###autoload
(defun akirak/org-capture-selected-source (description with-link)
  (interactive (list (when current-prefix-arg
                       (read-string "Description: "))
                     (equal current-prefix-arg '(16))))
  (akirak/org-capture-plain description
                            (akirak/org-capture-wrap-selection "SRC"
                              (string-remove-suffix "-mode" (symbol-name major-mode)))
                            (when with-link
                              (concat "From "
                                      (akirak/location-as-org-link (akirak/describe-location-in-source))))))

;;;###autoload
(defun akirak/org-capture-selected-quote (description)
  (interactive (list (when current-prefix-arg
                       (read-string "Description: "))
                     (equal current-prefix-arg '(16))))
  (akirak/org-capture-plain description
                            (akirak/org-capture-wrap-selection "QUOTE")
                            (when with-link
                              (org-store-link nil)
                              (let ((org-keep-stored-link-after-insertion nil))
                                (concat "From "
                                        (org-make-link-string
                                         (car (pop org-stored-links))
                                         (buffer-name)))))))

;;;###autoload
(defun akirak/org-capture-selected-example (description)
  (interactive (list (when current-prefix-arg
                       (read-string "Description: "))
                     (equal current-prefix-arg '(16))))
  (akirak/org-capture-plain description
                            (akirak/org-capture-wrap-selection "EXAMPLE")))

;;;;; Clipboard

;;;###autoload
(defun akirak/org-capture-clipboard-as-source (text lang url)
  (interactive (list (if current-prefix-arg
                         (completing-read "Kill ring: " (ring-elements kill-ring))
                       (funcall interprogram-paste-function))
                     (akirak/read-source-language "Language: ")
                     (clipurl-complete-url "Source URL of the code: ")))
  (akirak/org-capture-plain-popup
   (format "\n\n#+BEGIN_SRC %s\n%s\n#+END_SRC" lang text)
   (concat "From " (org-web-tools--org-link-for-url url))
   (akirak/org-capture-wrap-selection "SRC"
     (string-remove-suffix "-mode" (symbol-name major-mode)))
   (when with-link
     (concat "From "
             (akirak/location-as-org-link (akirak/describe-location-in-source))
             (org-make-link-string
              (car (pop org-stored-links))
              (akirak/describe-location-in-source))))))

;; TODO: Add quote

;; TODO: Add example

;;;;; Items

;;;###autoload
(defun akirak/org-capture-url-link-as-item (&optional url)
  (interactive)
  (unless (org-clocking-p)
    (user-error "Not clocking in"))
  (akirak/org-capture-item (org-web-tools--org-link-for-url url)))

;;;###autoload
(defun akirak/org-capture-string-as-item (text)
  (interactive (list (buffer-substring-no-properties
                      (region-beginning) (region-end))))
  (unless (org-clocking-p)
    (user-error "Not clocking in"))
  (akirak/org-capture-item text))

;;;; Utility functions and macros

(defun akirak/org-capture-wrap-selection (type &rest plist)
  (declare (indent 1))
  (unless (stringp type)
    (user-error "TYPE must be string: %s" type))
  (format "#+BEGIN_%s%s\n%s\n#+END_%s"
          type
          (when plist
            (concat " " (mapconcat (lambda (s) (format "%s" s))
                                   plist " ")))
          (buffer-substring-no-properties (region-beginning)
                                          (region-end))
          type))

(defun akirak/location-as-org-link (&optional description)
  (org-store-link nil)
  (let ((org-keep-stored-link-after-insertion nil))
    (org-make-link-string (car (pop org-stored-links))
                          description)))

(defun akirak/describe-location-in-source ()
  (let* ((func (which-function))
         (abspath (expand-file-name (buffer-file-name)))
         ;; TODO: Add support for .svn
         (root (locate-dominating-file abspath ".git"))
         (filepath (if root
                       (file-relative-name abspath root)
                     (file-name-nondirectory abspath))))
    (if func
        (format "%s in %s" func filepath)
      filepath)))

(defun akirak/major-mode-list ()
  (let (modes)
    (do-all-symbols (sym)
      (let ((name (symbol-name sym)))
        (when (and (commandp sym)
                   (string-suffix-p "-mode" name)
                   (let ((case-fold-search nil))
                     (string-match-p "^[a-z]" name))
                   (not (string-match-p (rx "/") name))
                   (not (string-match-p "global" name))
                   (not (memq sym minor-mode-list)))
          (push sym modes))))
    modes))

(defun akirak/read-source-language (prompt)
  (completing-read prompt
                   (-sort #'string<
                          (--map (string-remove-suffix "-mode" (symbol-name it))
                                 (akirak/major-mode-list)))))

(provide 'akirak/org-clock-capture)

