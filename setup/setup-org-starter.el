(use-package org-reverse-datetree)

(use-package org-super-agenda
  :after org-agenda
  :config
  ;; Basically stolen from org-super-agenda.el
  (eval-when-compile
    (org-super-agenda--def-auto-group ts-desc
      "the date of their latest timestamp anywhere in the entry (formatted according to `org-super-agenda-date-format', which see)"
      :keyword :auto-ts-desc
      :key-form (org-super-agenda--when-with-marker-buffer (org-super-agenda--get-marker item)
                  (let* ((limit (org-entry-end-position))
                         (latest-ts (->> (cl-loop for next-ts =
                                                  (when (re-search-forward org-element--timestamp-regexp limit t)
                                                    (ts-parse-org (match-string 1)))
                                                  while next-ts
                                                  collect next-ts)
                                         (-sort #'ts>)
                                         car)))
                    (when latest-ts
                      (propertize (ts-format org-super-agenda-date-format latest-ts)
                                  'org-super-agenda-ts latest-ts))))
      :key-sort-fn (lambda (a b)
                     ;; This part has been changed from `ts<' to `ts>'.
                     (ts> (get-text-property 0 'org-super-agenda-ts a)
                          (get-text-property 0 'org-super-agenda-ts b)))))
  (org-super-agenda-mode 1))

(use-package org-ql-search
  :straight org-ql)

(use-package org-ql-view
  :straight org-ql)

(use-package helm-org-ql
  :config
  ;; The default implementation of this function does not correctly
  ;; display the indirect buffer in another window: It displays the
  ;; entry both in the original window and another window.
  ;;
  ;; As a workaround, I override the function with my own
  ;; implementation that re-implements the indirect buffer creation
  ;; function.
  ;;
  ;; It also displays the outline path in the header line.
  (advice-add #'helm-org-ql-show-marker-indirect
              :override
              (defun akirak/helm-org-ql-show-marker-indirect (marker)
                (let* ((orig-buffer (marker-buffer marker))
                       (buffer (with-current-buffer (or (org-base-buffer orig-buffer)
                                                        orig-buffer)
                                 (org-with-wide-buffer
                                  (goto-char marker)
                                  (let ((pos (point))
                                        (buffer (org-get-indirect-buffer)))
                                    (with-current-buffer buffer
                                      (goto-char pos)
                                      (org-narrow-to-subtree)
                                      (org-show-entry)
                                      (rename-buffer (org-get-heading t t t t) 'unique)
                                      (setq-local header-line-format
                                                  (list (buffer-name orig-buffer)
                                                        ":"
                                                        (org-format-outline-path (org-get-outline-path)))))
                                    buffer)))))
                  (pop-to-buffer buffer))))
  (defun akirak/helm-org-ql-refile-action (marker)
    (unless (derived-mode-p 'org-mode)
      (user-error "Not in org-mode"))
    (unless (markerp marker)
      (user-error "Not a marker: %s" marker))
    (let ((filename (buffer-file-name (or (buffer-base-buffer (marker-buffer marker))
                                          (marker-buffer marker))))
          (heading (with-current-buffer (marker-buffer marker)
                     (org-with-wide-buffer
                      (goto-char marker)
                      (org-get-heading t t t t)))))
      (org-refile nil nil (list heading filename nil marker))))
  (defun akirak/helm-org-ql-add-child-entry (marker)
    (let* ((heading (read-string (format "Heading of the new entry in \"%s\": "
                                         (with-current-buffer (marker-buffer marker)
                                           (org-with-wide-buffer
                                            (goto-char marker)
                                            (substring-no-properties (org-get-heading t t t t)))))))
           (org-capture-entry `("c" "Child entry" entry
                                (function (lambda () (org-goto-marker-or-bmk ,marker)))
                                ,(concat "* " heading "\n:PROPERTIES:\n"
                                         ":CREATED_TIME: " (org-format-time-string (org-time-stamp-format 'long 'inactive))
                                         "\n:END:\n"))))
      (org-capture)))
  (defun akirak/org-clock-in (marker)
    (with-current-buffer (marker-buffer marker)
      (org-with-wide-buffer
       (goto-char marker)
       (org-clock-in))))
  (general-add-hook 'helm-org-ql-actions
                    '(("Refile the current org entry" . akirak/helm-org-ql-refile-action)
                      ("Create a new entry" . akirak/helm-org-ql-add-child-entry)
                      ("Clock in" . akirak/org-clock-in))
                    t))

(use-package org-multi-wiki
  :init
  (defvar akirak/org-multi-wiki-initialized nil)
  (unless akirak/org-multi-wiki-initialized
    (setq org-multi-wiki-namespace-list nil
          akirak/org-multi-wiki-initialized t))
  (with-eval-after-load 'org-ql
    (org-ql-defpred wiki (namespace)
      "It is inside a particular namespace."
      :body (org-multi-wiki-in-namespace-p (intern namespace))))
  :config
  (defun akirak/org-multi-wiki-entry-template-fn (heading)
    (concat "* " heading "\n:PROPERTIES:\n"
            ":CREATED_TIME: " (org-format-time-string (org-time-stamp-format 'long 'inactive))
            "\n:END:\n"))
  (org-multi-wiki-global-mode 1)
  :custom
  (org-multi-wiki-want-custom-id t)
  (org-multi-wiki-entry-template-fn #'akirak/org-multi-wiki-entry-template-fn))

(use-package helm-org-multi-wiki
  :config
  (general-create-definer akirak/bind-helm-org-multi-wiki-dummy
    :keymaps 'helm-org-multi-wiki-dummy-source-map
    :package 'helm-org-multi-wiki
    :prefix "C-c C-c")
  (akirak/bind-helm-org-multi-wiki-dummy
    "" '(:wk "Create wiki entry")))

(use-package org-starter
  :straight (org-starter :host github :repo "akirak/org-starter"
                         :branch "devel")
  :preface
  (defcustom org-starter-capture-meta-templates nil
    "An alist of org-capture meta-templates.

This lets you define patterns of `org-capture' templates that are
common in multiple files.

Each entry consists of a symbol which identifies the meta
template and the content of a :capture entry in
`org-starter-define-file' after the file name.

By defining a meta-template in this variable, you can define the
same template for multiple files by specifying the symbol at the
third argument, i.e. right after the description, in the entry."
    :type '(alist :key-type symbol
                  :value-type sexp))

  (cl-defmacro akirak/add-ql-view (title buffers-files query
                                         &key sort super-groups)
    (declare (indent 1))
    (let ((value (list :buffers-files
                       (cl-etypecase buffers-files
                         (string buffers-files)
                         (buffer buffers-files)
                         (list (if (symbolp (car buffers-files))
                                   (eval buffers-files)
                                 buffers-files)))
                       :query query
                       :super-groups super-groups
                       :sort sort
                       :title title)))
      `(if-let ((entry (assoc ,title org-ql-views)))
           (setcdr entry ',value)
         (push (cons ,title ',value) org-ql-views))))

  (cl-defmacro akirak/org-multi-wiki-add-ql-view (name namespace query
                                                       &key sort
                                                       super-groups)
    (declare (indent 2))
    (macroexpand
     `(akirak/add-ql-view ,name
        (lambda () (org-multi-wiki-entry-files ',namespace))
        ,query
        :sort ,sort :super-groups ,super-groups)))

  :config
  (org-starter-mode t)
  (general-add-hook 'org-starter-extra-find-file-map
                    '((";" org-starter-find-config-file "config"))
                    t)
  (general-add-hook 'org-starter-extra-alternative-find-file-map
                    '((";" org-starter-swiper-config-files "config"))
                    t)
  (general-add-hook 'org-starter-extra-refile-map
                    '(("'" avy-org-refile-as-child "avy"))
                    ;; Questionable
                    ;; ("?" akirak/org-refile-same-buffer "same buffer"))
                    t)

  (let ((todo-body "* TODO %i%?
:PROPERTIES:
:CREATED_TIME: %U
:END:
"))
    (general-add-hook 'org-starter-capture-meta-templates
                      `((todo
                         entry file ,todo-body
                         :clock-in t :clock-resume t)
                        (reverse-datetree
                         entry (file+function org-reverse-datetree-goto-date-in-file)
                         ,todo-body
                         :clock-in t :clock-resume t :empty-lines 1))
                      t))

  (defun akirak/helm-org-ql-known-files ()
    (interactive)
    (helm-org-ql org-starter-known-files))
  :custom
  (org-starter-capture-template-map-function
   (defun akirak/org-starter-meta-capture-templates (spec)
     (when-let (rest (and (= 3 (length spec))
                          (alist-get (nth 2 spec)
                                     org-starter-capture-meta-templates)))
       (setf (cddr spec) rest))
     spec))
  (org-starter-load-config-files t)
  (org-starter-require-file-by-default nil)
  (org-starter-exclude-from-recentf '(known-files path))
  (org-starter-alternative-find-function #'helm-org-ql)
  (org-starter-find-file-visit-window t)
  (org-starter-override-agenda-window-setup 'other-window)
  (org-starter-enable-local-variables :all))

(use-package org-starter-swiper
  :commands (org-starter-swiper-config-files)
  :straight org-starter)

(use-package xmind-org
  :straight (:host github :repo "akirak/xmind-org-el")
  :commands (xmind-org-insert-file))

;;;; Extra keybindings
(akirak/bind-search
  "M-a" #'helm-org-ql-agenda-files
  "M-k" #'akirak/helm-org-ql-known-files
  "M-o" #'org-starter-alternative-find-file-by-key
  "M-m" #'helm-org-multi-wiki
  "M-w" #'helm-org-multi-wiki-all)
(akirak/bind-jump
  "M-o" #'org-starter-find-file-by-key)
(akirak/bind-mode :keymaps 'org-mode-map :package 'org
  "r" #'org-starter-refile-by-key)

(provide 'setup-org-starter)
