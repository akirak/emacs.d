(use-package org-reverse-datetree)

(use-package org-workflow
  :straight (:host github :repo "akirak/org-workflow"))

(use-package helm-org
  :after helm

  :general
  (:keymaps 'helm-org-headings-map :package 'helm-org
            ;; Use the same keybinding as the other narrowing commands
            "C-x n"
            (defun akirak/helm-org-narrow-to-heading-at-marker ()
              "Narrow to the selected heading in helm-org."
              (interactive)
              (with-helm-alive-p
                (helm-exit-and-execute-action
                 (lambda (marker)
                   (when (buffer-narrowed-p)
                     (widen))
                   (helm-org-goto-marker marker)
                   (org-narrow-to-subtree))))))

  :config
  (setq helm-org-headings-actions
        (org-workflow--build-helm-actions org-workflow-heading-unary-action-list))

  (cl-defun akirak/helm-org-context-candidates (marker &key preceding descendants)
    (with-current-buffer (marker-buffer marker)
      (org-with-wide-buffer
       (goto-char marker)
       (let (result)
         (cl-labels ((add () (push (cons (buffer-substring-no-properties
                                          (point)
                                          (line-end-position))
                                         (point-marker))
                                   result)))
           (add)
           (when descendants
             (save-excursion
               (let ((end (save-excursion
                            (org-end-of-subtree))))
                 (while (re-search-forward org-heading-regexp nil end)
                   (add)))))
           (while (looking-at (rx bol "*" (+ "*") space))
             (if preceding
                 (re-search-backward org-heading-regexp)
               (org-up-heading-safe))
             (add)))
         (reverse result)))))

  (defconst akirak/helm-org-clock-context-source
    (helm-build-sync-source "Current clock context"
      :candidates (lambda ()
                    (when (and (org-clocking-p)
                               org-clock-hd-marker)
                      (akirak/helm-org-context-candidates
                       org-clock-hd-marker
                       :preceding t
                       :descendants t)))
      :action 'helm-org-headings-actions)))

(use-package org-super-agenda
  :after org-agenda

  :custom
  (org-super-agenda-date-format "%Y-%m-%d")

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
                          (get-text-property 0 'org-super-agenda-ts b))))

    (org-super-agenda--def-auto-group buffer-name "buffer name"
      :keyword :auto-buffer-name
      :key-form (org-super-agenda--when-with-marker-buffer (org-super-agenda--get-marker item)
                  (buffer-name))))
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

  ;; Unused
  (defun akirak/helm-org-ql-show-marker-indirect-2 (marker)
    (let ((orig-buffer (marker-buffer marker)))
      (switch-to-buffer
       (with-current-buffer (or (org-base-buffer orig-buffer)
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
            buffer))))))

  ;; Unused
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

  ;; Unused
  (defun akirak/helm-org-ql-add-child-entry (marker)
    (let* ((heading (read-string (format "Heading of the new entry in \"%s\": "
                                         (with-current-buffer (marker-buffer marker)
                                           (org-with-wide-buffer
                                            (goto-char marker)
                                            (substring-no-properties (org-get-heading t t t t)))))))
           (org-capture-entry `("c" "Child entry" entry
                                #'(lambda () (org-goto-marker-or-bmk ,marker))
                                ,(concat "* " heading "\n:PROPERTIES:\n"
                                         ":CREATED_TIME: " (org-format-time-string (org-time-stamp-format 'long 'inactive))
                                         "\n:END:\n"))))
      (org-capture)))

  ;; Unused
  (defun akirak/org-clock-in (marker)
    (with-current-buffer (marker-buffer marker)
      (org-with-wide-buffer
       (goto-char marker)
       (org-clock-in))))

  (cl-defun akirak/helm-org-ql-show-marker (marker &key indirect narrow)
    (with-current-buffer (marker-buffer marker)
      ;; (akirak/pop-to-buffer-prefer-center-pane (current-buffer))
      (switch-to-buffer (current-buffer))
      (goto-char marker)
      (org-show-entry)
      (cond
       (indirect
        (org-tree-to-indirect-buffer))
       (narrow
        (org-narrow-to-subtree))
       (t
        (recenter-top-bottom 0)))))

  (defun akirak/helm-org-ql-show-marker-indirect (marker)
    (akirak/helm-org-ql-show-marker marker :indirect t))

  (defun akirak/helm-org-ql-show-marker-narrow (marker)
    (akirak/helm-org-ql-show-marker marker :narrow t))

  (defun akirak/org-store-link-to-marker (marker)
    (with-current-buffer (marker-buffer marker)
      (org-with-wide-buffer
       (goto-char marker)
       ;; TODO: Set the custom id
       ;; (when (and (fboundp #'org-multi-wiki-entry-file-p)
       ;;            (org-multi-wiki-entry-file-p)
       ;;            (save-excursion
       ;;              ;; Skip if the heading is the first heading
       ;;              (re-search-backward org-heading-regexp nil t))
       ;;            (not (org-entry-get nil "CUSTOM_ID")))
       ;;   (org-set-property "CUSTOM_ID" nil))
       (call-interactively #'org-store-link))))

  (setq helm-org-ql-actions
        '(("Show" . akirak/helm-org-ql-show-marker)
          ("Show indirect buffer" . akirak/helm-org-ql-show-marker-indirect)
          ("Store link" . akirak/org-store-link-to-marker)))

  (general-def :keymaps 'helm-org-ql-map :package 'helm-org-ql
    "C-x n"
    (defun akirak/helm-org-ql-narrow-to-heading ()
      "Narrow to the selected heading in helm-org."
      (interactive)
      (with-helm-alive-p
        (helm-exit-and-execute-action
         (lambda (marker)
           (akirak/helm-org-ql-show-marker-narrow marker)))))))

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

  (setq org-multi-wiki-removal-block-functions
        (list (defun akirak/org-multi-wiki-block-function ()
                ;; Allow moving subtrees from journal
                (when-let (plist (org-multi-wiki-entry-file-p))
                  (not (eq (plist-get plist :namespace) 'journal))))))

  (setq org-multi-wiki-display-buffer-fn
        ;; #'akirak/pop-to-buffer-prefer-center-pane
        #'switch-to-buffer)

  (defun akirak/helm-org-multi-wiki-show-indirect (marker &optional switch-buffer-fn)
    (let* ((h (with-current-buffer (marker-buffer marker)
                (org-with-point-at marker
                  (org-heading-components))))
           (indirect (> (car h) 1))
           (buffer (if indirect
                       (make-indirect-buffer
                        (marker-buffer marker)
                        (format "%s - %s"
                                (buffer-name (marker-buffer marker))
                                (nth 4 h))
                        'clone)
                     (marker-buffer marker))))
      (with-current-buffer buffer
        (widen)
        (when indirect
          (goto-char (point-min)))
        (org-global-cycle 1)
        (goto-char marker)
        (org-show-set-visibility 'ancestors)
        (org-show-subtree)
        (org-cycle-hide-drawers 'all)
        (org-multi-wiki-run-mode-hooks)
        (when indirect
          (run-hooks 'clone-indirect-buffer-hook))
        (funcall (or switch-buffer-fn #'switch-to-buffer) (current-buffer)))))

  (setq helm-org-multi-wiki-actions
        '(("Show indirect buffer (same window)" . akirak/helm-org-multi-wiki-show-indirect)
          ("Show indirect buffer (ace window)" .
           (lambda (marker)
             (ace-window nil)
             (akirak/helm-org-multi-wiki-show-indirect marker)))
          ("Show indirect buffer (new tab)" .
           (lambda (marker)
             (tab-bar-new-tab)
             (akirak/helm-org-multi-wiki-show-indirect marker)))))

  (setq org-multi-wiki-extra-files (list 'org-agenda-files
                                         (defun akirak/org-journal-dir-files ()
                                           (-sort #'string>
                                                  (directory-files org-journal-dir t
                                                                   org-agenda-file-regexp
                                                                   'nosort)))))
  :custom
  (org-multi-wiki-recentf-exclude t)
  (org-multi-wiki-want-custom-id t)
  (org-multi-wiki-entry-template-fn #'akirak/org-multi-wiki-entry-template-fn)
  (org-multi-wiki-display-buffer-fn #'switch-to-buffer))

(use-package helm-org-multi-wiki
  :config
  (setq helm-org-multi-wiki-file-actions
        '(("Switch to buffer (center pane)"
           . akirak/pop-to-buffer-prefer-center-pane)
          ("Switch to buffer (same window)"
           . switch-to-buffer)
          ("Switch to buffer (other window)"
           . switch-to-buffer-other-window)
          ("Switch to buffer (other tab)"
           . switch-to-buffer-other-tab)
          ("Switch to buffer (other frame)"
           . switch-to-buffer-other-frame)))

  (general-create-definer akirak/bind-helm-org-multi-wiki-dummy
    :keymaps 'helm-org-multi-wiki-dummy-source-map
    :package 'helm-org-multi-wiki
    :prefix "C-c C-c")
  (akirak/bind-helm-org-multi-wiki-dummy
    "" '(:wk "Create wiki entry")))

(defvar akirak/helm-org-ql-dummy-source
  (helm-build-dummy-source " Query"
    :action
    (helm-make-actions
     "Search in org-multi-wiki"
     (lambda (s)
       (org-ql-search (->> org-multi-wiki-namespace-list
                           (-map #'car)
                           (--map (org-multi-wiki-entry-files it :as-buffers t))
                           (apply #'append))
         s
         :super-groups '((:auto-buffer-name))))
     "Search in org-starter-known-files"
     (lambda (s)
       (org-ql-search org-starter-known-files
         s
         :super-groups '((:auto-category)))))))

(use-package org-starter
  :straight (:host github :repo "akirak/org-starter"
                   :branch "devel"
                   :files (:defaults (:exclude "org-starter-swiper.el"
                                               "org-starter-extras.el")))
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
  (defun akirak/find-user-init-file ()
    (interactive)
    (find-file (expand-file-name "init.el" user-emacs-directory)))

  (defun akirak/find-file-in-user-emacs-dir ()
    (interactive)
    (akirak/find-file-recursively user-emacs-directory))

  (defun akirak/find-file-in-commonplace ()
    (interactive)
    (akirak/find-file-recursively "~/commonplace/"))

  (defun akirak/commonplace-repos-status ()
    (interactive)
    (magit-status "~/commonplace/"))

  (org-starter-mode t)
  (general-add-hook 'org-starter-extra-find-file-map
                    '((";" org-starter-find-config-file "config")
                      ;; ("y" akirak/yankpad-show-current-category "yankpad")
                      ("i" akirak/find-user-init-file "init.el")
                      ("c" akirak/commonplace-repos-status "commonplace"))
                    t)
  (general-add-hook 'org-starter-extra-alternative-find-file-map
                    '((";" org-starter-swiper-config-files "config")
                      ("i" akirak/find-file-in-user-emacs-dir "emacs-dir")
                      ("c" akirak/find-file-in-commonplace "commonplace"))
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
                         :clock-in t :clock-resume t :empty-lines 1)
                        (reverse-datetree-no-clocking
                         entry (file+function org-reverse-datetree-goto-date-in-file)
                         ,todo-body
                         :empty-lines 1))
                      t))

  (org-starter-def-capture "," "To the clock")

  (defun akirak/file-major-mode-name (file)
    (->> (find-buffer-visiting file)
         (buffer-local-value 'major-mode)
         (symbol-name)
         (string-remove-suffix "-mode")))

  (org-starter-def-capture ",s" "source block"
    plain
    (function (lambda ()
                (org-goto-marker-or-bmk org-clock-marker)
                (goto-char (org-entry-end-position))))
    "#+BEGIN_SRC %(akirak/file-major-mode-name \"%F\")\n%i\n#+END_SRC\n\n"
    :immediate-finish t)
  (org-starter-def-capture ",q" "quote"
    plain
    (function (lambda ()
                (org-goto-marker-or-bmk org-clock-marker)
                (goto-char (org-entry-end-position))))
    "#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n"
    :immediate-finish t)
  (general-add-hook 'org-capture-templates-contexts
                    '(("," (org-clocking-p))
                      (",s" (region-active-p))
                      (",q" (region-active-p))))

  (defun akirak/helm-org-ql-known-files ()
    (interactive)
    (helm-org-ql org-starter-known-files))
  :custom
  (org-starter-capture-template-map-function
   (defun akirak/org-starter-meta-capture-templates (spec)
     (if-let (rest (and (= 3 (length spec))
                        (alist-get (nth 2 spec)
                                   org-starter-capture-meta-templates)))
         (append (-take 2 spec)
                 (copy-sequence rest))
       spec)))
  (org-starter-load-config-files t)
  (org-starter-require-file-by-default nil)
  (org-starter-exclude-from-recentf '(known-files path))
  (org-starter-alternative-find-function #'helm-org-ql)
  (org-starter-find-file-visit-window t)
  (org-starter-override-agenda-window-setup 'other-window)
  (org-starter-enable-local-variables :all))

(use-package org-starter-swiper
  :straight (:host github :repo "akirak/org-starter"
                   :branch "devel"
                   :files ("org-starter-swiper.el"))
  :commands (org-starter-swiper-config-files))

(use-package xmind-org
  :straight (:host github :repo "akirak/xmind-org-el")
  :commands (xmind-org-insert-file))

(use-package readable
  :straight (:host github :repo "akirak/readable.el")
  :custom
  (readable-cache-directory (expand-file-name "emacs-readable"
                                              (xdg-cache-home))))

;;;; Extra keybindings
(akirak/bind-search
  "M-a" #'helm-org-ql-agenda-files
  "M-k" #'akirak/helm-org-ql-known-files
  "M-o" #'org-starter-alternative-find-file-by-key
  "M-m" #'helm-org-multi-wiki
  "M-w" #'helm-org-multi-wiki-all)
(akirak/bind-search :keymaps 'org-mode-map :package 'org
  "l" #'org-multi-wiki-backlink-view)
(akirak/bind-jump
  "M-o" #'org-starter-find-file-by-key
  "@" (defun akirak/org-clock-follow-link ()
        (interactive)
        (unless (org-clocking-p)
          (user-error "Not clocking"))
        (let (links
              directory)
          (org-with-point-at org-clock-marker
            (setq directory default-directory)
            (org-back-to-heading)
            (let ((entry-end (org-entry-end-position)))
              (while (re-search-forward org-link-any-re entry-end t)
                (push (propertize (org-link-display-format
                                   (buffer-substring-no-properties (line-beginning-position)
                                                                   (point)))
                                  'htmlize-link
                                  (get-char-property (1- (point)) 'htmlize-link))
                      links))))
          (let* ((dest (cl-case (length links)
                         (0 (user-error "No link found in the clocked entry"))
                         (1 (car links))
                         (otherwise (completing-read "Link: " links))))
                 (default-directory directory)
                 (marker (save-window-excursion
                           (org-link-open-from-string
                            (plist-get (get-char-property (1- (length dest)) 'htmlize-link dest)
                                       :uri))
                           (point-marker))))
            (switch-to-buffer (marker-buffer marker))
            (goto-char marker)
            (message "Followed %s" dest)))))
(akirak/bind-mode :keymaps 'org-mode-map :package 'org
  "r" #'org-starter-refile-by-key)
(general-def :keymaps 'org-mode-map :package 'org
  "C-x i" #'helm-org-multi-wiki-insert-link)

;; Unbind `buffer-menu-open'
(general-unbind "<C-f10>")

(general-def :prefix "<C-f10>"
  ;; These commands are not included in this repository for now.
  "c" '(akirak/org-ql-search-category-todos :wk "category todos")
  "t" '(akirak/org-ql-search-tag-todos :wk "tag todos")
  ;; Others
  "v" #'org-ql-view)

(provide 'setup-org-starter)
