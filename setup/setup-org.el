(require 'org-habit)
(add-hook 'org-modules 'org-protocol)

(general-unbind "C-'" :keymaps 'org-mode-map :package 'org)

(when (bound-and-true-p akirak/mode-prefix-key)
  (general-translate-key nil 'org-mode-map
    :package 'org akirak/mode-prefix-key "C-c C-x")
  (general-translate-key nil 'org-agenda-mode-map
    :package 'org-agenda akirak/mode-prefix-key "C-c C-x"))

(general-create-definer akirak/bind-org-export
  :prefix (concat akirak/mode-prefix-key " X")
  :keymaps 'org-mode-map :package 'org)

(akirak/bind-org-export "" '(nil :wk "export shortcuts"))

(general-def :package 'org :keymaps 'org-mode-map :prefix akirak/mode-prefix-key
  "h" #'org-edit-headline
  "l" '(nil :wk "insert link")
  "s" '(nil :wk "set")
  "sc" #'akirak/org-set-category
  "c"
  (defun akirak/avy-org-clone-subtree ()
    "Copy the subtree to a location selected with avy.

If the subtree contains logbooks, they will be removed from the clone."
    (interactive)
    (assert (derived-mode-p 'org-mode))
    (assert (not (org-before-first-heading-p)))
    (org-copy-subtree 1)
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (insert (pop kill-ring))
      (goto-char (point-min))
      (while (re-search-forward org-logbook-drawer-re nil t)
        (replace-match ""))
      (avy-org-refile-as-child))))

(general-def :package 'org-agenda :keymaps 'org-agenda-mode-map :prefix akirak/mode-prefix-key
  "s" '(nil :wk "set")
  "sc" #'akirak/org-set-category)

(defmacro akirak/org-with-maybe-agenda-origin (&rest progn)
  `(cond
    ((derived-mode-p 'org-mode)
     ,@progn)
    ((derived-mode-p 'org-agenda-mode)
     (let ((marker (or (org-get-at-bol 'org-hd-marker)
                       (org-agenda-error))))
       (with-current-buffer (marker-buffer marker)
         (org-with-wide-buffer
          (goto-char (marker-position marker))
          ,@progn))))
    (t
     (user-error "Neither in org-mode nor org-agenda-mode"))))

(setq-default org-agenda-start-with-clockreport-mode t
              org-agenda-remove-tags t
              org-agenda-sticky t
              org-clock-history-length 20
              org-clock-mode-line-total (quote today)
              org-clock-out-remove-zero-time-clocks t
              org-clock-persist t
              org-clock-persist-query-resume nil
              org-enforce-todo-dependencies t
              org-log-done (quote time)
              org-log-into-drawer t
              org-log-refile nil
              org-outline-path-complete-in-steps nil
              org-refile-allow-creating-parent-nodes (quote confirm)
              org-refile-use-outline-path (quote full-file-path)
              org-habit-following-days 7
              org-habit-graph-column 55
              org-habit-preceding-days 14
              org-habit-scheduled-past-days 7
              org-habit-show-done-always-green t
              org-image-actual-width (ignore-errors
                                       (* 90 (/ (frame-text-width)
                                                (frame-width))))
              org-src-tab-acts-natively t
              org-startup-folded 'content
              org-startup-indented t
              ;; When `org-tag-persistent-alist' is non-nil,
              ;; `org-current-tag-alist' is set, and tags from
              ;; `org-get-buffer-tags' are unavailable in completion.
              ;;
              ;; To provide non-persistent tags, enable this option to
              ;; feed tags from all agenda files.
              ;;
              ;; This will be safe, since I use categories instead of
              ;; tags to identify work projects.
              org-complete-tags-always-offer-all-agenda-tags t
              org-startup-truncated nil
              org-use-speed-commands t
              org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
              org-track-ordered-property-with-tag t
              org-group-tags t
              ;; org-use-fast-tag-selection t
              ;; org-fast-tag-selection-single-key nil
              org-agenda-use-tag-inheritance t
              org-tags-exclude-from-inheritance '("ORDERED" "crypt")
              org-extend-today-until 5
              org-blank-before-new-entry '((heading . nil)
                                           (plain-list-item . auto))
              org-special-ctrl-a/e t
              org-M-RET-may-split-line nil
              org-imenu-depth 6
              ;; Prevent an annoying behaviour probably due to org's bug.
              org-link-search-must-match-exact-headline t
              ;; Based on https://lepisma.xyz/2017/10/28/ricing-org-mode/
              org-ellipsis " ⌄ "
              org-pretty-entities t
              org-hide-emphasis-markers t
              org-fontify-whole-heading-line t
              org-fontify-done-headline nil
              org-fontify-quote-and-verse-blocks t)

;; https://yiufung.net/post/org-mode-hidden-gems-pt1/
(setq-default org-cycle-separator-lines 0
              org-catch-invisible-edits 'show-and-error)

;; Prevent from saving org-refile and org-capture locations to bookmarks
(setq org-bookmark-names-plist nil)

(setq org-clock-goto-may-find-recent-task nil)

;; https://emacs.stackexchange.com/questions/21171/company-mode-completion-for-org-keywords
(defun org-add-completion-at-point ()
  (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point
            nil t))
(add-hook 'org-mode-hook #'org-add-completion-at-point)

(with-eval-after-load 'org-clock
  (org-clock-persistence-insinuate))

(defun akirak/org-add-empty-checkbox ()
  (interactive)
  (let ((checkbox-regexp (rx "[" (or "X" (optional space)) "] "))
        (item-regexp (rx bol (* space) "- ")))
    (cl-labels ((maybe-insert-checkbox
                 ()
                 (unless (looking-at checkbox-regexp)
                   (insert "[ ] "))))
      (if (region-active-p)
          (let* ((pos (point))
                 (beg (region-beginning))
                 (end (region-end))
                 (src (buffer-substring-no-properties beg end)))
            (delete-region beg end)
            (insert
             (with-temp-buffer
               (insert src)
               (goto-char (point-min))
               (while (re-search-forward item-regexp (point-max) t)
                 (maybe-insert-checkbox))
               (buffer-string)))
            (goto-char pos))
        (save-excursion
          (beginning-of-line)
          (when (re-search-forward item-regexp (line-end-position) t)
            (maybe-insert-checkbox)))))))

(general-def :keymaps 'org-mode-map :package 'org
  ;; I don't use any of these bindings and want to use them for other purposes
  "C-c [" nil
  "C-c ]" nil
  ;; M-up/down/left/right is unavailable on Chromebooks, so I need
  ;; alternative bindings for commands bound on those keys.
  "M-n" 'org-metadown
  "M-p" 'org-metaup
  "M-H" 'org-shiftmetaleft
  "M-L" 'org-shiftmetaright
  "C-1" 'counsel-org-tag
  [remap org-set-tags-command]
  (defun akirak/org-set-group-tag (&optional arg)
    "Set a tag from a group."
    ;; This is an alternative to `org-use-fast-tag-selection', which I
    ;; don't want to use because of its window configuration
    ;; management.
    (interactive "P")
    (let* ((tag-groups (org-tag-alist-to-groups org-current-tag-alist))
           (group (pcase arg
                    ((pred numberp)
                     (nth arg tag-groups))
                    ('(4)
                     (assoc (org-completing-read "Tag group: " tag-groups)
                            tag-groups))
                    (_
                     (car tag-groups))))
           (current-tags (org-get-tags))
           (tag (org-completing-read (format "Tag in group %s: " (car group))
                                     (cdr group))))
      (org-set-tags (cons tag (-difference current-tags (cdr group)))))))

(general-def :keymaps 'org-agenda-mode-map :package 'org
  "C-1" 'counsel-org-tag)

(akirak/bind-mode :keymaps 'org-mode-map :package 'org
  "t" 'akirak/org-table-create-or-edit
  "B" #'akirak/org-add-empty-checkbox)

(defun akirak/org-table-create-or-edit ()
  (interactive)
  (if (org-at-table-p)
      (akirak/org-table-hydra/body)
    (org-table-create)))

(defhydra akirak/org-table-hydra (:hint nil)
  "
Org Table

        ^^Insert  ^^Delete
Row     _ir_      _dr_
Column  _ic_      _dc_

Edit: _e_
Navigation: _n_ _p_ _f_ _b_
"
  ("dc" org-table-delete-column)
  ("dr" org-table-kill-row)
  ("ic" org-table-insert-column)
  ("ir" org-table-insert-row)
  ("e" org-edit-special)
  ("f" org-table-next-field)
  ("b" org-table-previous-field)
  ("n" org-table-next-row)
  ("p" previous-line))

(defun akirak/ad-around-org-beginning-of-line (orig n)
  (cond
   ((org-at-property-p)
    (if (bolp)
        (when-let* ((line (thing-at-point 'line))
                    (match (string-match org-property-re line)))
          (beginning-of-line 1)
          (right-char (string-match (regexp-quote (match-string 3 line))
                                    line)))
      (beginning-of-line n)))
   (t (funcall orig n))))
(advice-add 'org-beginning-of-line :around
            'akirak/ad-around-org-beginning-of-line)

(cl-defmacro akirak/org-define-set-property-command (property &key
                                                              default-value-fn
                                                              prohibit-override)
  "Define an interactive command which lets the user set a particular property."
  (declare (indent 1))
  (let ((command (intern (format "akirak/org-set-%s-property"
                                 (downcase (s-replace "_" "-" property))))))
    `(defun ,command ()
       ,(format "Set %s property of the subtree." property)
       (interactive)
       (cl-case current-prefix-arg
         ('(4) (princ (org-entry-get nil ,property t)))
         (otherwise
          (org-set-property ,property
                            ,(when default-value-fn
                               `(let ((existing (org-entry-get nil ,property)))
                                  (when (and existing ,prohibit-override)
                                    (user-error "This entry already has %s property, so don't override it"
                                                ,property))
                                  (read-string ,(format "Enter a value of %s property: "
                                                        property)
                                               (or existing
                                                   (funcall ,default-value-fn)))))))))))

(add-to-list 'which-key-replacement-alist
             '(("C-," . "akirak/org-set-") .
               (lambda (kb)
                 (if (string-match (rx bol "akirak/org-set-"
                                       (group (+ any))
                                       "-property" eol)
                                   (cdr kb))
                     (cons (car kb) (match-string 1 (cdr kb)))
                   kb))))

(defun akirak/org-default-custom-id ()
  (->> (org-get-heading t t t t)
       (org-link-display-format)
       (org-multi-wiki-default-custom-id-escape-fn)))

(akirak/org-define-set-property-command "CUSTOM_ID"
  :default-value-fn #'akirak/org-default-custom-id
  :prohibit-override t)

(general-def :keymaps 'org-read-date-minibuffer-local-map
  "C-p" (lambda () (interactive)
          (org-eval-in-calendar '(calendar-forward-week -1)))
  "C-n" (lambda () (interactive)
          (org-eval-in-calendar '(calendar-forward-week 1)))
  "<tab>" (lambda () (interactive)
            (org-eval-in-calendar '(calendar-forward-day 1)))
  "<S-iso-lefttab>" (lambda () (interactive)
                      (org-eval-in-calendar '(calendar-forward-day -1))))

(defun akirak/org-set-created-timestamp (&rest args)
  "Add a creation timestamp to the current Org entry.

If the current command is run with a prefix argument, prevent
from running."
  (unless current-prefix-arg
    (org-set-property "CREATED_TIME"
                      (org-timestamp-format
                       (org-timestamp-from-time (current-time) t t)
                       (org-time-stamp-format t t)))))

(advice-add #'org-insert-heading
            :after #'akirak/org-set-created-timestamp)

;;;; org-open-at-point

(defcustom akirak/desktop-file-extension-list
  '("xlsx")
  "List of file extensions to be handled by desktop applications.")

(defun akirak/org-open-at-point-with-xdg ()
  (ignore-errors
    (let* ((link (substring-no-properties (plist-get (get-text-property (point) 'htmlize-link) :uri)))
           (filename (and (string-match (rx bol "file:" (group (+ anything))) link)
                          (match-string 1 link))))
      (when (and filename
                 (member (file-name-extension filename) akirak/desktop-file-extension-list))
        (let* ((sha1 (car (s-match (rx bol (+ (not space)))
                                   (shell-command-to-string
                                    (format "sha1sum %s" (shell-quote-argument
                                                          (expand-file-name filename)))))))
               (directory (f-join (xdg-cache-home) "emacs" "xdg-open" (concat sha1 ".sha1")))
               (tmp-file-name (expand-file-name (s-match (rx (+ (any alpha digit "-_"))) (file-name-nondirectory filename))
                                                directory)))
          (unless (file-directory-p directory)
            (make-directory directory t))
          (make-symbolic-link filename tmp-file-name t)
          (message "Opening %s" tmp-file-name)
          (sleep-for 0 500)
          (start-process "xdg-open" nil "xdg-open" (expand-file-name tmp-file-name)))))))

(add-to-list 'org-open-at-point-functions 'akirak/org-open-at-point-with-xdg)

;;;; Misc
(defun akirak/org-yank-into-new-block (&optional arg)
  "Create a new block with the yanked text as its content.

With ARG, pick a text from the kill ring instead of the last one."
  (interactive "P")
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org-mode"))
  ;; TODO: Check if already inside a block
  (unless (looking-back (rx bol))
    (beginning-of-line 1))
  (let ((begin (point))
        (text (when arg
                (completing-read "Text: " kill-ring)))
        done)
    (unwind-protect
        (progn
          (if arg
              (insert text)
            (yank))
          ;; Select the pasted text.
          (push-mark begin)
          (setq mark-active t)
          (call-interactively #'org-insert-structure-template)
          (setq done t)
          ;; Unselect the pasted text
          (deactivate-mark)
          (let ((case-fold-search t))
            (save-excursion
              (goto-char begin)
              (when (looking-at (rx (* space) "#+begin_src" space))
                (let ((lang (completing-read "Language: "
                                             (->> (akirak/major-mode-list)
                                                  (-map (lambda (mode)
                                                          (string-remove-suffix "-mode"
                                                                                (symbol-name mode))))))))
                  (end-of-line 1)
                  (insert lang))))
            (re-search-forward (rx bol (* space) "#+end_")))
          ;; If there is whitespace at the beginning of the pasted text,
          ;; the block will have preceding space as well.
          ;;
          ;; Thus you have to re-indent the entire block to ensure
          ;; that it has no preceding space at the bol.
          (indent-region begin (point))
          (forward-line 1)
          ;; Insert an empty line.
          (unless (looking-at (rx eol))
            (insert "\n\n")
            (beginning-of-line 0)))
      (unless done
        ;; If the user has cancelled `org-insert-structure-template',
        ;; restore the previous state.
        (deactivate-mark)
        (delete-region begin (point))))))

(general-def :package 'org :keymaps 'org-mode-map :prefix "C-,"
  "y" #'akirak/org-yank-into-new-block)

(defun akirak/org-multi-wiki-archive-web-page (url)
  (interactive "sUrl: ")
  (when-let (matches (org-ql-select (->> '(refs voice)
                                         (--map (org-multi-wiki-entry-files it :as-buffers t))
                                         (apply #'append))
                       `(link ,url)
                       :action
                       '(format "%s:%s" (buffer-name) (nth 4 (org-heading-components)))))
    (user-error "There are %d matches for the URL: %s" (length matches) (car matches)))
  (let* ((refs-root (org-multi-wiki-directory 'refs))
         (dir-alist (append (->> (directory-files refs-root)
                                 (--filter (and (not (string-match-p "^\\." it))
                                                (f-directory-p (f-join refs-root it))))
                                 (--map (cons it (f-join refs-root it))))
                            (list (cons "voice" (org-multi-wiki-directory 'voice)))
                            (->> (akirak/major-mode-list)
                                 (--map (string-remove-suffix "-mode" (symbol-name it))))))
         (dir-name (completing-read "Directory: " dir-alist))
         (dir (or (alist-get dir-name dir-alist)
                  (f-join refs-root dir-name)))
         (entry (org-web-tools--url-as-readable-org url))
         (buffer (create-file-buffer (f-join dir "new-article.org")))
         (headings (with-current-buffer buffer
                     (insert entry)
                     (org-mode)
                     (goto-char (point-min))
                     (let ((case-fold-search nil)
                           headings)
                       (while (re-search-forward org-complex-heading-regexp nil t)
                         (let ((components (org-heading-components)))
                           (push (concat (make-string (1- (nth 0 components)) ?\s)
                                         (org-link-display-format (nth 4 components)))
                                 headings)))
                       (nreverse headings))))
         (title (completing-read "Title of the document: "
                                 headings))
         (filename (f-join dir (concat (funcall org-multi-wiki-escape-file-name-fn
                                                (string-trim title))
                                       ".org"))))
    (with-current-buffer buffer
      (setq buffer-file-name filename)
      (save-buffer)
      (goto-char (point-min))
      (rename-buffer title)
      (pop-to-buffer buffer))))

(use-package org-autolist
  :after org
  ;; :diminish 'org-autolist-mode
  :init
  (add-hook 'org-mode-hook #'org-autolist-mode))

;; Allow you to bookmark headings in Org-Mode
(use-package org-bookmark-heading
  :after org
  :straight (:host github :repo "akirak/org-bookmark-heading")
  :custom
  (org-bookmark-heading-filename-fn
   (defun akirak/org-bookmark-heading-filename (path)
     (let* ((path (abbreviate-file-name (expand-file-name path)))
            (project (project-current))
            (dir (abbreviate-file-name (file-name-directory path)))
            (filename (file-name-nondirectory path))
            (root (car-safe (ignore-errors (project-roots project)))))
       (if root
           (f-relative path (f-parent root))
         path))))
  (org-bookmark-heading-name-fn
   (defun akirak/org-bookmark-heading-name-fn (path heading)
     (if-let ((reverse-path (nreverse (ignore-errors (org-get-outline-path t)))))
         (format "%s:%s%s"
                 (akirak/org-bookmark-heading-filename path)
                 (org-link-display-format (car reverse-path))
                 (if (cdr reverse-path)
                     (format " (in %s)"
                             (substring-no-properties
                              (org-format-outline-path
                               (mapcar #'org-link-display-format
                                       (cdr reverse-path))
                               nil nil " < ")))
                   ""))
       (akirak/org-bookmark-heading-filename path)))))

(defun akirak/org-eldoc-heading ()
  (let ((outline (-> (org-get-outline-path t t)
                     (org-format-outline-path)))
        (category (org-get-category))
        (tags (org-get-tags-at nil t)))
    (--> (list (format "%s:" category)
               outline
               (when tags
                 (propertize (format ":%s:" (string-join tags ":"))
                             'face 'org-tag)))
         (-non-nil it)
         (string-join it " "))))

(defun akirak/org-get-last-inactive-timestamp ()
  (save-excursion
    (let ((subtree-end (save-excursion
                         (org-end-of-subtree)))
          match)
      (while (re-search-forward org-ts-regexp-inactive subtree-end t)
        (backward-char)
        (let ((elem (org-element-at-point)))
          (cl-case (org-element-type elem)
            (clock
             (let ((value (org-element-property :value elem)))
               (cl-case (org-element-property :type value)
                 (inactive-range
                  (push (list 0
                              (org-element-property :minute-end value)
                              (org-element-property :hour-end value)
                              (org-element-property :day-end value)
                              (org-element-property :month-end value)
                              (org-element-property :year-end value)
                              nil nil nil)
                        match)))))
            (node-property
             (push (org-parse-time-string
                    (org-element-property :value elem))
                   match)))))
      (car (sort (mapcar #'encode-time match) (-compose #'not #'time-less-p))))))

(defun akirak/org-get-state-log ()
  ;; This should start from the beginning of the heading
  (save-excursion
    (when-let* ((entry-end (org-entry-end-position))
                (logbook-end (save-excursion
                               (re-search-forward org-logbook-drawer-re entry-end t))))
      (when (re-search-forward (rx bol "- State ") logbook-end t)
        (let ((end (save-excursion
                     (when (re-search-forward
                            (rx (or (regexp org-clock-line-re)
                                    (and bol (or "- " ":END:")))))
                       (end-of-line 0)
                       (point)))))
          (string-trim-left
           (buffer-substring-no-properties
            (line-beginning-position) end)))))))

(defun akirak/org-eldoc-heading-with-extra ()
  (let* ((clock-sum (org-clock-sum-current-item))
         (created (org-entry-get nil "CREATED_TIME"))
         (last (akirak/org-get-last-inactive-timestamp))
         (done (org-entry-is-done-p))
         (scheduled (unless done (org-entry-get nil "SCHEDULED")))
         (deadline (unless done (org-entry-get nil "DEADLINE")))
         (state-log (-some->> (akirak/org-get-state-log)
                      (replace-regexp-in-string "\n" "")
                      (replace-regexp-in-string
                       (rx (+ (any space))) " ")
                      (replace-regexp-in-string
                       (rx bol "- ") ""))))
    (--> (list (when created
                 (format "Created %s" created))
               (when last
                 (format-time-string (concat "Last " (org-time-stamp-format t t)) last))
               (when (and clock-sum (> clock-sum 0))
                 (format "Spent %d min" clock-sum))
               (when scheduled
                 (format "Scheduled %s" scheduled))
               (when deadline
                 (format "Deadline %s" deadline))
               state-log)
         (-non-nil it)
         (string-join it ", ")
         (concat (akirak/org-eldoc-heading) " " it))))

(defun akirak/org-eldoc-function ()
  (if (org-at-heading-p)
      (akirak/org-eldoc-heading-with-extra)
    (or (-> (get-text-property (point) 'htmlize-link)
            (plist-get :uri))
        (akirak/org-eldoc-heading))))

(add-hook 'org-mode-hook
          (defun akirak/setup-org-eldoc ()
            (set (make-local-variable 'eldoc-documentation-function)
                 #'akirak/org-eldoc-function)
            (eldoc-mode t)))

(use-package org-superstar
  :custom
  (org-superstar-headline-bullets-list '("Ⅰ" "Ⅱ" "Ⅲ" "Ⅳ" "Ⅴ" "Ⅵ"))
  :hook
  (org-mode . org-superstar-mode))

(use-package company-org-block
  :after (company org)
  :disabled t
  :straight (:host github :repo "xenodium/dotsies"
                   :files ("emacs/ar/company-org-block.el"))
  :functions (company-org-block)
  :company org-mode
  :config
  ;; Originally from akirak/major-mode-list in my emacs-config-library repo
  (defmemoize company-org-block--major-mode-list ()
    (let (modes)
      (do-all-symbols (sym)
        (when-let* ((_nonminor (not (memq sym minor-mode-list)))
                    (name (symbol-name sym))
                    (_command (commandp sym))
                    (language (when (string-suffix-p "-mode" name)
                                (string-remove-suffix "-mode" name))))
          (when (and (let ((case-fold-search nil))
                       (string-match-p "^[a-z]" language))
                     (not (string-match-p (rx (or "global" "/")) language)))
            (push language modes))))
      modes))
  :config/el-patch
  (el-patch-defun company-org-block--candidates (prefix)
    "Return a list of org babel languages matching PREFIX."
    (seq-filter (lambda (language)
                  (string-prefix-p prefix language))
                ;; Flatten `org-babel-load-languages' and
                ;; `org-structure-template-alist', join, and sort.
                (seq-sort
                 #'string-lessp
                 (append
                  (el-patch-swap
                    (mapcar #'prin1-to-string
                            (map-keys org-babel-load-languages))
                    (company-org-block--major-mode-list))
                  (map-values org-structure-template-alist))))))

(use-package org-sidebar
  :straight (:host github :repo "alphapapa/org-sidebar")
  :custom
  (org-sidebar-side 'right)
  (org-sidebar-tree-side 'right))

(general-def "<f10>" #'org-agenda)

(use-package org-clock-convenience
  :after org-agenda
  :general
  (:keymaps 'org-agenda-mode-map
            "<S-up>" 'org-clock-convenience-timestamp-up
            "<S-down>" 'org-clock-convenience-timestamp-down
            ;; "ö" 'org-clock-convenience-fill-gap
            ;; "é" 'org-clock-convenience-fill-gap-both
            ))

(use-package org-crypt
  :after org
  :straight (:type built-in))

(use-package org-chronos
  :after org
  :straight (:host github :repo "akirak/org-chronos")
  :custom
  (org-chronos-scan-containing-file t)
  (org-chronos-logged-properties
   '(("LANGUAGE" :inherit t)
     ("OCTOPUS_REMOTE_REPO" :inherit t)))
  (org-chronos-show-property-summary t)
  (org-chronos-week-start-day 1)
  (org-chronos-clock-threshold 5)
  (org-chronos-auto-export t))

(defun akirak/org-find-or-create-logbook ()
  "Go to the end of the log book of the entry."
  (org-back-to-heading)
  (let ((bound (org-entry-end-position)))
    (if (re-search-forward org-logbook-drawer-re bound t)
        (beginning-of-line 1)
      (forward-line)
      (if (re-search-forward org-property-drawer-re bound t)
          (insert "\n")
        (while (looking-at org-planning-line-re)
          (forward-line)))
      (insert ":LOGBOOK:\n:END:\n")
      (beginning-of-line 0)))
  (point-marker))

(defun akirak/org-transfer-clock-entries (dest)
  (assert (markerp dest))
  (let ((dest-logbook (with-current-buffer (marker-buffer dest)
                        (org-with-wide-buffer
                         (goto-char dest)
                         (akirak/org-find-or-create-logbook)
                         (point-marker)))))
    (let (entries)
      (save-excursion
        (save-restriction
          (widen)
          (org-back-to-heading)
          (org-narrow-to-subtree)
          (while (re-search-forward (rx-to-string `(and bol (* (any " \\t"))
                                                        ,org-clock-string
                                                        (+ (any " \\t"))))
                                    nil t)
            (beginning-of-line 1)
            (let ((start (point))
                  (end (line-beginning-position 2)))
              (push (buffer-substring-no-properties (point) end) entries)
              (delete-region (point) end)
              (goto-char start)))
          (goto-char (point-min))
          (replace-regexp (rx bol (* (any " \\t")) ":LOGBOOK:\n"
                              (* (any " \\t"))  ":END:\n")
                          "")))
      (with-current-buffer (marker-buffer dest-logbook)
        (org-with-wide-buffer
         (goto-char dest-logbook)
         (while entries
           (insert (pop entries)))))
      (org-back-to-heading))))

(defun akirak/avy-org-transfer-clock-entries ()
  (interactive)
  (let ((dest (save-selected-window
                (save-excursion
                  (akirak/avy-org-heading t)
                  (point-marker)))))
    (akirak/org-transfer-clock-entries dest)))

(defun akirak/org-from-tsv (text &optional dest
                                 &key last-column-as-tags)
  (interactive (list (if (region-active-p)
                         (buffer-substring-no-properties
                          (region-beginning)
                          (region-end))
                       (x-get-selection))
                     'new-buffer
                     :last-column-as-tags
                     current-prefix-arg))
  (check-type text string)
  (let ((out (->> (split-string text "\n")
                  (-map (lambda (line)
                          (split-string line "\t")))
                  (-map (lambda (row)
                          (cond
                           ((-all-p #'string-empty-p row)
                            "")
                           ((-all-p #'string-empty-p (cdr row))
                            (concat "* " (car row) "\n"))
                           (t
                            (->> (-zip (number-sequence 1 (length row))
                                       (if last-column-as-tags
                                           (let* ((tags-cell (-last-item row))
                                                  (tags (unless (string-empty-p tags-cell)
                                                          (split-string tags-cell ",")))
                                                  (cells (-butlast row)))
                                             (append (-butlast cells)
                                                     (list (concat (-last-item cells)
                                                                   (if tags
                                                                       (format " :%s:"
                                                                               (mapconcat #'string-trim
                                                                                          tags ":"))
                                                                     "")))))
                                         row))
                                 (-map (pcase-lambda (`(,i . ,text))
                                         (if (string-empty-p text)
                                             ""
                                           (concat (make-string (1+ i) ?\*)
                                                   " " text "\n"))))
                                 (string-join))))))
                  (string-join))))
    (pcase dest
      ('new-buffer (with-current-buffer (generate-new-buffer "*org from tsv*")
                     (insert (format "%s" out))
                     (org-mode)
                     (pop-to-buffer (current-buffer))))
      (_ out))))

(akirak/bind-jump :keymaps 'org-mode-map :package 'org
  "SPC" #'org-babel-next-src-block
  "S-SPC" #'org-babel-previous-src-block)

(akirak/bind-search :keymaps 'org-mode-map :package 'org
  "s" #'org-babel-goto-named-src-block)

(general-def
  "C-c q"
  (defun akirak/org-start-activity-with-link (&optional arg)
    (interactive "P")
    (let* ((title (read-string "Title of the new entry: "))
           (template-body (concat "* STARTED " title "\n"
                                  ":PROPERTIES:\n"
                                  ":CREATED_TIME: "
                                  (org-format-time-string (org-time-stamp-format 'long 'inactive))
                                  "\n:END:\n"
                                  "%a"))
           (org-capture-entry (list "" "started entry with clock"
                                    'entry
                                    (list 'function
                                          (lambda ()
                                            (if (org-clocking-p)
                                                (progn
                                                  (org-goto-marker-or-bmk org-clock-marker)
                                                  (org-back-to-heading))
                                              ;; This function is part
                                              ;; of my config and not
                                              ;; part of the package.
                                              (org-journal-find-location))))
                                    template-body
                                    :immediate-finish t)))
      (org-capture)
      (save-window-excursion
        (org-capture-goto-last-stored)
        (org-clock-in))))
  "C-c #"
  (defun akirak/org-log-message-to-clock (msg)
    (interactive "sMessage: ")
    ;; TODO: Find a better way to compose the message
    (let* ((org-capture-entry (list "" "message"
                                    'plain
                                    (list 'function
                                          (lambda ()
                                            (org-goto-marker-or-bmk org-clock-marker)
                                            (goto-char (org-entry-end-position))))
                                    (concat msg "\n\n")
                                    :immediate-finish t)))
      (org-capture))))

(provide 'setup-org)
