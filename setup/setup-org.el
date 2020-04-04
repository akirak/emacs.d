(require 'org-habit)
(add-hook 'org-modules 'org-protocol)

(general-unbind "C-'" :keymaps 'org-mode-map :package 'org)

(when (bound-and-true-p akirak/mode-prefix-key)
  (general-translate-key nil 'org-mode-map
    :package 'org akirak/mode-prefix-key "C-c C-x")
  (general-translate-key nil 'org-agenda-mode-map
    :package 'org-agenda akirak/mode-prefix-key "C-c C-x"))

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
              org-src-tab-acts-natively t
              org-startup-indented t
              org-startup-truncated nil
              org-use-speed-commands t
              org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
              org-group-tags t
              ;; org-use-fast-tag-selection t
              ;; org-fast-tag-selection-single-key nil
              org-agenda-use-tag-inheritance t
              org-tags-exclude-from-inheritance '()
              org-blank-before-new-entry '((heading . nil)
                                           (plain-list-item . auto))
              org-special-ctrl-a/e t
              org-M-RET-may-split-line nil
              org-imenu-depth 6
              ;; Based on https://lepisma.xyz/2017/10/28/ricing-org-mode/
              org-ellipsis " ⌄ "
              org-pretty-entities t
              org-hide-emphasis-markers t
              org-fontify-whole-heading-line t
              org-fontify-done-headline t
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
  "C-1" 'counsel-org-tag)

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

(defmacro akirak/org-define-set-property-command (property)
  (let ((command (intern (format "akirak/org-set-%s-property"
                                 (downcase (s-replace "_" "-" property))))))
    `(defun ,command ()
       ,(format "Set %s property of the subtree." property)
       (interactive)
       (cl-case current-prefix-arg
         ('(4) (princ (org-entry-get nil ,property t)))
         (otherwise (org-set-property ,property nil))))))

(add-to-list 'which-key-replacement-alist
             '(("C-," . "akirak/org-set-") .
               (lambda (kb)
                 (if (string-match (rx bol "akirak/org-set-"
                                       (group (+ any))
                                       "-property" eol)
                                   (cdr kb))
                     (cons (car kb) (match-string 1 (cdr kb)))
                   kb))))

(akirak/org-define-set-property-command "CUSTOM_ID")

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
(defun akirak/org-yank-into-new-block ()
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org-mode"))
  ;; TODO: Check if already inside a block
  (unless (looking-back (rx bol))
    (beginning-of-line 1))
  (let ((begin (point))
        done)
    (unwind-protect
        (progn
          (yank)
          (push-mark begin)
          (setq mark-active t)
          (call-interactively #'org-insert-structure-template)
          (setq done t)
          (deactivate-mark)
          (let ((case-fold-search nil))
            (re-search-forward (rx bol "#+end_")))
          (forward-line 1)
          (unless (looking-at (rx eol))
            (insert "\n\n")
            (beginning-of-line 0)))
      (unless done
        (deactivate-mark)
        (delete-region begin (point))))))

(general-def :package 'org :keymaps 'org-mode-map :prefix "C-,"
  "y" #'akirak/org-yank-into-new-block)

(use-package org-entry-links
  :after org
  :straight (:host github :repo "akirak/org-entry-links")
  :config
  (akirak/bind-jump :keymaps 'org-mode-map :package 'org
    "L" #'org-entry-links-ivy)
  :general
  (:keymaps 'org-agenda-mode-map :package 'org-agenda
            "L" #'org-entry-links-ivy))

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

(use-package org-bullets :after org
  :custom
  (org-bullets-bullet-list '("Ⅰ" "Ⅱ" "Ⅲ" "Ⅳ" "Ⅴ" "Ⅵ"))
  :hook
  (org-mode . org-bullets-mode))

(use-package company-org-block
  :after (company org)
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

(provide 'setup-org)
