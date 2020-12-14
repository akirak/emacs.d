;;;; Tabs
(defvar-local akirak/centaur-tabs-buffer-groups nil
  "Explicitly-set groups of the current buffer.

This is intended to be set inside `akirak/set-header-line' function.")

(defsubst akirak/unset-buffer-group (&optional buffer-or-name)
  (with-current-buffer (or buffer-or-name (current-buffer))
    (setq akirak/centaur-tabs-buffer-groups nil)))

(advice-add #'bury-buffer :after #'akirak/unset-buffer-group)

(global-tab-line-mode t)

(use-package centaur-tabs
  :disabled t
  :config
  ;; Disable centaur-tabs in any buffers that are displayed using
  ;; fit-window-to-buffer.
  (advice-add #'fit-window-to-buffer
              :before #'akirak/disable-centaur-tabs-before-fit-window-buffer)
  (defun akirak/disable-centaur-tabs-before-fit-window-buffer
      (&optional window &rest args)
    (let ((buffer (if window
                      (window-buffer window)
                    (current-buffer))))
      (unless (local-variable-p 'centaur-tabs--local-hlf buffer)
        (with-current-buffer buffer
          (centaur-tabs-local-mode 1)))))
  (defun ceutaur-tabs-hide-tab (x)
    (or (string-match-p (rx (or "*helm"
                                "*direnv*"
                                "special*"
                                "*LV*"))
                        (format "%s" x))
        (derived-mode-p 'magit-process-mode)))
  (defun centaur-tabs-buffer-groups ()
    (or (and (eq major-mode 'exwm-mode)
             (list (format "EXWM-%s" exwm-class-name)))
        akirak/centaur-tabs-buffer-groups
        (and (string-match-p (rx bol (or "*Messages*"
                                         "*Warnings*"
                                         "*Backtrace*"))
                             (buffer-name))
             '("Errors"))
        ;; (list (centaur-tabs-get-group-name (current-buffer)))
        ))
  (centaur-tabs-mode 1)
  :general
  (:keymaps 'centaur-tabs-mode-map
            "<S-right>" #'centaur-tabs-forward
            "<S-left>" #'centaur-tabs-backward)
  :custom
  (centaur-tabs-style "bar")
  (centaur-tabs-set-icons t)
  (centaur-tabs-gray-out-icons 'buffer)
  (centaur-tabs-set-bar 'over)
  (centaur-tabs-set-close-button nil)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-modified-marker "*")
  (centaur-tabs-cycle-scope 'tabs))

;;;; Faces
(defface akirak/header-line-buffer-name
  '()
  "Face for the buffer name segment in a header line.")

(defface akirak/header-line-non-file-buffer-name
  '((default :inherit 'akirak/header-line-buffer-name
      :slant italic))
  "Face for non-file buffer names.")

(defface akirak/header-line-indirect-buffer-name
  '((default :inherit 'akirak/header-line-buffer-name
      :weight bold
      :slant italic))
  "Face for the buffer names of indirect buffers.")

(defface akirak/header-line-outline
  '((default :inherit font-lock-function-name-face))
  "Face for the function name or header in the header line.")

;;;; Setting the header line
(defvar-local akirak/orig-header-line-format nil)

(defun akirak/set-header-line ()
  (catch 'abort
    (let* ((modes (let ((mode major-mode)
                        modes)
                    (catch 'ok
                      (while mode
                        (push mode modes)
                        (setq mode (get mode 'derived-mode-parent))))
                    ;; TODO: Abort in helm
                    (when (or (window-minibuffer-p))
                      (throw 'abort nil))
                    modes))
           (project (akirak/project-name))
           (groups nil)
           (fmt (cond
                 ((memq 'lisp-interaction-mode modes)
                  (setq groups '("Scratch"))
                  nil)
                 ((bound-and-true-p git-commit-mode)
                  (setq groups '("Git"))
                  nil)
                 ((or (memq 'prog-mode modes)
                      (memq 'sgml-mode modes)
                      (memq 'json-mode modes))
                  (setq groups (list (if project
                                         (format "@%s:%s" project mode-name)
                                       mode-name)))
                  (akirak/make-header-line-format))
                 ((memq 'org-mode modes)
                  (cond
                   ;; An Org buffer has a header line if it is either an indirect buffer
                   ;; or a file buffer which does not reside inside ~/lib
                   ((and (buffer-base-buffer)
                         (not (string-prefix-p "CAPTURE-" (buffer-name))))
                    (akirak/make-header-line-format))
                   ((let ((file (buffer-file-name (org-base-buffer (current-buffer)))))
                      (and (stringp file)
                           (string-prefix-p (abbreviate-file-name org-journal-dir)
                                            (abbreviate-file-name file))))
                    (setq groups '("OrgJournal"))
                    nil)
                   ((let ((file (buffer-file-name (org-base-buffer (current-buffer)))))
                      (and (stringp file)
                           (or (not (string-prefix-p "~/lib/" (abbreviate-file-name file)))
                               (string-prefix-p "~/lib/notes/writing/" (abbreviate-file-name file)))))
                    (akirak/make-header-line-format))
                   (t
                    (setq groups '("Org"))
                    (akirak/make-header-line-format))))
                 ((and (buffer-file-name)
                       project)
                  (setq groups `(,project))
                  nil)
                 ((memq 'org-agenda-mode modes)
                  ;; '((:eval (and (featurep 'all-the-icons)
                  ;;               (all-the-icons-icon-for-buffer)))
                  ;;   " "
                  ;;   (:eval (pcase org-agenda-redo-command
                  ;;            (`(org-agenda-run-series ,desc . ,_)
                  ;;             (when-let ((key (caar (cl-remove-if-not
                  ;;                                    (lambda (list) (equal (nth 1 list) desc))
                  ;;                                    org-agenda-custom-commands))))
                  ;;               (format "[%s]%s" key (car (split-string desc ":")))))
                  ;;            (x (prin1-to-string x)))))
                  (setq groups '("Org Agenda"))
                  nil)
                 ((or (memq 'helpful-mode modes)
                      (memq 'help-mode modes)
                      (memq 'Info-mode modes)
                      (memq 'eww-mode modes))
                  (setq groups '("Emacs Help"))
                  nil)
                 ((memq 'comint-mode modes)
                  (setq groups '("REPL"))
                  nil)
                 ((memq 'exwm-mode modes)
                  nil)
                 ((memq 'dired-mode modes)
                  (progn
                    (setq dired-filter-header-line-format
                          `("  "
                            ,(if (featurep 'all-the-icons)
                                 (all-the-icons-icon-for-buffer)
                               'mode-name)
                            " "
                            dired-directory
                            " "
                            (:eval (dired-filter--describe-filters))))
                    nil))
                 ((memq 'vterm-mode modes)
                  (setq groups (list (if project
                                         (format "@%s:Terminal" project)
                                       "Terminal")))
                  nil)
                 ((string-prefix-p "magit-" (format "%s" major-mode))
                  (setq groups (list (if project
                                         (format "%s:Magit" project)
                                       "Git")))
                  nil))))
      (cond
       (fmt
        (setq header-line-format fmt)
        (unless groups
          (setq groups (ignore-errors
                         (list (cl-etypecase mode-name
                                 (string mode-name)
                                 (list (car mode-name))))))))
       (groups
        (setq akirak/centaur-tabs-buffer-groups groups))
       (t
        (when (fboundp 'centaur-tabs-local-mode)
          (centaur-tabs-local-mode 1)
          (setq header-line-format nil))))
      header-line-format)))

(add-hook 'after-change-major-mode-hook 'akirak/set-header-line)
(add-hook 'clone-indirect-buffer-hook 'akirak/set-header-line)

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-headerline-breadcrumb-mode-hook
            (defun akirak/revert-lsp-breadcrumb-headerline ()
              (when lsp-headerline-breadcrumb-mode
                ;; Remove the first element pushed by lsp-headerline
                (setq-local header-line-format (cdr header-line-format))))))

;; Make org-tree-to-indirect-buffer run clone-indirect-buffer-hook
;; after creating its indirect buffer.
(advice-add 'org-tree-to-indirect-buffer
            :after (cl-function
                    (lambda (&optional arg)
                      (run-hooks 'clone-indirect-buffer-hook))))

(add-hook 'org-capture-mode-hook #'akirak/set-up-org-capture-header-line)

(defun akirak/set-up-org-capture-header-line ()
  (unless akirak/orig-header-line-format
    (setq akirak/orig-header-line-format header-line-format)
    (setq header-line-format nil
          mode-line-format akirak/orig-header-line-format))
  (when (fboundp 'centaur-tabs-local-mode)
    (centaur-tabs-local-mode 0))
  (setq akirak/centaur-tabs-buffer-groups '("Org-Capture")))

;;;; Helper packages
(use-package flycheck-indicator
  :after flycheck)

;;;; Default header line format
(cl-defun akirak/make-header-line-format (&rest body &key omit-project &allow-other-keys)
  "Build a header line format with the standard set of segments."
  (require 'all-the-icons)
  (cl-remprop :omit-project body)
  (let* ((filep (when buffer-file-name t))
         (base-buffer (unless filep (buffer-base-buffer)))
         (indirectp (when base-buffer t))
         (file-name (cond
                     (filep buffer-file-name)
                     (base-buffer (buffer-file-name base-buffer))))
         (project (project-current))
         (project-root (when project
                         (car-safe (project-roots project))))
         (project-name (when project-root
                         (f-filename project-root)))
         (relative-name (when (and file-name project-root)
                          (file-relative-name file-name project-root)))
         (file-segment (when file-name
                         (propertize (or relative-name
                                         (file-name-nondirectory file-name))
                                     'face 'akirak/header-line-buffer-name)))
         (buffer-segment (unless filep
                           (propertize (buffer-name)
                                       'face (if indirectp
                                                 'akirak/header-line-indirect-buffer-name
                                               'akirak/header-line-non-file-buffer-name))))
         (icon (or (and (featurep 'all-the-icons)
                        (let ((file-name (buffer-file-name)))
                          (if (and file-name
                                   (bound-and-true-p polymode-mode))
                              (all-the-icons-icon-for-file file-name)
                            (all-the-icons-icon-for-buffer))))
                   mode-name)))
    `("  "
      ;; Display an icon for the mode if any
      ,icon
      " "
      ,(when project-name (format "[%s]" project-name))
      ;; If it is a file-visiting buffer, show the file name.
      ;; Otherwise, show the buffer name.
      " "
      ,(when indirectp "Indirect <")
      ,(if filep
           file-segment
         "%b")
      ,(when indirectp ">")
      ;; Display the statuses of the buffer
      " %* %n "
      ;; Display the column number if the buffer is in prog-mode
      ,(if (derived-mode-p 'prog-mode)
           "(%l,%3c) "
         " ")
      (lsp-headerline-breadcrumb-mode (:eval lsp-headerline--string))
      ;; Display the flycheck status in prog-mode
      ,(when (derived-mode-p 'prog-mode)
         (cond
          ((boundp 'flycheck-indicator-mode-line) flycheck-indicator-mode-line)))
      ;; Coding system
      ,(if filep
           '(:eval save-buffer-coding-system)
         "")
      ;; Append any segments
      ,@body)))

;;;; org-mode
;; Set the header line format for org-mode with the outline path.
(defvar akirak/header-line-org-outline-path-root-level nil)
(make-variable-buffer-local 'akirak/header-line-org-outline-path-root-level)

(defun akirak/header-line-org-outline-path ()
  (let ((indirect-p (buffer-base-buffer))
        (indirect-level akirak/header-line-org-outline-path-root-level)
        (path (unless (org-before-first-heading-p)
                (org-get-outline-path t t))))
    ;; If the buffer is an indirect buffer, store the level of the root
    (when (and indirect-p (not indirect-level))
      (setq indirect-level (save-excursion
                             (goto-char (point-min))
                             (length path))
            akirak/header-line-org-outline-path-root-level indirect-level))
    ;; If the buffer is an indirect buffer, trim the root path
    (when indirect-p
      (setq path (seq-drop path indirect-level)))
    ;; Continue if and only if the path is not null
    (when path
      (org-format-outline-path
       (let* ((orig-rev (nreverse path))
              (seg-length (pcase (length orig-rev)
                            ((pred (< 4)) 8)
                            ((pred (< 2)) 12)
                            (_ 20))))
         (nreverse
          (cons (car orig-rev)
                (mapcar (lambda (s)
                          (if (> (length s) seg-length)
                              (substring s 0 seg-length)
                            s))
                        (cdr orig-rev)))))))))

(provide 'setup-header-line)
