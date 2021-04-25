(defun akirak/run-desktop-file (desktop-file)
  (async-start-process "dex" "dex" nil desktop-file))

(use-package counsel
  ;; :diminish counsel-mode
  :config/el-patch
  (el-patch-defun counsel-linux-apps-parse (desktop-entries-alist)
    (let (result)
      (setq counsel-linux-apps-faulty nil)
      (dolist (entry desktop-entries-alist result)
        (let* ((id (car entry))
               (file (cdr entry))
               (r (counsel-linux-app--parse-file file)))
          (when r
            (push (el-patch-swap (cons r id)
                                 (cons r file))
                  result))))))
  (el-patch-defun counsel-linux-app-action-default (desktop-shortcut)
    "Launch DESKTOP-SHORTCUT."
    (el-patch-swap (call-process "gtk-launch" nil 0 nil (cdr desktop-shortcut))
                   (akirak/run-desktop-file (cdr desktop-shortcut))))
  :config
  (ivy-decorator-set-intermediate 'counsel-M-x
      #'intern-soft
    (command-name-and-key 35)
    (function-doc))
  (ivy-decorator-set-intermediate 'counsel-describe-function
      #'intern-soft
    (original 40)
    (function-doc))
  (ivy-decorator-set-intermediate 'counsel-describe-variable
      #'intern-soft
    (original 30)
    (variable-doc))
  ;; counsel-rg may fail in a direnv + shell.nix + lorri environment,
  ;; so I included the absolute path of rg in the command line.
  (setq counsel-rg-base-command
        (cl-typecase counsel-rg-base-command
          (string (replace-regexp-in-string (rx bol "rg ")
                                            (let ((exec-path (default-value 'exec-path)))
                                              (concat (executable-find "rg")
                                                      " "))
                                            counsel-rg-base-command))
          (list (cons (executable-find "rg")
                      (cdr counsel-rg-base-command)))
          (otherwise counsel-rg-base-command)))
  (counsel-mode 1) ; Remap built-in functions with counsel equivalents
  (general-unbind :keymaps 'counsel-mode-map "<menu>")
  (ivy-add-actions #'counsel-find-library
                   '(("l" load-library "load")
                     ("g" akirak/magit-status-of-library "git repo")
                     ("d" akirak/dired-of-library "dired")
                     ("r" akirak/open-library-readme "readme")
                     ("u" akirak/straight-update-package "update package")))
  (cl-loop for (command find-other-window)
           in '((counsel-describe-function find-function-other-window)
                (counsel-describe-variable find-variable-other-window)
                (counsel-M-x find-function-other-window))
           do (ivy-add-actions command
                               `(("j" ,(-compose find-other-window 'intern)
                                  "definition in other window"))))
  (ivy-add-actions #'counsel-describe-function
                   '(("e" akirak/open-eval-expression-with-function "eval")))
  (ivy-add-actions #'counsel-rg
                   `(("j" ,(-partial #'akirak/counsel-git-grep-action-with-find-file
                                     #'find-file-other-window)
                      "other window")
                     ("f" ,(-partial #'akirak/counsel-git-grep-action-with-find-file
                                     #'find-file-other-frame)
                      "other frame")))
  (ivy-add-actions #'counsel-find-file
                   '(("gs" magit-status "magit-status")))
  (global-set-key [remap recentf-open-files] 'counsel-recentf)
  (global-set-key [remap insert-char] 'counsel-unicode-char)
  (general-def :keymaps 'counsel-mode-map "M-y" nil)
  (cl-loop for (command . str) in ivy-initial-inputs-alist
           do (when (and (symbolp command)
                         (string-prefix-p "counsel-" (symbol-name command)))
                (delq (assoc command ivy-initial-inputs-alist)
                      ivy-initial-inputs-alist)))
  ;; Let counsel-find-file-at-point choose the file under cursor
  ;; https://www.reddit.com/r/emacs/comments/7wjyhy/emacs_tip_findfileatpoint/du1xlbg/
  (setq counsel-find-file-at-point (not (akirak/windows-subsystem-for-linux-p)))

  (ivy-add-actions 'counsel-find-file
                   `(("c"
                      ,(lambda (file)
                         (let ((dest (read-file-name (format "Destination [%s]: " file)
                                                     (file-name-directory file))))
                           (if (file-name-directory dest)
                               (copy-file file (f-join dest (f-file-name file)))
                             (copy-file file dest))))
                      "Copy")
                     ("t"
                      ,(lambda (file)
                         (let ((default-directory file))
                           (vterm)))))))

;; TODO: Add todo occur command based on counsel-rg

(defun akirak/straight-update-package (x)
  (let ((package x))
    (straight-pull-package package)
    (straight-rebuild-package package)
    (load-library x)))

(defun akirak/counsel-find-file-magit-status ()
  (interactive)
  (ivy-exit-with-action
   #'magit-status))

(defun akirak/ad-after-counsel-org-goto-action (_x)
  (org-show-entry))
(advice-add 'counsel-org-goto-action :after
            'akirak/ad-after-counsel-org-goto-action)

(defun akirak/open-eval-expression-with-function (x)
  (let ((exp (let ((minibuffer-completing-symbol t)
                   (prompt "Eval: ")
                   (initial-contents (format "(%s )" x)))
               ;; Stolen from the implementation of `read--expression' in simple.el.gz
               (minibuffer-with-setup-hook
                   (lambda ()
                     ;; Put the cursor inside the brackets
                     (backward-char 1)
                     ;; FIXME: call emacs-lisp-mode?
                     (add-function :before-until (local 'eldoc-documentation-function)
                                   #'elisp-eldoc-documentation-function)
                     (eldoc-mode 1)
                     (add-hook 'completion-at-point-functions
                               #'elisp-completion-at-point nil t)
                     (run-hooks 'eval-expression-minibuffer-setup-hook))
                 (read-from-minibuffer prompt initial-contents
                                       read-expression-map t
                                       'read-expression-history)))))
    (pp-eval-expression exp)))

(defun akirak/counsel-git-grep-action-with-find-file (find-file-func x)
  "Go to occurrence X in current Git repository."
  (when (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" x)
    (let ((file-name (match-string-no-properties 1 x))
          (line-number (match-string-no-properties 2 x)))
      (funcall find-file-func (expand-file-name
                               file-name
                               (ivy-state-directory ivy-last)))
      (goto-char (point-min))
      (forward-line (1- (string-to-number line-number)))
      (re-search-forward (ivy--regex ivy-text t) (line-end-position) t)
      (swiper--ensure-visible)
      (run-hooks 'counsel-grep-post-action-hook)
      (unless (eq ivy-exit 'done)
        (swiper--cleanup)
        (swiper--add-overlays (ivy--regex ivy-text))))))

(defun akirak/magit-status-of-library (x)
  (if-let ((path (find-library-name x))
           (repo (locate-dominating-file (file-truename path) ".git")))
      (magit-status repo)
    (user-error "Cannot find library or its directory %s" x)))

(defun akirak/dired-of-library (x)
  (if-let ((path (find-library-name x))
           (truename (file-truename path)))
      (dired-jump nil (cond
                       ((equal truename path) path)
                       ((yes-or-no-p "Follow symbolic link? ")
                        truename)
                       (t path)))
    (user-error "Cannot find library or its directory %s" x)))

(defun akirak/find-readme (dir)
  (interactive )
  (if-let (files (or (directory-files dir t (rx bol "README" (+ (any alpha ".")) eol))
                     (directory-files dir t (rx (or ".org" ".md") eol))))
      (akirak/view-file (if (= 1 (length files))
                            (car files)
                          (completing-read (format "README for %s: " x)
                                           files nil t)))
    (dired-other-window dir)
    (message "No readme found for %s" x)))

(defun akirak/open-library-readme (x)
  (let* ((path (find-library-name (cl-etypecase x
                                    (string x)
                                    (symbol (symbol-name x)))))
         (dir (if path
                  (file-name-directory (file-truename path))
                (user-error "Cannot find library %s" x))))
    (akirak/find-readme dir)))

(defun akirak/view-file (filename)
  (let ((buffer (or (find-buffer-visiting filename)
                    (find-file-noselect filename))))
    (with-current-buffer buffer
      (view-mode 1))
    (pop-to-buffer buffer)))

(provide 'setup-counsel)
