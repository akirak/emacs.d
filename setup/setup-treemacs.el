(use-package treemacs
  :preface
  (setq treemacs-python-executable (executable-find "python3"))
  (pcase (cons (not (null (executable-find "git")))
               (not (null treemacs-python-executable)))
    (`(t . t)
     (setq treemacs-git-mode 'deferred))
    (`(t . _)
     (setq treemacs-git-mode 'simple)))
  :general
  ("C-0" #'akirak/treemacs
   "C--" #'akirak/treemacs-find-file)
  :config
  (defvar akirak/treemacs-origin-window nil)
  (defun akirak/treemacs ()
    (interactive)
    (cond
     ((derived-mode-p 'treemacs-mode)
      (when akirak/treemacs-origin-window
        (select-window akirak/treemacs-origin-window)))
     (current-prefix-arg
      (treemacs))
     (t
      (setq akirak/treemacs-origin-window (selected-window))
      (treemacs-select-window))))

  (defun akirak/treemacs-find-file ()
    "Find the current file in treemacs."
    (interactive)
    (treemacs-find-file)
    (treemacs-select-window))

  (add-to-list 'treemacs-ignored-file-predicates
               (defun akirak/treemacs-ignored-file-predicate (filename path)
                 (member filename '(".direnv")))
               t)
  :custom
  (treemacs-follow-mode 1)
  (treemacs-filewatch-mode 1)
  (treemacs-fringe-indicator-mode 'always))

(use-package treemacs-icons-dired
  :after (dired)
  :hook
  (dired-mode . treemacs-icons-dired-mode))

(use-package treemacs-magit
  :disabled t
  :after (treemacs magit))

(provide 'setup-treemacs)
