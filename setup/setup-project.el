;;; An opinionated infrastructure for projects -*- lexical-binding: t; -*-

;;;; Project definitions

(defcustom akirak/project-builder-alist
  '(("mix.exs"
     :name "Mix"
     :executable "mix"
     :languages (elixir)
     :helm-sources-fn akirak/helm-compile-mix-sources)
    (".recipes"
     :name "Emacs Lisp (elinter)"
     :executable "elinter"
     :languages (emacs-lisp)
     :command elinter)
    ("pnpm-lock.yaml"
     :name "Node.js (pnpm)"
     :executable "pnpm"
     :package "package.json"
     :languages (javascript typescript)
     :helm-sources-fn akirak/helm-compile-pnpm-sources)
    ("yarn.lock"
     :name "Node.js (yarn)"
     :executable "yarn"
     :package "package.json"
     :languages (javascript typescript)
     :helm-sources-fn akirak/helm-compile-yarn-sources)
    ("package-lock.json"
     :name "Node.js (npm)"
     :executable "npm"
     :package "package.json"
     :languages (javascript typescript)
     :helm-sources-fn akirak/helm-compile-npm-sources)
    ("spago.dhall"
     :name "PureScript Spago"
     :executable "spago"
     :languages (purescript)
     :helm-sources-fn akirak/helm-compile-spago-sources)
    ("Makefile"
     :name "Make"
     :executable "make"
     :function counsel-compile))
  "Alist of compilation backend settings.")

(defcustom akirak/project-language-alist
  '((typescript
     :human "TypeScript"
     :extensions (".ts" ".tsx"))
    (javascript
     :human "JavaScript"
     :extensions (".js" ".jsx"))
    (emacs-lisp
     :human "Emacs Lisp"
     :extensions (".el"))
    (elixir
     :human "Elixir"
     :extensions (".ex" ".exs"))
    (haskell
     :human "Haskell"
     :extensions (".hs"))
    (nix
     :human "Nix"
     :extensions (".nix")))
  "List of languages for source code.")

;;;; project.el integration

;; Fallback for Git modules.
(add-hook 'project-find-functions
          (defun akirak/project-git-root (dir)
            (when-let (root (locate-dominating-file dir ".git"))
              (cons 'git root))))

;; Fallback for Nix
(defmethod project-root ((project (head git)))
  (cdr project))

(add-hook 'project-find-functions
          (defun akirak/project-nix-store-root (dir)
            (save-match-data
              (when (string-match "^/nix/store/[^/]+/"
                                  dir)
                (cons 'nix-store (match-string 0 dir))))))

(defmethod project-root ((project (head nix-store)))
  (cdr project))

;; Build file
(defun akirak/project-find-build-root (dir)
  (cl-some (lambda (filename)
             (when-let (root (locate-dominating-file dir filename))
               (list 'builder filename root)))
           (mapcar #'car akirak/project-builder-alist)))

(add-hook 'project-find-functions #'akirak/project-find-build-root)

(defmethod project-root ((project (head builder)))
  (nth 2 project))

(defun akirak/project-builder (project)
  (pcase project
    (`(builder ,filename ,_)
     (assoc filename akirak/project-builder-alist))))

;;;; project-files

(defmethod project-files (project &optional dirs)
  (if dirs
      (-flatten-n 1 (-map (lambda (root)
                            (-map (lambda (path)
                                    (f-join root path))
                                  (akirak/project-files root)))
                          dirs))
    (let ((root (project-root project)))
      (--map (f-join root it) (akirak/project-files root)))))

;;;; compile integration

(defsubst akirak/project-builder-get-property (property builder)
  (plist-get (cdr builder) property))

(defun akirak/project-find-package-file (&optional func)
  (when-let* ((project (project-current))
              (builder (akirak/project-builder project)))
    (funcall (or func #'find-file)
             (expand-file-name (or (akirak/project-builder-get-property :package builder)
                                   (car builder))
                               (project-root project)))))

(defun akirak/project-call-build-command ()
  (require 'my/compile)
  (require 'my/helm/source/compile)
  (let* ((project (project-current))
         (builder (akirak/project-builder project))
         (root (project-root project)))
    (if builder
        (let* ((command (akirak/project-builder-get-property :command builder))
               (func (akirak/project-builder-get-property :function builder))
               (helm-sources-fn (akirak/project-builder-get-property :helm-sources-fn builder)))
          (when-let (executable (akirak/project-builder-get-property :executable builder))
            (unless (executable-find executable)
              (user-error "Required executable %s is not found" executable)))
          (cond
           (func
            (funcall func root))
           (command
            (let ((default-directory root))
              (call-interactively command)))
           (helm-sources-fn
            (let ((default-directory root))
              (helm :prompt (format "%s project at [%s]: "
                                    (akirak/project-builder-get-property :name builder)
                                    (f-short root))
                    :sources (funcall helm-sources-fn))))))
      (akirak/helm-shell-command))))

;;;; Directory structure

(defun akirak/project-parent-directories ()
  (cl-labels
      ((go (level parent)
           (if (= level 0)
               (list parent)
             (->> (f-directories parent)
                  (--map (go (1- level) it))
                  (-flatten-n 1)))))
    (->> magit-repository-directories
         (-map (pcase-lambda (`(,root . ,level))
                 (when (and (> level 0)
                            (f-directory-p root))
                   (go (1- level) root))))
         (-flatten-n 1)
         (-map #'f-slash)
         (-map #'f-short))))

;;;; Org integration
(use-package octopus
  :straight (octopus :host github :repo "akirak/octopus.el")
  :custom
  (octopus-session-value-source 'windows)
  (octopus-capture-finish-but-clock-in t)
  (octopus-project-org-properties '("LANGUAGE"))
  :general
  ("<S-f8>" #'octopus-switch-project)
  :config
  (general-add-hook 'octopus-select-hidden-tags
                    '("@project"
                      "ORDERED"))
  (require 'octopus-capture)
  (setq octopus-capture-template-alist
        `((todo
           ,(octopus-entry-capture-template
             :todo "TODO"
             :heading "%?"))
          (project
           ,(octopus-entry-capture-template
             :heading "%?"))
          (current-with-input
           (function
            (lambda ()
              (octopus-entry-capture-template
               :todo "STARTED"
               :heading "%i"
               :body (if (buffer-file-name)
                         "%a\n\n%?"
                       "%?"))))
           :immediate-finish t))))

(use-package helm-octopus
  :straight octopus)

(use-package octopus-hydra
  :straight octopus
  :commands (octopus-hydra))

(general-def
  "C-8" #'helm-octopus-project-scoped-ql
  "C-z" #'octopus-hydra)

;;;; TODO: Add support for initial setup workflow

(provide 'setup-project)
