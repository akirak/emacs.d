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

;;;; TODO: Org integration
(use-package octopus
  :straight (octopus :host github :repo "akirak/octopus.el"))

(use-package helm-octopus
  :straight octopus
  :after helm-octopus)

;;;; TODO: Add support for initial setup workflow

(provide 'setup-project)
