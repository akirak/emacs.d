;; Rather than grouping init files by subdirectories,
;; I have decided to put all of them in a single directory,
;; like Kaushal Modi does.
;; https://github.com/kaushalmodi/.emacs.d

;; The new setup files should be prefixed with setup-, and
;; the directory is named setup save typing.

(add-to-list 'load-path (expand-file-name "setup" user-emacs-directory))

(defvar akirak/setup-failed-modules nil
  "List of modules failed to load.")

(defvar akirak/setup-module-worked-on nil)

(defun akirak/setup-find-failed-module ()
  "Open a module which has been failed to load."
  (interactive)
  (if-let* ((feature (setq akirak/setup-module-worked-on
                           (or akirak/setup-module-worked-on
                               (pop akirak/setup-failed-modules))))
            (filename (expand-file-name (concat "setup/" (symbol-name feature) ".el")
                                        user-emacs-directory)))
      (when (file-exists-p filename)
        (find-file filename)
        (display-buffer "*Backtrace*"))
    (message "No broken module left")))

(defun akirak/setup-load (feature &optional severity)
  "Load a configuration module.

FEATURE should be a module in ~/.emacs.d/setup.

If SEVERITY is non-nil, abort the initialization process."
  (unless (require feature nil t)
    (add-to-list akirak/setup-failed-modules feature t)
    (message "Failed to load %s" feature)
    (when severity
      (akirak/setup-find-failed-module)
      (error "Aborted due to a failed module."))))

(defalias 'akirak/require 'akirak/setup-load)

(defun akirak/load-babel-config-file (srcfile outfile)
  "Load an Org literate config file.

SRCFILE is the source Org file, and OUTFILE is the file name of an
output file without the directory."
  (require 'ob-tangle)
  (if (file-exists-p srcfile)
      (let ((enable-local-variables nil)
            (outpath (f-join user-emacs-directory ".cache" outfile)))
        (org-babel-tangle-file srcfile outpath)
        (load-file outpath))
    (message "%s does not exist. Maybe you haven't checked out submodules"
             config)))
