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

(defun akirak/setup-display-failed-modules-if-any ()
  (when akirak/setup-failed-modules
    (with-current-buffer (generate-new-buffer "*failed modules*")
      (insert "The following modules have failed to load:\n"
              (mapconcat
               (lambda (module)
                 (propertize module
                             'button
                             (lambda (&rest _) (find-file (find-library-name module)))))
               akirak/setup-failed-modules "\n"))
      (pop-to-buffer (current-buffer)))))

(add-hook 'emacs-startup-hook 'akirak/setup-display-failed-modules-if-any)

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

(defcustom akirak/blacklisted-features nil
  "List of features to prevent loading.

This is applicable for modules loaded by `akirak/setup-load' and
`akirak/require'."
  :type '(repeat symbol)
  :group 'akirak)

(cl-defun akirak/setup-load (feature &optional severity
                                     &key (when t))
  "Load a configuration module.

FEATURE should be a module in ~/.emacs.d/setup.

If SEVERITY is non-nil, abort the initialization process."
  (when (and when
             (if (member feature akirak/blacklisted-features)
                 (progn
                   (message "Module %s is blacklisted. See akirak/blacklisted-features" feature)
                   nil)
               t)
             (not (require feature nil (not init-file-debug))))
    (add-to-list 'akirak/setup-failed-modules feature t)
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

(defun akirak/library-exists-p (name &optional verbose)
  (if (ignore-errors (find-library-name name))
      t
    (when verbose
      (message "%s package is unavailable" name))
    nil))
