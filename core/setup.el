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

(cl-defun akirak/setup-load (feature &optional severity
                                     &key (when t))
  "Load a configuration module.

FEATURE should be a module in ~/.emacs.d/setup.

If SEVERITY is non-nil, abort the initialization process."
  (when (and when (not (require feature nil t)))
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

(defun akirak/running-on-crostini-p ()
  "Return non-nil if Emacs is running on Crostini of Chrome OS."
  (stringp (getenv-internal "SOMMELIER_VERSION")))

(defvar akirak/is-wsl)

(defun akirak/windows-subsystem-for-linux-p ()
  "Return non-nil if Emacs is running inside WSL."
  (if (boundp 'akirak/is-wsl)
      akirak/is-wsl
    (setq akirak/is-wsl (and (eq system-type 'gnu/linux)
                             (with-temp-buffer
                               (insert-file-contents "/proc/sys/kernel/osrelease")
                               (insert-file-contents "/proc/version")
                               (string-match-p (rx (or "Microsoft" "WSL"))
                                               (buffer-string)))))))

(defun akirak/os-like-debian-p ()
  (when (file-exists-p "/etc/os-release")
    (with-temp-buffer
      (insert-file-contents "/etc/os-release")
      (goto-char (point-min))
      (or (re-search-forward (rx bol "ID=" (?  "\"") "debian" (?  "\"") eol)
                             nil t)
          (re-search-forward (rx bol "ID_LIKE=" (? "\"") (* anything) "debian")
                             nil t)))))
