;;; akirak.el --- My main initialization file -*- lexical-binding: t no-byte-compile: t -*-

;; Copyright (C) 2018 by Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; URL: https://github.com/akirak/emacs.d

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;;;; Check the version of Emacs
(when (version< emacs-version "24.4")
  (error "Use GNU Emacs version 24.4 or later"))

;;;; Bootstrap straight.el package manager
;; See https://github.com/raxod502/straight.el#getting-started for details

;; In case straight.el has been already loaded,
;; the entire body is wrapped with `(unless (featurep 'straight) ...)`.
(unless (featurep 'straight)
  (let ((bootstrap-file (concat user-emacs-directory
                                "straight/repos/straight.el/bootstrap.el"))
        (bootstrap-version 3))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))

;;;; Define common facilities of the initialization framework

;; This is required in order to use `use-package`
(straight-use-package 'use-package)

;; Load prerequisites inside this library
(require 'cl)
(require 'subr-x)
(use-package dash :straight t)
(use-package dash-functional :straight t)

(defcustom akirak/init-directory (expand-file-name "init" user-emacs-directory)
  "Directory containing my initialization modules.

The default value of this variable is ~/.emacs.d/init. If you have checked out
my configuration repository to a directory other than ~/.emacs.d and 'init'
directory in the repository exists in a different location, you must set this
variable before the main library is loaded."
  :group 'akirak)

(defconst akirak/emacs-cache-directory (expand-file-name ".cache" user-emacs-directory)
  "Directory containing cache files used by Emacs Lisp applications.")

(defcustom akirak/init-file-blacklist '()
  "List of init files to ignore. Items in this list should be symbols excluding
the suffix \".el\". "
  :group 'akirak :type '(repeat symbol))

(defcustom akirak/preloaded-init-files
  '(ak-keys)
  "List of modules that should be reloaded before other modules.

The modules in this list are loaded by `akirak/load-init-files` in the order
before the other modules. The items should be specified as symbols excluding
\".el\" suffix. "
  :group 'akirak :type '(repeat symbol))

(defun akirak/load-init-files ()
  "Force reloading initialization files in akirak/init-directory.

Files contained in akirak/init-file-blacklist are skipped during this loading
process."
  (interactive)
  (->> (append
        (->> akirak/preloaded-init-files
             (-map 'symbol-name)
             (--map (concat it ".el"))) ; convert symbols to file names ending with .el
        (->> (directory-files akirak/init-directory nil "\.el$")
             (--remove (memq (intern (file-name-base it)) ; convert to symbol to check membership
                             (append akirak/preloaded-init-files
                                     akirak/init-file-blacklist)))))
       (--map (expand-file-name it akirak/init-directory)) ; convert to full path
       (mapc 'load-file))) ; load them

(defun akirak/startup ()
  "The normal startup initialization of my configuration."
  (add-to-list 'load-path akirak/init-directory)
  (akirak/load-init-files))

(defcustom akirak/prevent-startup-on-loading nil
  "Prevent from the normal startup when the library is loaded.

If you want to prevent from the normal startup process, set this value to
non-nil BEFORE the main library is loaded.")

;;;; Load initialization files right now
(unless akirak/prevent-startup-on-loading
  (akirak/startup))

(provide 'akirak)

;;; akirak.el ends here
