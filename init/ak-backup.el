;;; ak-backup.el --- backup and auto save settings -*- lexical-binding: t -*-

;; Copyright (C) 2018 by Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4"))

;;; Commentary:

;; This library sets the following options related to backup and auto saving:
;; - Specify a directory for auto-saved files and backup files
;; - Set backup options such as backup-by-copying

;; The following resources can be useful:
;; - https://www.emacswiki.org/emacs/AutoSave
;; - https://www.emacswiki.org/emacs/BackupDirectory
;; - https://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files

;;; Code:

(setq-default backup-by-copying t)

;; The parent directory of backup and auto save directories.
;; As the home directory can be modified at startup by a program like emacs-pg, 
;; use ~USER rather than simple ~ to denote the home directory.
(defconst akirak/temporary-directory
  (expand-file-name ".cache/emacs" (concat "~" user-login-name)))

(defconst akirak/backup-directory
  (expand-file-name "backup/" akirak/temporary-directory)
  "Directory for backup files.")

(defconst akirak/auto-save-directory
  (expand-file-name "auto-save/" akirak/temporary-directory)
  "Directory for auto save files.")

(setq backup-directory-alist `((".*" . ,akirak/backup-directory))
      auto-save-file-name-transforms `((".*" ,akirak/auto-save-directory t))
      auto-save-list-file-prefix akirak/auto-save-directory)

(provide 'ak-backup)

;;; ak-backup.el ends here
