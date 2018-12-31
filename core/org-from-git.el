;; Use a package manager to fresh install org mode

;; Required by org-git-version and org-release
(use-package git)

;; Workarounds for org-mode installed by straight.el
;; See https://github.com/raxod502/radian/blob/master/radian-emacs/radian-org.el
(defun org-git-version ()
  "The Git version of org-mode.
  Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (git-run "describe"
              "--match=release\*"
              "--abbrev=6"
              "HEAD"))))

(defun org-release ()
  "The release version of org-mode.
  Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (string-remove-prefix
      "release_"
      (git-run "describe"
               "--match=release\*"
               "--abbrev=0"
               "HEAD")))))

(provide 'org-version)

(use-package org
  :config
  (require 'org-loaddefs))

;; Workaround for a weird behaviour in `org-src-switch-to-buffer'
;; when `org-src-window-setup' is set to `split-window-below'.
;; It splits the window even when exiting the source buffer,
;; which is not what I expect.

(defun akirak/ad-around-org-src-switch-to-buffer (orig buffer context)
  (if (and (eq org-src-window-setup 'split-window-below)
           (memq context '(exit save)))
      (progn
        (delete-window)
        (if-let ((w (get-buffer-window buffer)))
            (select-window w)
          (select-window (split-window-sensibly))
          (switch-to-buffer buffer)))
    (funcall orig buffer context)))

(advice-add 'org-src-switch-to-buffer :around
            'akirak/ad-around-org-src-switch-to-buffer)
