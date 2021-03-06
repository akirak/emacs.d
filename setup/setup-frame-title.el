;;;; Advices

;;;;; Unset a frame title set by frame-purpose

(defun akirak/ad-fr-make-frame-unset-frame-title (frame)
  (let ((title (frame-parameter frame 'title)))
    (set-frame-parameter frame 'title nil)
    (set-frame-parameter frame 'purpose-name title)
    frame))

(advice-add 'frame-purpose-make-frame
            :filter-return #'akirak/ad-fr-make-frame-unset-frame-title)

;;;;; Track an initially clocked task

;;;; Title components

(defun akirak/frame-title-subject ()
  (require 'project)
  (cond
   ((frame-parameter nil 'buffer-predicate)
    (format ":%s: " (frame-parameter nil 'purpose-name)))
   (t (format "%s: " (akirak/project-name)))))

(defun akirak/frame-title-body ()
  (let ((path (or buffer-file-name dired-directory)))
    (if (and path (projectile-project-p))
        (file-relative-name path (projectile-project-root))
      (buffer-name))))

(defun akirak/frame-title-task ()
  (when-let ((task (frame-parameter nil 'initial-clock-task)))
    (format " on %s" task)))

;;;; The frame title format

(setq frame-title-format
      '((:eval (akirak/frame-title-subject))))

(provide 'setup-frame-title)
