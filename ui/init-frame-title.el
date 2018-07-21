;;; init-frame-title.el --- Set the frame title format -*- lexical-binding: t -*-

(setq frame-title-format
      '(or (and (bound-and-true-p frame-workflow-mode)
                (frame-workflow--frame-subject-name))
           (and buffer-file-name
                (abbreviate-file-name buffer-file-name))
           dired-directory
           buffer-name))

(provide 'init-frame-title)
;;; init-frame-title.el ends here
