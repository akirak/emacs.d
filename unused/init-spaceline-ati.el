(use-package spaceline-all-the-icons
  :config
  (spaceline-define-segment frame-workflow
    "The current frame-workflow subject."
    (when-let ((subject (frame-workflow--frame-subject-name)))
      (format "%s %s"
              (propertize (all-the-icons-octicon "browser" :v-adjust 0)
                          'face `(:family ,(all-the-icons-octicon-family)
                                          :height ,(spaceline-all-the-icons--height 1.1)
                                          :inherit))
              (propertize subject 'face '(:height 0.8 :inherit))))
    :tight t
    :when (bound-and-true-p frame-workflow-mode))
  (spaceline-all-the-icons-theme 'frame-workflow)
  (spaceline-all-the-icons--setup-anzu)
  (spaceline-all-the-icons--setup-git-ahead)
  (spaceline-toggle-all-the-icons-which-function-off)
  :custom
  (spaceline-all-the-icons-hide-long-buffer-path t)
  (spaceline-all-the-icons-clock-always-visible nil))

(provide 'init-spaceline-ati)
