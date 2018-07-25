(akirak/define-frame-workflow "planner"
  :key "p"
  :make-frame
  '(frame-purpose-make-mode-frame 'org-mode)
  :layout
  '(progn
     (delete-other-windows)
     (find-file (org-starter-locate-file "planner.org" nil t))))

(provide 'my-planner)
