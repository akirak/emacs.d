(use-package org-noter
  :after org-starter
  :custom
  (org-noter-default-notes-file-names
   `("noter-notes.org"
     ,(org-starter-locate-file "noter-notes.org" nil t))))

(provide 'setup-org-noter)
