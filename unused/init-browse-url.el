;;; init-browse-url.el --- Configuration for browse-url -*- lexical-binding: t -*-

(when (executable-find "firefox")
  (setq browse-url-function 'browse-url-firefox))

(provide 'init-browse-url)
;;; init-browse-url.el ends here
