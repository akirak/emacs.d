;;; init-google-translate.el --- Google Translate -*- lexical-binding: t -*-

(use-package google-translate
  :commands (google-translate-at-point
             google-translate-query-translate)
  :functions (google-translate-translate))

(provide 'init-google-translate)
;;; init-google-translate.el ends here
