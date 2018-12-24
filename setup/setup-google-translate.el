;;; setup-google-translate.el --- Google Translate -*- lexical-binding: t -*-

(use-package google-translate
  :commands (google-translate-at-point
             google-translate-query-translate)
  :functions (google-translate-translate))

(provide 'setup-google-translate)
;;; setup-google-translate.el ends here
