(setq akirak/prevent-startup-on-loading t)
(add-to-list 'load-path (expand-file-name default-directory))
(add-to-list 'load-path (expand-file-name "init" default-directory))
(require 'akirak)
(require 'ak-keys)

(ert-deftest partial-test ()
  (should (fboundp 'general-define-key))
  (should (fboundp 'which-key-mode)))
