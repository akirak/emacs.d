(setq akirak/init-directory (expand-file-name "init" default-directory))
(load-file "akirak.el")

(ert-deftest all-test ()
  (should (fboundp 'use-package))
  (should (fboundp 'which-key-mode))
  (should (fboundp 'akirak/startup)))
