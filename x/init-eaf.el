(use-package eaf
  :straight (eaf :host github
                 :repo "manateelazycat/emacs-application-framework")
  :config
  (defun akirak/eaf-install-deps-maybe ()
    (interactive)
    (unless (featurep 'init-eaf-deps)
      (eshell-command "sudo pip install dbus-python PyMuPDF grip qrcode pyqt5 python-xlib")
      (provide 'init-eaf-deps)))
  (advice-add #'eaf-open
              :before (lambda (_) (akirak/eaf-install-deps-maybe)))
  (setq eaf-python-file
        (f-join user-emacs-directory "straight/repos/emacs-application-framework" "eaf.py")))

(provide 'init-eaf)
