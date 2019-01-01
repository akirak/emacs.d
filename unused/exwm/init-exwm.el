;; See
;; https://github.com/ch11ng/exwm/wiki
;; https://github.com/Ambrevar/dotfiles/blob/master/.emacs.d/lisp/init-exwm.el

(straight-override-recipe '(exwm :host github :repo "ch11ng/exwm"))

(straight-use-package 'exwm)

;; You can use this, but I don't like some part of the default config
;; (exwm-config-default)

(require 'exwm)
(require 'exwm-config)
(exwm-enable)

(setq shell-file-name "/bin/sh")

(provide 'init-exwm)
