;; TODO: Consider adapting http://emacs.rubikitch.com/bm-repository-open/

(use-package bm
  :commands (bm-toggle bm-next bm-previous))

(use-package helm-bm :after helm
  :commands (helm-bm))

;; Not using for now
(defhydra akirak/bm-hydra (:foreign-keys run :hint nil)
  "
bm
_p_ previous / _n_ next
"
  ("m" bm-toggle "toggle")
  ("n" bm-next)
  ("p" bm-previous)
  ("q" nil "quit"))

(provide 'init-bm)
