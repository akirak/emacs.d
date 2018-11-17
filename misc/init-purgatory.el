(defhydra purgatory (:hint nil)
  "
_R_ refresh session
_s_ save all buffers   _x_ exit emacs

^^Desktop.el %s(abbreviate-file-name desktop-dirname)
_l_ clear         _C_ change dir
_L_ read desktop  _S_ save desktop
"
  ("R" akirak/refresh-session :exit t)
  ("s" (save-some-buffers t))
  ("x" kill-emacs)
  ("l" desktop-clear)
  ("C" desktop-change-dir)
  ("L" desktop-read)
  ("S" desktop-save))

(defalias 'purgatory 'purgatory/body)

(provide 'init-purgatory)
