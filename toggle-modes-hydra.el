(require 'hydra)

(defun akirak//hy-flycheck-status ()
  (format "flycheck %s"
          (if flycheck-mode (or flycheck-checker "on ") "off")))

(defun akirak//hy-eldoc-status ()
  (format "eldoc %s" (if eldoc-mode "on " "off")))

(defun akirak//hy-flyspell-status ()
  (format "flyspell %s" (if flyspell-mode "on " "off")))

(defhydra akirak/toggle-modes-hydra
  (:hint nil)
  "
^^Sidebar     ^^Info       ^^^^^^^^^^^^^^^^^^  ^^Checkers      ^^^^^^^^^^^^^^^^^^
^^----------  ^^-----------^^^^^^^^^^^^^^^^^^  ^^--------------^^^^^^^^^^^^^^^^^^
_i_ imenu     _d_ %s(akirak//hy-eldoc-status)  _c_ %s(akirak//hy-flycheck-status)
_xd_ dired    ^^           ^^^^^^^^^^^^^^^^^^  -> _,c_ select
_xb_ ibuffer  ^^           ^^^^^^^^^^^^^^^^^^  _s_ %s(akirak//hy-flyspell-status)
"
  ("c" flycheck-mode)
  (",c" flycheck-select-checker)
  ("d" eldoc-mode)
  ("s" flyspell-mode)
  ("i" imenu-list-smart-toggle)
  ("xd" dired-sidebar-toggle-sidebar)
  ("xb" ibuffer-sidebar-toggle-sidebar)
  ("q" nil "quit"))

(provide 'toggle-modes-hydra)
