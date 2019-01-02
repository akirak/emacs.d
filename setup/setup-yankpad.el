(use-package yankpad
  :after (org yasnippet))

(defun yankpad-hydra--format-active-snippets ()
  (mapconcat (lambda (ent)
               (concat (car ent) (if-let ((key (nth 1 ent)))
                                     (format " %s" key)
                                   "")))
             (yankpad-active-snippets)
             "\n"))

(defhydra yankpad-hydra (:hint nil)
  "
Category: %s`yankpad-category

%s(yankpad-hydra--format-active-snippets)

^Expand^       ^Category^           ^Snippets^
^^----------   ^^----------------   ^^-------------------
_SPC_ insert   _<tab>_     set      _e_ edit    _C_ capture
_m_   map      _<backtab>_ append   _R_ reload"
  ("SPC" yankpad-insert :exit t)
  ("m" yankpad-map :exit t)
  ("<tab>" yankpad-set-category)
  ("<backtab>" yankpad-append-category)
  ("e" yankpad-edit :exit t)
  ("R" yankpad-reload)
  ("C" yankpad-capture-snippet :exit t))

(provide 'setup-yankpad)
