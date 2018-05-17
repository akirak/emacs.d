;; I have stopped using this keymap as I bound this binding directory to
;; org-capture.
(define-prefix-command 'akirak/capture-map)

(general-def akirak/capture-map
  "c" 'org-capture
  "j" 'org-journal-new-entry
  "s" 'akirak/screenshot
  "v" 'akirak/kazam)

(provide 'init-capture-map)
