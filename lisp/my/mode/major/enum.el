(defun akirak/known-major-mode-list ()
  (cl-loop for sym being the symbols of obarray
           when (get sym 'derived-mode-parent)
           collect sym))

(provide 'my/mode/major/enum)
