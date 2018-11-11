;;; init-ivy-frame-actions.el --- Ivy actions creating a new frame

;;;; Action functions

;; TODO: Integrate these functions with frame-workflow

(defun akirak/switch-buffer-some-frame (buffer)
  "Visit BUFFER in a new frame or some existing frame."
  (select-frame-set-input-focus (save-window-excursion
                                  (switch-to-buffer buffer)
                                  (make-frame))))

(defun akirak/find-file-some-frame (file)
  "Visit FILE in a new frame or some existing frame."
  (akirak/switch-buffer-some-frame (or (find-buffer-visiting file)
                                       (find-file-noselect file))))

(defun akirak/find-library-some-frame (library)
  (akirak/find-file-some-frame (find-library-name library)))

;;;; Adding actions

(with-eval-after-load 'ivy
  (ivy-add-actions 'ivy-switch-buffer
                   '(("F" akirak/switch-buffer-some-frame "frame"))))

(with-eval-after-load 'counsel
  (ivy-add-actions 'counsel-find-file
                   '(("F" akirak/find-file-some-frame "frame")))
  (ivy-add-actions 'counsel-load-library
                   '(("j" counsel-find-library-other-window "definition in other window")
                     ("F" akirak/find-library-some-frame "definition in frame")))
  (ivy-add-actions 'counsel-recentf
                   '(("f" akirak/find-file-some-frame "frame")
                     ("F" akirak/find-file-some-frame "frame"))))

(provide 'init-ivy-frame-actions)
