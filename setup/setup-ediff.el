(straight-use-package '(ediff :type built-in))

(use-package ediff
  :custom
  (ediff-split-window-function #'split-window-hozontally "Always split the window horizontally for ediff"))

;;;; Show the entire contents of Org buffers in ediff

(advice-add 'ediff-setup :before #'akirak/ad-before-ediff-setup-org)

(defun akirak/ad-before-ediff-setup-org (bufferA _fileA
                                                 bufferB _fileB
                                                 bufferC _fileC
                                                 &rest _args)
  (dolist (buffer (list bufferA bufferB bufferC))
    (when (and (bufferp buffer)
               (buffer-live-p buffer)
               (eq 'org-mode (buffer-local-value 'major-mode buffer)))
      (with-current-buffer buffer
        (org-show-all)))))

(provide 'setup-ediff)
