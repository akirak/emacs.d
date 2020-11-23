;; -*- lexical-binding: t; -*-

(defun akirak/with-ensure-git-submodule (submodule git-submodule-args
                                                   on-update
                                                   func &rest args)
  "Ensure a submodule is active and then call a function.

SUBMODULE is an alist containing information of the submodule. An
item returned from `akirak/toplevel-repos-submodules' is an
example.

GIT-SUBMODULE-ARGS is a list of optional arguments passed to \"git
submodule update\" command.

ON-UPDATE is a function called if and only i the submodule is
updated. It is called before FUNC.

FUNC is called with ARGS."
  (let-alist submodule
    (if \.active
        (apply func args)
      (message "Updating submodule %s in %s..." \.name \.root)
      (cl-labels ((make-sentinel (on-success on-error process event)
                                 (when (eq (process-status process) 'exit)
                                   (if (= (process-exit-status process) 0)
                                       (progn
                                         (message "Successfully run git submodule update")
                                         (funcall on-success))
                                     (funcall on-error)))))
        (let ((default-directory \.root))
          (magit-with-toplevel
            (make-process :name "git-submodule-update"
                          :command `("git"
                                     "submodule" "update"
                                     "--init"
                                     ,@git-submodule-args
                                     "--"
                                     ,\.name)
                          :sentinel
                          (-partial #'make-sentinel
                                    (-partial (lambda (root on-update func0)
                                                (when (functionp on-update)
                                                  (let ((default-directory root))
                                                    (funcall on-update)))
                                                (funcall func0))
                                              \.root
                                              on-update
                                              (-partial (-applify func) args))
                                    (-partial #'user-error
                                              "Failed to check out %s in %s"
                                              \.name
                                              \.root)))))))))

(provide 'my/gitmodule/function)
