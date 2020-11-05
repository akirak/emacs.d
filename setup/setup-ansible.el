;; To activate =ansible= minor mode in specific projects, add the
;; following configuration to =.dir-locals.el=:
;;
;; #+begin_src emacs-lisp
;; ((yaml-mode
;;   (mode . ansible)))
;; #+end_src
(use-package ansible
  :straight (:type built-in)
  :after yaml
  :commands (ansible)
  :init
  (defalias 'ansible-mode 'ansible))

;;;; Detecting Ansible projects

(defun akirak/ansible-detect ()
  "Turn on `ansible' mode if you are inside an Ansible project."
  (when (akirak/ansible-project-p)
    (ansible 1)))

(add-hook 'yaml-mode-hook 'akirak/ansible-detect)

(defcustom akirak/ansible-playbook-files '("playbook.yml")
  "List of the file names of Ansible playbooks.")

(defvar akirak/ansible-project-p nil
  "non-nil if you are inside an Ansible project.

It is suggested that you set this variable as a directory-local
variable.")

(make-variable-buffer-local 'akirak/ansible-project-p)

(defun akirak/ansible-playbook-file-p ()
  "Check if the current buffer is an Ansible playbook."
  (and buffer-file-name
       (member (file-name-nondirectory buffer-file-name)
               akirak/ansible-playbook-files)))

(defun akirak/ansible-has-playbook-p ()
  "Check if the current project directory contains an Ansible playbook."
  (when-let ((root (car-safe (project-roots (project-current)))))
    (--some (file-exists-p (expand-file-name it root))
            akirak/ansible-playbook-files)))

(defun akirak/ansible-project-p ()
  "Check if you are inside an Ansible project."
  (or akirak/ansible-project-p
      (akirak/ansible-playbook-file-p)
      (akirak/ansible-has-playbook-p)))

(provide 'setup-ansible)
