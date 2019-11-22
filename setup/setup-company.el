;;;; General settings

(use-package company
  :general
  ("C-M-/" #'company-complete)
  (:keymaps 'company-mode-map
            "M-/" #'company-complete)
  (:keymaps 'company-active-map :package 'company
            "C-f" #'company-complete-selection
            "C-n" #'company-select-next
            "C-p" #'company-select-previous
            "M-/" #'company-other-backend
            "<f1>" #'company-show-doc-buffer)
  :hook
  (prog-mode . company-mode)
  (company-mode . akirak/setup-company-backend)
  :custom
  (company-idle-delay nil)
  (company-auto-complete (lambda ()
                           (and (company-tooltip-visible-p)
                                (company-explicit-action-p))))
  (company-continue-commands '(not save-buffer
                                   save-some-buffers
                                   save-buffers-kill-terminal
                                   save-buffers-kill-emacs
                                   comint-previous-matching-input-from-input
                                   comint-next-matching-input-from-input
                                   completion-at-point))
  (company-dabbrev-other-buffers 'all "Search all buffers for company-dabbrev")
  (company-tooltip-align-annotations nil))

(use-package company-posframe
  :config/el-patch
  ;; Override company-posframe-show for displaying the list of active
  ;; backends in the mode line.
  (defface company-posframe-active-backend-name
    '((nil :inherit mode-line-emphasis))
    "Face for the active backend name in the header line.")

  (defun company-posframe-format-backend-name (backend)
    (cl-typecase backend
      (symbol (string-remove-prefix "company-" (symbol-name backend)))
      (list (format "[%s]" (mapconcat #'company-posframe-format-backend-name backend "|")))
      (otherwise "-")))

  (el-patch-defun company-posframe-show ()
    "Show company-posframe candidate menu."
    (let* ((height (min company-tooltip-limit company-candidates-length))
           (length company-candidates-length)
           (lines (company--create-lines company-selection height))
           (backend-names (mapconcat
                           (lambda (backend)
                             (if (equal backend company-backend)
                                 (propertize (company-posframe-format-backend-name backend)
                                             'face 'company-posframe-active-backend-name)
                               (company-posframe-format-backend-name backend)))
                           company-backends "|"))
           (width (length (car lines)))
           (contents (mapconcat #'identity lines "\n"))
           (buffer (get-buffer-create company-posframe-buffer)))
      ;; FIXME: Do not support mouse at the moment, so remove mouse-face
      (setq contents (copy-sequence contents))
      (remove-text-properties 0 (length contents) '(mouse-face nil) contents)
      (with-current-buffer buffer
        (setq-local overriding-local-map company-posframe-active-map)
        (setq-local mode-line-format `(,backend-names)))
      (posframe-show buffer
                     :string contents
                     :position (- (point) (length company-prefix))
                     :height (1+ height) :width width
                     :x-pixel-offset (* -1 company-tooltip-margin (default-font-width))
                     :respect-mode-line t
                     :font company-posframe-font
                     :min-width company-tooltip-minimum-width
                     :background-color (face-attribute 'company-tooltip :background))))
  :hook
  (company-mode . company-posframe-mode))

(use-package company-quickhelp
  :straight (:host github :repo "akirak/company-quickhelp"
                   :branch "dedicated-window")
  :after company
  :hook
  (company-mode . company-quickhelp-local-mode))

;;;; Specific backend configurations

(defvar akirak/company-major-backend-alist
  (mapcar (lambda (sym)
            (let ((name (symbol-name sym)))
              (cons (intern (concat name "-mode"))
                    (intern (concat "company-" name)))))
          '(shell
            restclient)))

(defun akirak/setup-company-backend ()
  (setq-local company-backends
              (let ((default-backends
                      '(company-capf
                        company-dabbrev
                        company-files
                        company-keywords)))
                (cond
                 ((bound-and-true-p lsp-mode)
                  (cons 'company-lsp default-backends))
                 (t
                  (if-let ((mode (apply #'derived-mode-p
                                        (mapcar #'car akirak/company-major-backend-alist))))
                      (list (alist-get mode akirak/company-major-backend-alist)
                            'company-capf)
                    default-backends))))))

;;;; Completion backends based on a minor mode

(use-package company-lsp
  ;; For company completion and snippets
  ;;
  ;; If the language server for the language doesn't support returning
  ;; snippets, you can define one one your own by customizing the
  ;; variable `company-lsp--snippet-functions'.
  ;; See the following document for details.
  ;; https://github.com/tigersoldier/company-lsp#defining-completion-snippet-for-a-certain-language
  :after (company lsp))

;;;; Completion backends based on a major mode

(use-package company-ansible
  :after (company ansible))

(use-package company-shell
  :after (company shell))

(use-package company-restclient
  :after (company restclient))

(provide 'setup-company)
