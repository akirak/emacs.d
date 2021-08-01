;; Elixir configuration for Emacs.
;;
;; I've installed some of the packages listed in
;;
;; https://www.badykov.com/emacs/2020/05/30/emacs-setup-for-elixir/

(use-package elixir-mode
  :mode "\\.exs?\\'"
  :hook
  (elixir-mode . mix-format-on-save-mode))

(use-package mix
  :disabled t
  :hook
  (elixir-mode . mix-minor-mode))

(use-package flycheck-credo
  ;; Use lsp
  :disabled t
  :after elixir-mode
  :config
  (add-hook 'elixir-mode-hook
            (defun akirak/setup-flycheck-credo ()
              (flycheck-mode 1)
              (flycheck-credo-setup))))

(defconst akirak/elixir-outline-regexp
  (rx bol (or (seq (group (* space))
                   (group (or (sequence "def" (* alpha))
                              (sequence "@" (+ (not space)))
                              "alias"
                              "import"
                              "require"
                              "use"))
                   (+ space)
                   (group (*? any))
                   (or ", do:" (seq " do" eol) eol))
              (seq (group-n 1 "  ")
                   (group-n 2 (or (seq (not (any "e" space)) (* alpha))
                                  (seq "e" (not (any "n" space)) (* alpha))
                                  (seq "en" (not (any "d" space)) (* alpha))))
                   (+ space)
                   (group-n 3 (*? any))
                   (or ", do:" (seq " do" eol)))
              (seq (group-n 1 (* space))
                   (group-n 4 (sequence "def" (* alpha)))
                   "("
                   (group-n 5 (+ alnum))
                   (group-n 6 ", do:" (*\? any))
                   ")"
                   eol))))

(defun akirak/elixir-outline-title ()
  (let ((s1 (match-string 2))
        (s2 (match-string 3))
        (s3 (match-string 4)))
    (or (when (match-string 5)
          (concat (match-string 4)
                  " "
                  (match-string 5)
                  (match-string 6)))
        (pcase s1
          ("@derive" (concat s1 " " s2))
          ((pred (string-prefix-p "@")) s1)
          ("defstruct" s1)
          ;; ("defprotocol" s2)
          ("defprotocol" (concat s2 " " s3))
          ((pred (string-prefix-p "def")) (concat s1 " " s2 s3))
          (_ (concat s1 " " s2))))))

(defun akirak/elixir-outline-level ()
  (/ (length (match-string 1)) 2))

(defun akirak/elixir-counsel-outline-action (x)
  (counsel-outline-action x)
  (back-to-indentation))

(add-to-list 'counsel-outline-settings
             `(elixir-mode :outline-regexp ,akirak/elixir-outline-regexp
                           :outline-level akirak/elixir-outline-level
                           :outline-title akirak/elixir-outline-title
                           :action akirak/elixir-counsel-outline-action
                           :caller akirak/counsel-outline-elixir))

(defun akirak/counsel-outline-elixir-display-transformer (orig)
  (--> orig
       (split-string it counsel-outline-path-separator)
       (-map #'string-trim it)
       (-map (lambda (s)
               (cond
                ((string-match (rx bol
                                   (group "def" (* (not space)))
                                   " ")
                               s)
                 (let ((prefix (pcase (substring-no-properties (match-string 1 s))
                                 ("defmodule" nil)
                                 ("defp" nil)
                                 ("def" nil)
                                 (s1 s1)))
                       (body (string-remove-prefix (match-string 0 s) s)))
                   (if prefix
                       (concat (propertize prefix 'face 'font-lock-type-face)
                               " " body)
                     body)))
                ((string-prefix-p "@" s)
                 s)
                ((string-match (rx bol (* (not space))) s)
                 (let* ((prefix (substring-no-properties (match-string 0 s)))
                        (body (string-remove-prefix prefix s)))
                   (if (string-empty-p (string-trim body))
                       s
                     (concat (propertize prefix 'face 'font-lock-type-face)
                             body))))
                (t (error s))))
             it)
       (mapconcat #'identity it counsel-outline-path-separator)))

(ivy-set-display-transformer
 'akirak/counsel-outline-elixir
 'akirak/counsel-outline-elixir-display-transformer)

(use-package inf-elixir
  :after elixir-mode
  :config
  (setq-mode-local inf-elixir-mode compilation-minor-mode t)
  :general
  (:keymaps 'elixir-mode-map :package 'elixir-mode
            "C-c i" #'inf-elixir
            "C-c e" (general-predicate-dispatch 'inf-elixir-send-line
                      (region-active-p) 'inf-elixir-send-region)))

;; Deprecated. Use inf-elixir-project
(defun akirak/iex-mix (root)
  (interactive (list (or (locate-dominating-file default-directory "mix.exs")
                         (user-error "Cannot find mix.exs"))))
  (akirak/run-interactive-shell-command "iex -S mix"
    (format "*iex %s*" (f-filename root))
    :root root))

(akirak/bind-mode-repl :keymaps 'elixir-mode-map
  "" #'inf-elixir-project)

(defun akirak/elixir-module-name-from-file ()
  (let ((segments (f-split (buffer-file-name))))
    (when-let (i (-find-last-index
                  (lambda (x) (member x '("lib" "test")))
                  segments))
      (mapconcat (lambda (s)
                   (->> (split-string s "_")
                        (-map #'capitalize)
                        (string-join)))
                 (append (-slice segments (1+ i) -1)
                         (list (f-base (-last-item segments))))
                 "."))))

(provide 'setup-elixir)
