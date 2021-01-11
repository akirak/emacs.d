;; Elixir configuration for Emacs.
;;
;; I've installed some of the packages listed in
;;
;; https://www.badykov.com/emacs/2020/05/30/emacs-setup-for-elixir/

(use-package elixir-mode)

(use-package mix
  :hook
  (elixir-mode . mix-minor-mode))

(use-package flycheck-credo
  :after elixir-mode
  :config
  (add-hook 'elixir-mode-hook
            (defun akirak/setup-flycheck-credo ()
              (flycheck-mode 1)
              (flycheck-credo-setup))))

;; Alchemist package for Elixir support on Emacs
;; See https://alchemist.readthedocs.io/en/latest/configuration/ for setup
;; (defconst akirak/alchemist-key-command-prefix "C-,")

(use-package alchemist
  :disabled t
  :after elixir-mode
  :general
  ;; alchemist-mode-keymap is defined as a prefix command, so this
  ;; works without setting `alchemist-key-command-prefix'.
  (:keymaps 'alchemist-mode-map
            akirak/alchemist-key-command-prefix #'alchemist-mode-keymap)
  ;; Somehow this doesn't seem to work.
  (:keymaps 'alchemist-mode-map
            :prefix akirak/alchemist-key-command-prefix
            "c" '(:ignore t :wk "compile")
            "e" '(:ignore t :wk "execute")
            "f" '(:ignore t :wk "point")
            "m" '(:ignore t :wk "mix")
            "mt" '(:ignore t :wk "mix-test")
            "X" '(:ignore t :wk "hex")
            "h" '(:ignore t :wk "help")
            "p" '(:ignore t :wk "project")
            "i" '(:ignore t :wk "iex")
            "v" '(:ignore t :wk "eval")
            "o" '(:ignore t :wk "macroexpand"))
  :custom
  (alchemist-key-command-prefix (kbd akirak/alchemist-key-command-prefix)))

(akirak/which-key-add-stripped-prefix "alchemist-")

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

(defun akirak/iex-mix (root)
  (interactive (list (or (locate-dominating-file default-directory "mix.exs")
                         (user-error "Cannot find mix.exs"))))
  (akirak/run-interactive-shell-command "iex -S mix"
    (format "*iex %s*" (f-filename root))
    :root root))

(akirak/bind-mode-repl :keymaps 'elixir-mode-map
  "" #'akirak/iex-mix)

(provide 'setup-elixir)
