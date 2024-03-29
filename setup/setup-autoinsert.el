(auto-insert-mode 1)
(setq auto-insert 'other
      auto-insert-query nil
      auto-insert-alist `((
                           ;; ~/.emacs.d/lisp/my/**/*.el
                           ;; Add
                           ;; (provide 'DIRECTORY/BASENAME)
                           ;; e.g. (provide 'akirak/org-refile)
                           (,(rx "/lisp/my/" (+ anything) ".el" eos)
                            . "Extra elisp file")
                           . (> ";; -*- lexical-binding: t; -*-\n"
                                _ "\n\n"
                                "(provide '"
                                (file-relative-name
                                 (file-name-sans-extension
                                  (expand-file-name (buffer-file-name)))
                                 (expand-file-name "lisp" user-emacs-directory))
                                ")"))
                          ;; setup-*.el
                          ;; Add
                          ;; (provide 'BASENAME)
                          (("/\\(init\\|my\\|setup\\)-.+\\.el\\'"
                            . "Emacs init")
                           . (> _ "\n\n"
                                "(provide '"
                                (file-name-base (buffer-file-name))
                                ")"))
                          ;; Elisp dotfiles (.*.el)
                          ;; Noop
                          ((,(rx "/." (+ (not (any "/"))) ".el" eos) . "Configuration files") . nil)
                          ((,(rx "-test" (optional "s") ".el" eos) . "Test suite (buttercup)")
                           . (> ";;; -*- lexical-binding: t -*-\n\n"
                                "(require 'buttercup)\n"
                                "(require '" (s-replace-regexp "-tests?\\'" "" (file-name-base (buffer-file-name))) ")\n\n"
                                _ "\n\n"
                                "(provide '" (file-name-base (buffer-file-name)) ")\n"))
                          (("/dir-locals\\.nix\\'" . "nix-buffer")
                           . (>
                              "# This is a configuration for nix-buffer.el for Emacs.\n"
                              "# See <https://github.com/shlevy/nix-buffer>\n"
                              "let pkgs = import <nixpkgs> {}; in\n"
                              "pkgs.nixBufferBuilders.withPackages [\n"
                              "  " _
                              "\n]"))
                          ;; Melpa recipes
                          ;; Insert a minimal recipe definition
                          (("melpa/recipes/.+\\'" . "Melpa recipe")
                           . (> "("
                                (file-name-nondirectory (buffer-file-name))
                                " :fetcher github :repo \""
                                akirak/github-login
                                "/"
                                _
                                "\")"))
                          (("/notes/wiki/\[^/\]+\.org\\'" . "My wiki")
                           . (> "* " (akirak/unescape-wiki-file-name (file-name-base (buffer-file-name))) "\n"
                                ":PROPERTIES:\n:CREATED_TIME: " (format-time-string (org-time-stamp-format t t))
                                "\n:END:\n"))
                          (("README\.org\\'" . "README")
                           . (> "* " (f-filename default-directory) "\n"
                                _
                                "\n\n"
                                "# Add CI badges here\n\n"
                                "#+BEGIN_HTML\n#+END_HTML\n"
                                "** Table of contents\n:PROPERTIES:\n:TOC: siblings\n:END:\n"
                                "\n"
                                "** COMMENT Meta :noexport:\n"
                                ":PROPERTIES:\n:TOC:      ignore\n:END:\n"
                                "# The COMMENT keyword prevents GitHub's renderer from showing this entry.\n"
                                "# Local Variables:\n"
                                "# eval: (when (require (quote org-make-toc) nil t) (org-make-toc-mode t))\n"
                                "# End:\n"))
                          (("/config/config\.exs\\'" . "config/config.exs")
                           . (> "use Mix.Config\n\n"
                                _))
                          (("/lib/.+\.ex\\'" . "Elixir library module")
                           . (> "defmodule " (akirak/elixir-module-name-from-file) " do\n"
                                "  " _
                                "end"))
                          (("/test/.+_test\.exs\\'" . "Elixir test module")
                           . (> "defmodule " (akirak/elixir-module-name-from-file) " do\n"
                                "  use ExUnit.Case\n\n"
                                "  alias " (string-remove-suffix
                                            "Test" (akirak/elixir-module-name-from-file))
                                "\n"
                                "  doctest " (string-remove-suffix
                                              "Test" (akirak/elixir-module-name-from-file))
                                "\n"
                                "  " _
                                "\nend"))
                          (("\.go\\'" . "Go module")
                           . (> "package "
                                (file-name-base (or buffer-file-name (buffer-name)))
                                "\n\nimport (\n"
                                ")\n\n"))
                          (("\.el\\'" . "Emacs Lisp")
                           . (> ";;; "
                                (file-name-nondirectory (or buffer-file-name (buffer-name)))
                                " --- " _
                                " -*- lexical-binding: t -*-\n"
                                "\n\n\n"
                                "(provide '"
                                (file-name-base (or buffer-file-name (buffer-name)))
                                ")\n"
                                ";;; "
                                (file-name-nondirectory (or buffer-file-name (buffer-name)))
                                " ends here"))))

(provide 'setup-autoinsert)
