;;; init-emake.el --- Run emake -*- lexical-binding: t -*-

;;;; Assets from the example repository

(defun akirak/emake-example-file-url (filename &optional branch)
  "Return a source URL of FILENAME at BRANCH."
  (format "https://raw.githubusercontent.com/vermiculus/emake.el-example/%s/%s"
          (or branch "master") filename))

(defun akirak/write-emake-assets (&optional branch)
  "Download emake configuration source files except for Makefile.

BRANCH is a revision of the files to retrieve."
  (let ((project-root (projectile-project-root))
        (filenames '(".travis.yml" ".gitignore")))
    (dolist (filename filenames)
      (unless (or (not (file-exists-p (expand-file-name filename project-root)))
                  (yes-or-no-p (format "%s already exists. Overwrite it?" filename)))
        (user-error "Aborted")))
    (dolist (filename filenames)
      (let ((fpath (expand-file-name filename project-root))
            (url (akirak/emake-example-file-url filename "master")))
        (url-copy-file url fpath 'overwrite)))))

;;;; Generate an emake configuration for a simple project

(defun akirak/setup-simple-emake ()
  "Set up emake for a single-file project."
  (interactive)
  (akirak/write-emake-assets)
  (let* ((project-root (projectile-project-root))
         (package-basename (read-from-minibuffer "Package basename: "
                                                 (when (eq major-mode 'emacs-lisp-mode)
                                                   (file-name-base (buffer-file-name)))))
         (project-makefile (expand-file-name "Makefile" project-root))
         (url (akirak/emake-example-file-url "Makefile")))
    (unless (or (not (file-exists-p project-makefile))
                (yes-or-no-p (format "%s already exists. Overwrite it?" project-makefile)))
      (user-error "Aborted"))
    (url-copy-file url project-makefile 'overwrite)
    (find-file (expand-file-name "Makefile" project-root))
    (goto-char (point-min))
    (re-search-forward (rx bol "PACKAGE_BASENAME" (1+ space)
                           ":=" (1+ space) (group (1+ wordchar)) eol))
    (replace-match package-basename nil nil nil 1)
    (re-search-forward (rx "wget"))
    (replace-match "curl -O")))

;;;; Generate an emake configuration for a complex project

(defconst akirak/complex-emake-sha1 "9095599536e5b3ad8c34a3dd3362dbb92ebf701f")

(defun akirak/setup-complex-emake ()
  "Generate a Makefile for a complex emake project."
  (interactive)
  (akirak/write-emake-assets akirak/complex-emake-sha1)
  (with-current-buffer (generate-new-buffer "Makefile")
    (makefile-mode)
    (insert (akirak/complex-emake-template
             (directory-files (projectile-project-root)
                              nil "\\.el\\'")))
    (switch-to-buffer (current-buffer))))

(defun akirak/complex-emake-template (files)
  "Makefile template for a complex emake project."
  (concat "EMAKE_SHA1            := " akirak/complex-emake-sha1 "
PACKAGE_LISP          := " (string-join files " ") "

PACKAGE_ARCHIVES      := gnu melpa
#PACKAGE_TESTS         := test-sample.el # normally, EMake would discover these in the test/ directory
PACKAGE_TEST_DEPS     := package-lint
PACKAGE_TEST_ARCHIVES := gnu melpa

EMACS ?= emacs
CURL ?= curl

EMAKE = PACKAGE_LISP=\"$(PACKAGE_LISP)\" \\
	PACKAGE_ARCHIVES=\"$(PACKAGE_ARCHIVES)\" \\
	PACKAGE_TEST_DEPS=\"$(PACKAGE_TEST_DEPS)\" \\
	PACKAGE_TEST_ARCHIVES=\"$(PACKAGE_TEST_ARCHIVES)\" \\
	$(EMACS) -batch -l emake.el \\
	--eval \"(setq enable-dir-local-variables nil)\" \\
	$(EMACS_ARGS) \\
	--eval \"(emake (pop argv))\"

.PHONY: clean emacs setup install-emacs install lint compile

clean::                         ## clean all generated files
	rm -f *.elc             # delete compiled files
	rm -rf .elpa/           # delete dependencies
	rm -rf .elpa.test/
	rm -f emake.el

emake.el:                       ## download the EMake script
	curl -O 'https://raw.githubusercontent.com/vermiculus/emake.el/$(EMAKE_SHA1)/emake.el'

emacs-travis.mk:                ## download the emacs-travis.mk Makefile
	$(CURL) -O 'https://raw.githubusercontent.com/flycheck/emacs-travis/master/emacs-travis.mk'

emacs: emake.el                 ## report emacs version (installing $EMACS_VERSION if necessary)
	$(EMACS) -batch -l emake.el -f emake-verify-version 2>&1 || make install-emacs
	$(EMACS) --version

setup: emacs

install-emacs: emacs-travis.mk	## build and install a fresh emacs
	export PATH=\"$(HOME)/bin:$(PATH)\"
	make -f emacs-travis.mk install_emacs

.elpa: emake.el

install: .elpa
"
(mapconcat (lambda (file) (format "	PACKAGE_FILE=%s $(EMAKE) install" file))
           files "\n")
"
lint: install
	$(EMAKE) test package-lint
	$(EMAKE) test checkdoc

compile: install
	rm -f $(PACKAGE_LISP:.el=.elc)
	$(EMAKE) compile ~error-on-warn"))

(provide 'init-emake)
;;; init-emake.el ends here
