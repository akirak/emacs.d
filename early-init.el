;; Expand the GC threshold until gcmh-mode is activated.
;; gcmh-mode updates this value later, so you don't have to reset it.
;; The value is stolen from http://akrl.sdf.org/
(setq gc-cons-threshold #x40000000)
;; (setq gc-cons-threshold most-positive-fixnum)

;; package.el pre-initialization
(when (eq system-type 'gnu/linux)
  (require 'xdg)
  (setq package-user-dir (file-name-as-directory
                          (expand-file-name (format "emacs/elpa/%s"
                                                    emacs-major-version)
                                            (xdg-cache-home)))))
(setq package-enable-at-startup nil)

(setq auto-window-vscroll nil)

;; From doom-emacs
;; https://github.com/hlissner/doom-emacs/blob/develop/early-init.el

;; Disable graphical elements
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq frame-inhibit-implied-resize t)

;; Prevent resizing the initial frame
(setq inhibit-startup-screen t)

;; Ignore X resources
(advice-add #'x-apply-session-resources :override #'ignore)
