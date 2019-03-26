(use-package meghanada
  :init
  (cond
   ((eq system-type 'windows-nt)
    (setq meghanada-java-path (expand-file-name "bin/java.exe" (getenv "JAVA_HOME")))
    (setq meghanada-maven-path "mvn.cmd"))
   (t
    (setq meghanada-java-path "java")
    (setq meghanada-maven-path "mvn")))
  (defun akirak/load-meghanada ()
    (meghanada-mode 1)
    (flycheck-mode 1)
    (setq c-basic-offset 2)
    (add-hook 'before-save-hook 'meghanada-code-beautify-before-save nil t))
  :hook
  (java-mode . akirak/load-meghanada))

(provide 'setup-java-meghanada)
