(defhydra akirak/profiling-hydra (:pre (require 'profiler))
  "
Profiling

[_g_]: GC messages %(akirak/gc-verbose-status-string)
"
  ("g" (akirak/gc-verbose-toggle))
  ("c" (profiler-cpu-start profiler-sampling-interval) "CPU start" :exit t)
  ("m" (profiler-memory-start) "Mem start" :exit t)
  ("s" (profiler-stop) "Stop")
  ("R" (profiler-reset) "Reset")
  ("." (profiler-report) "Report"))

(defun akirak/gc-verbose-status-string ()
  (if (or (and (bound-and-true-p gcmh-mode)
               gcmh-verbose)
          garbage-collection-messages)
      "Yes"
    "No"))

(defun akirak/gc-verbose-toggle ()
  (if (bound-and-true-p gcmh-mode)
      (setq gcmh-verbose (not gcmh-verbose))
    (setq garbage-collection-messages
          (not garbage-collection-messages))))

(general-def "<f8>" 'akirak/profiling-hydra/body)

(provide 'setup-profiling)
