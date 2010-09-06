;; cpu.lisp

(in-package :hardware)

(defun cpu-count ()
  "Return the number of CPU's on this machine."
  (iter (with count = 0)
        (for line in-file "/proc/stat" using #'read-line)
        (when (and (string-starts-with line "cpu") (digit-char-p (char line 3)))
          (let ((num (parse-integer line :start 3 :junk-allowed t)))
            (when (< count num)
              (setf count num))))
        (finally (return (1+ count)))))

(load-foreign-library "/usr/lib/libcpufreq.so")

(defcfun "cpufreq_cpu_exists" :int (cpu :uint))
(defcfun "cpufreq_get_freq_kernel" :ulong (cpu :uint))
(defcfun "cpufreq_get_transition_latency" :ulong (cpu :uint))
(defcfun "cpufreq_get_hardware_limits" :int (cpu :uint) (min :pointer) (max :pointer))

(defun cpu-limits (cpu)
  (with-foreign-objects ((min :uint) (max :uint))
    (when (zerop (cpufreq-get-hardware-limits cpu min max))
      (values (mem-ref min :uint)
              (mem-ref max :uint)))))

(defun cpu-info ()
  "Print out information on this machines cpu(s)."
  (print-table 
   (iter (for cpu from 0 to (1- (cpu-count)))
         (collect (nconc (list cpu (cpufreq-get-freq-kernel cpu))
                         (multiple-value-list (cpu-limits cpu)))))
   :headings '("cpu" "frequency (kHz)" "min (kHz)" "max (kHz)")))