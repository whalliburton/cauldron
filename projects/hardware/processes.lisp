;; processes.lisp

(in-package :hardware)

(defclass process ()
  ((pid :initarg :pid :reader pid)
   (name :reader name)))

(defmethod print-object ((process process) stream)
  (with-slots (pid name) process
    (print-unreadable-object (process stream :type t)
      (format stream "~A ~A" pid name))))

(defun proc-status (pid)
  (process-string (slurp-lines (format nil "/proc/~a/status" pid))
                  '((:split #\:) (:trim #\space #\tab))))

(defun proc-cmdline (pid)
  (split-sequence #\Nul (slurp-line (format nil "/proc/~a/cmdline" pid)) 
                  :remove-empty-subseqs t))

;; from /usr/src/linux/fs/proc/array.c
;; also see 'man 5 proc'

;; 	seq_printf(m, "%d (%s) %c %d %d %d %d %d %u %lu \
;; %lu %lu %lu %lu %lu %ld %ld %ld %ld %d 0 %llu %lu %ld %lu %lu %lu %lu %lu \
;; %lu %lu %lu %lu %lu %lu %lu %lu %d %d %u %u %llu %lu %ld\n",
;; 	    0	pid_nr_ns(pid, ns),                     
;; 	    1	tcomm,                                  
;; 	    2	state,                                    
;; 	    3	ppid,
;; 	    4	pgid,
;; 	    5	sid,
;; 	    6	tty_nr,
;; 	    7	tty_pgrp,
;;          8 	task->flags,
;; 	    9	min_flt,
;; 	   10	cmin_flt,
;; 	   11	maj_flt,
;;         12	cmaj_flt,
;; 	   13	cputime_to_clock_t(utime),
;; 	   14	cputime_to_clock_t(stime),
;; 	   15	cputime_to_clock_t(cutime),
;; 	   16 	cputime_to_clock_t(cstime),
;; 	   17	priority,
;; 	   18	nice,
;; 	   19	num_threads,
;; 	   20	start_time,
;; 	   21	vsize,
;; 	   22	mm ? get_mm_rss(mm) : 0,
;; 	   23	rsslim,
;; 	   24	mm ? mm->start_code : 0,
;; 	   25	mm ? mm->end_code : 0,
;; 	   26	(permitted && mm) ? task->stack_start : 0,
;; 	   27	esp,
;; 	   28	eip,
;; 		/* The signal information here is obsolete.
;; 		 * It must be decimal for Linux 2.0 compatibility.
;; 		 * Use /proc/#/status for real-time signals.
;; 		 */
;; 	  29	task->pending.signal.sig[0] & 0x7fffffffUL,
;; 	  30	task->blocked.sig[0] & 0x7fffffffUL,
;; 	  31	sigign      .sig[0] & 0x7fffffffUL,
;; 	  32	sigcatch    .sig[0] & 0x7fffffffUL,
;; 	  33	wchan,
;; 	  34	0UL,
;; 	  35	0UL,
;; 	  36	task->exit_signal,
;; 	  37	task_cpu(task),
;; 	  38	task->rt_priority,
;; 	  39	task->policy,
;; 	  40	(unsigned long long)delayacct_blkio_ticks(task),
;; 	  41	cputime_to_clock_t(gtime),
;; 	  42	cputime_to_clock_t(cgtime));

(defun proc-stat (pid)
  (iter (for x upfrom 0)
        (for el in (split-sequence #\space (slurp-line (format nil "/proc/~a/stat" pid))))
        (awhen (cond
                 ((= x 1) (cons :comm (subseq el 1 (1- (length el)))))
                 ((= x 2) (cons :state
                                (case (char el 0)
                                  (#\R :running)
                                  (#\S :sleeping)
                                  (#\D :disk-sleep)
                                  (#\Z :zombie)
                                  (#\T :traced)
                                  (#\W :paging))))
                 ((= x 13) (cons :utime (parse-integer el)))
                 ((= x 14) (cons :stime (parse-integer el)))
                 ((= x 15) (cons :cutime (parse-integer el)))
                 ((= x 16) (cons :cstime (parse-integer el)))
                 )
          (collect it))))

(defmethod initialize-instance :after ((process process) &rest rest)
  (declare (ignore rest))
  (with-slots (pid name) process
    (setf name (cdr (assoc :comm (proc-stat pid))))))

(defun processes ()
  (mapcan 
   (lambda (el) 
     (awhen (ignore-errors (parse-integer (last1 (pathname-directory el))))
       (list (make-instance 'process :pid it))))
   (list-directory "/proc/")))

(defun describe-process (pid)
  (print-table (slot-value (make-instance 'process :pid pid) 'status)))

(defun processes-with-name (name)
  (remove-if-not (lambda (process) (string= (name process) name))
                 (processes)))

