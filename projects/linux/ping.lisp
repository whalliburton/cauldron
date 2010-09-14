;; ping.lisp

(in-package :linux)

;; As much as I'd rather do this all from Lisp, I have been unable to
;; get SBCL to run suid root.

;; The format expected is what I see with 

;; $ ping -V
;; ping utility, iputils-sss20071127

;; 64 bytes from ir1.fp.vip.sk1.yahoo.com (72.30.2.43): icmp_seq=1 ttl=46 time=119 ms

(defun ping (hostname)
  (when-let (rtn (second (process-lines (format nil "/bin/ping -c 1 ~s" hostname))))
    (multiple-value-bind (all subs)
        (#~c/.* bytes from (.*) \((.*)\): icmp_seq=(.*) ttl=(.*) time=(.*) ms/ rtn)
      (when subs
        (destructuring-bind (hostname ip icmp-seq ttl time) (coerce subs 'list)
          (values all hostname ip (parse-integer icmp-seq) (parse-integer ttl)
                  (parse-float time)))))))
