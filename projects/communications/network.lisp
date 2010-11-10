;; network.lisp

(in-package :communications)

(defun raw-network-statistics ()
  (mapcar #L(list (string-trim '(#\space) (first %))
                  (split-sequence #\space (second %) :remove-empty-subseqs t))
          (mapcar #L(split-sequence #\: %)
                  (subseq (slurp-lines "/proc/net/dev") 2))))

(defun network-statistics ()
  (mapcar #L(list (first %)
                  (second (second %))   ;revieved packets
                  (first (second %))    ;recieved bytes
                  (tenth (second %))    ;transmitted packets
                  (ninth (second %)))   ;transmitted bytes
          (raw-network-statistics)))

(defun network ()
  "Print network statistics."
  (print-table (network-statistics)
               :headings '("interface" "out packets" "out bytes" "in packets" "in bytes")))