;; torrent.lisp

(in-package :torrents)

(defclass torrent (blob)
  ()
  (:metaclass persistent-class))

(defun load-torrent (filename)
  (with-open-file (stream filename :element-type '(unsigned-byte 8))
    (bencode:decode (flex:make-flexi-stream stream))))

(defun symbol-to-string (symbol)
  (substitute #\space #\- (string-downcase symbol)))

;; http://bittorrent.org/beps/bep_0003.html

(defgeneric describe-torrent (torrent)
  (:method ((torrent hash-table))
    (with-hash-values ((announce created-by creation-date info comment)
                       torrent :to-key #'symbol-to-string)
      (with-hash-values ((name files piece-length) info :to-key #'symbol-to-string)
        (print-table 
         `(("name" ,name)
           ("created by" ,created-by)
           ("creation date" ,(local-time:unix-to-timestamp creation-date))
           ("announce" ,announce)
           ("comment" ,comment)
           ("piece length" ,piece-length)))
        (newline)
        (print-table 
         (iter (for file in files)
               (collect (list (car (gethash "path" file))
                              (gethash "length" file))))))))
  (:method ((filename string))
    (describe-torrent (load-torrent filename))))
