;; torrent.lisp

(in-package :documents)

(defun load-torrent (filename)
  (with-open-file (stream filename :element-type '(unsigned-byte 8))
    (bencode:decode (flex:make-flexi-stream stream))))

(defun symbol-to-string (symbol)
  (substitute #\space #\- (string-downcase symbol)))

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
  (:method ((torrent torrent-document))
    (describe-torrent (load-torrent (namestring (blob-pathname torrent)))))
  (:method ((torrent-id integer))
    (describe-torrent (get-document torrent-id))))

;; http://bittorrent.org/beps/bep_0003.html
;; http://wiki.theory.org/BitTorrentSpecification

(defun torrent-info-sha1 (torrent)
  (coerce
   (mapcar #'code-char 
           (coerce (ironclad:digest-sequence :sha1 (bencode:encode (gethash "info" torrent) nil))
                   'list))
   'string))

(defgeneric track-torrent (torrent)
  (:method ((torrent hash-table))
    (with-hash-values ((announce) torrent :to-key #'symbol-to-string)
      (multiple-value-bind (body status-code headers uri stream to-close reason)
          (http-request announce 
                        :parameters `(("peer_id" . ,(random-string 10))
                                      ("info_hash" . ,(torrent-info-sha1 torrent))) 
                        :force-binary t )
        (declare (ignore headers uri stream to-close reason ))
        (when (eql status-code 200)
          (with-input-from-sequence (stream body)
            (let ((bencode::*binary-bdictionary-keys* '("peers")))
              (bencode:decode (flex:make-flexi-stream stream))))))))
  (:method ((torrent torrent-document))
    (track-torrent (load-torrent (namestring (blob-pathname torrent)))))
  (:method ((torrent-id integer))
    (track-torrent (get-document torrent-id))))

