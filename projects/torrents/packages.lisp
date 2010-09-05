;; packages.lisp

(defpackage torrents
  (:use common-lisp utilities databases bknr.datastore iterate)
  (:import-from sb-ext quit)
  (:export describe-torrent))
