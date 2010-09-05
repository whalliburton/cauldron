;; torrents.asd

(defsystem :torrents
  :serial t
  :components ((:static-file "torrents.asd")
	       (:file "packages")
	       (:file "torrent"))
  :depends-on (:bencode :flexi-streams :databases))


