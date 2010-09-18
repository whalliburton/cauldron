;; client-packages.lisp

(defpackage paint-client
  (:use common-lisp utilities)
  (:import-from sb-ext quit)
  (:export paint start-paint-server))
