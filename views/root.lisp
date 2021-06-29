(defpackage #:quickdocs-api/views/root
  (:use #:cl)
  (:import-from #:utopian/views
                #:defview)
  (:import-from #:quickdocs-api/views
                #:jzon-view-class)
  (:import-from #:quickdocs-api/models
                #:dist-version
                #:dist-name)
  (:export #:index
           #:ping))
(in-package #:quickdocs-api/views/root)

(defview index ()
  (dist)
  (:metaclass jzon-view-class)
  (:render
   `(("dist_name" . ,(dist-name dist))
     ("dist_version" . ,(dist-version dist))
     ("updated_at" . ,(mito:object-created-at dist)))))

(defview ping ()
  ()
  (:metaclass jzon-view-class)
  (:render
   '(("pong" . "ok"))))
