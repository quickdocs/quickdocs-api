(defpackage #:quickdocs-api/views/dist
  (:use #:cl)
  (:import-from #:utopian
                #:defview)
  (:import-from #:quickdocs-api/views
                #:jzon-view-class)
  (:import-from #:quickdocs-api/models
                #:dist-name
                #:dist-version
                #:dist-provided-releases-count)
  (:export #:show))
(in-package #:quickdocs-api/views/dist)

(defstruct dist-json
  (name nil :type string)
  (version nil :type string)
  (provided-releases-count nil :type integer))

(defview show ()
  (dist)
  (:metaclass jzon-view-class)
  (:render
   (make-dist-json
     :name (dist-name dist)
     :version (dist-version dist)
     :provided-releases-count (dist-provided-releases-count dist))))
