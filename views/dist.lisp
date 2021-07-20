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
  (provided-releases-count nil :type integer)
  (new-projects nil :type list)
  (updated-projects nil :type list)
  (updated-at nil :type (or string local-time:timestamp)))

(defstruct project
  (name nil :type string)
  (description nil :type (or string null)))

(defun to-projects (rows)
  (mapcar (lambda (row)
            (apply #'make-project :allow-other-keys t row))
          rows))

(defview show ()
  (dist new-projects updated-projects)
  (:metaclass jzon-view-class)
  (:render
   (make-dist-json
     :name (dist-name dist)
     :version (dist-version dist)
     :provided-releases-count (dist-provided-releases-count dist)
     :new-projects (to-projects new-projects)
     :updated-projects (to-projects updated-projects)
     :updated-at (mito:object-created-at dist))))
