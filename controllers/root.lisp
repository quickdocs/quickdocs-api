(defpackage #:quickdocs-api/controllers/root
  (:use #:cl
        #:utopian)
  (:import-from #:quickdocs-api/models
                #:find-latest-dist
                #:dist-version)
  (:import-from #:quickdocs-api/views/root
                #:ping)
  (:import-from #:quickdocs-api/views/dist
                #:show)
  (:export #:ping))
(in-package #:quickdocs-api/controllers/root)

(defun index (params)
  (declare (ignore params))
  (let ((dist (find-latest-dist)))
    (render 'show
            :dist dist)))

(defun ping (params)
  (declare (ignore params))
  (render 'ping))
