(defpackage #:quickdocs-api/controllers/root
  (:use #:cl
        #:utopian)
  (:import-from #:quickdocs-api/views/root
                #:ping)
  (:export #:ping))
(in-package #:quickdocs-api/controllers/root)

(defun ping (params)
  (declare (ignore params))
  (render 'ping))
