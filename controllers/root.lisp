(defpackage #:quickdocs-api/controllers/root
  (:use #:cl
        #:utopian)
  (:export #:index))
(in-package #:quickdocs-api/controllers/root)

(defun index (params)
  (declare (ignore params))
  "index")
