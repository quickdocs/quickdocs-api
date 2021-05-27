(defpackage #:quickdocs-api/controllers/root
  (:use #:cl
        #:utopian
        #:quickdocs-api/views/root)
  (:export #:index))
(in-package #:quickdocs-api/controllers/root)

(defun index (params)
  (declare (ignore params))
  (render 'index-page))
