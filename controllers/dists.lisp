(defpackage #:quickdocs-api/controllers/dists
  (:use #:cl
        #:utopian)
  (:import-from #:quickdocs-api/views/dist
                #:show)
  (:import-from #:quickdocs-api/models
                #:dist)
  (:import-from #:assoc-utils
                #:aget)
  (:export #:show))
(in-package #:quickdocs-api/controllers/dists)

(defun show (params)
  (let* ((version (aget params :version))
         (dist (mito:find-dao 'dist :version version)))
    (unless dist
      (throw-code 404))
    (render 'show :dist dist)))
