(defpackage #:quickdocs-api/controllers/dists
  (:use #:cl
        #:utopian)
  (:import-from #:quickdocs-api/views/dist
                #:show)
  (:import-from #:quickdocs-api/models
                #:dist
                #:dist-updated-projects)
  (:import-from #:assoc-utils
                #:aget)
  (:export #:show))
(in-package #:quickdocs-api/controllers/dists)

(defun is-new (row)
  (getf row :is-new))

(defun show (params)
  (let* ((version (aget params :version))
         (dist (mito:find-dao 'dist :version version)))
    (unless dist
      (throw-code 404))
    (let ((updated-projects (dist-updated-projects dist)))
      (render 'show
              :dist dist
              :new-projects (remove-if-not #'is-new updated-projects)
              :updated-projects (remove-if #'is-new updated-projects)))))
