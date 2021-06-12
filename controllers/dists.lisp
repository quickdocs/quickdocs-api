(defpackage #:quickdocs-api/controllers/dists
  (:use #:cl
        #:utopian)
  (:import-from #:quickdocs-api/views/dist)
  (:import-from #:quickdocs-api/models
                #:dist)
  (:import-from #:assoc-utils
                #:aget)
  (:import-from #:com.inuoe.jzon
                #:stringify)
  (:import-from #:kebab
                #:to-snake-case)
  (:export #:show))
(in-package #:quickdocs-api/controllers/dists)

(defun show (params)
  (let* ((version (aget params :version))
         (dist (mito:find-dao 'dist :version version)))
    (unless dist
      (throw-code 404))
    (stringify dist
               :coerce-key #'to-snake-case)))
