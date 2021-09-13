(defpackage #:quickdocs-api/controllers/projects/systems
  (:use #:cl
        #:utopian
        #:sxql)
  (:import-from #:quickdocs-api/models
                #:release
                #:system)
  (:import-from #:quickdocs-api/views/system
                #:release-systems)
  (:import-from #:mito)
  (:import-from #:assoc-utils
                #:aget)
  (:export #:listing))
(in-package #:quickdocs-api/controllers/projects/systems)

(defun listing (params)
  (let* ((name (aget params :name))
         (release
           (mito:find-dao 'release
                          :dist-name "quicklisp"
                          :name name))
         (systems
           (mito:select-dao 'system
             (where (:= :release release))
             (order-by :name))))
    (render 'release-systems
            :release release
            :systems systems)))
