(defpackage #:quickdocs-api/controllers/releases
  (:use #:cl
        #:utopian
        #:sxql)
  (:import-from #:quickdocs-api/models
                #:dist
                #:release)
  (:import-from #:quickdocs-api/views/release
                #:show)
  (:import-from #:assoc-utils
                #:aget)
  (:export #:show))
(in-package #:quickdocs-api/controllers/releases)

(defun find-dist (dist-version)
  (if dist-version
      (mito:find-dao 'dist
                     :name "quicklisp"
                     :version dist-version)
      (first (mito:select-dao 'dist
               (where (:= :name "quicklisp"))
               (order-by (:desc :version))
               (limit 1)))))

(defun show (params)
  (let* ((name (aget params :name))
         (dist-version (aget params "dist_version"))
         (dist (find-dist dist-version)))
    (unless dist
      (throw-code 404))
    (let ((release (first
                     (mito:select-dao 'release
                       (left-join :dist_release :on (:= :dist_release.release_id :release.id))
                       (where (:and (:= :dist_id (mito:object-id dist))
                                    (:= :release.name name)))
                       (limit 1)))))
      (unless release
        (throw-code 404))
      (render 'show
              :release release
              :dist dist))))
