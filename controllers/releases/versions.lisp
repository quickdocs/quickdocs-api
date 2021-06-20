(defpackage #:quickdocs-api/controllers/releases/versions
  (:use #:cl
        #:utopian
        #:sxql)
  (:import-from #:quickdocs-api/views/release
                #:release-versions)
  (:import-from #:quickdocs-api/models
                #:release
                #:release-dist-version)
  (:import-from #:assoc-utils
                #:aget)
  (:export #:listing))
(in-package #:quickdocs-api/controllers/releases/versions)

(defun listing (params)
  (let* ((name (aget params :name))
         (page (or (aget params "page") 1))
         (per-page (or (aget params "per_page") 100))
         (releases
           (mito:select-dao 'release
             (where (:and (:= :dist_name "quicklisp")
                          (:= :name name)))
             (order-by (:desc :dist_version))
             (limit per-page)
             (offset (* (1- page) per-page))))
         (versions-count
           (mito:count-dao 'release
                           :dist-name "quicklisp"
                           :name name)))
    (render 'release-versions
            :versions (mapcar #'release-dist-version releases)
            :per-page per-page
            :page page
            :count versions-count)))
