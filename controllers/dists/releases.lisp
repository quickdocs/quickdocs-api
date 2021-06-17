(defpackage #:quickdocs-api/controllers/dists/releases
  (:use #:cl
        #:utopian
        #:sxql)
  (:import-from #:quickdocs-api/views/release
                #:listing)
  (:import-from #:quickdocs-api/models
                #:dist
                #:release
                #:dist-provided-releases-count)
  (:import-from #:assoc-utils
                #:aget)
  (:export #:listing))
(in-package #:quickdocs-api/controllers/dists/releases)

(defun listing (params)
  (let* ((version (aget params :version))
         (page (or (aget params "page") 1))
         (per-page (or (aget params "per_page") 100))
         (dist (mito:find-dao 'dist :name "quicklisp" :version version))
         (releases (mito:select-dao 'release
                     (where (:= :dist dist))
                     (order-by :release.name)
                     (limit per-page)
                     (offset (* (1- page) per-page)))))
    (render 'listing
            :releases releases
            :per-page per-page
            :page page
            :count (dist-provided-releases-count dist))))
