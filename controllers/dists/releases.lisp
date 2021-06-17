(defpackage #:quickdocs-api/controllers/dists/releases
  (:use #:cl
        #:utopian
        #:sxql)
  (:import-from #:quickdocs-api/views/release
                #:listing
                #:show)
  (:import-from #:quickdocs-api/models
                #:dist
                #:release
                #:dist-provided-releases-count)
  (:import-from #:assoc-utils
                #:aget)
  (:export #:listing
           #:show))
(in-package #:quickdocs-api/controllers/dists/releases)

(defun listing (params)
  (let* ((version (aget params :version))
         (page (or (aget params "page") 1))
         (per-page (or (aget params "per_page") 100))
         (dist (mito:find-dao 'dist :name "quicklisp" :version version)))
    (unless dist
      (throw-code 404))
    (let ((releases (mito:select-dao 'release
                      (where (:= :dist dist))
                      (order-by :release.name)
                      (limit per-page)
                      (offset (* (1- page) per-page)))))
      (render 'listing
              :releases releases
              :per-page per-page
              :page page
              :count (dist-provided-releases-count dist)))))

(defun show (params)
  (let* ((version (aget params :version))
         (name (aget params :name))
         (dist (mito:find-dao 'dist :name "quicklisp" :version version)))
    (unless dist
      (throw-code 404))
    (let ((release (mito:find-dao 'release
                                  :dist dist
                                  :name name)))
      (unless release
        (throw-code 404))
      (render 'show :release release))))
