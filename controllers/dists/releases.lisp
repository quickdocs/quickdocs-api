(defpackage #:quickdocs-api/controllers/dists/releases
  (:use #:cl
        #:utopian
        #:sxql)
  (:import-from #:quickdocs-api/views/release)
  (:import-from #:quickdocs-api/views
                #:make-pagination)
  (:import-from #:quickdocs-api/models
                #:dist
                #:release
                #:dist-provided-releases-count)
  (:import-from #:assoc-utils
                #:aget
                #:alist-hash)
  (:import-from #:com.inuoe.jzon
                #:stringify)
  (:import-from #:kebab
                #:to-snake-case)
  (:export #:listing))
(in-package #:quickdocs-api/controllers/dists/releases)

(defun listing (params)
  (let* ((version (aget params :version))
         (page (or (aget params "page") 1))
         (per-page (or (aget params "per_page") 100))
         (dist (mito:find-dao 'dist :version version))
         (releases (mito:select-dao 'release
                     (where (:= :dist dist))
                     (limit per-page)
                     (offset (* (1- page) per-page)))))
    (stringify
      (make-pagination
        :per-page per-page
        :page page
        :count (dist-provided-releases-count dist)
        :items releases))))
