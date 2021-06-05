(defpackage #:quickdocs-api/controllers/dists
  (:use #:cl
        #:utopian)
  (:import-from #:assoc-utils
                #:aget
                #:alist-hash)
  (:import-from #:com.inuoe.jzon
                #:stringify)
  (:export #:show))
(in-package #:quickdocs-api/controllers/dists)

(defun show (params)
  (stringify
    (alist-hash `(("version" . ,(aget params :version))))))
