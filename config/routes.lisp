(defpackage #:quickdocs-api/config/routes
  (:use #:cl
        #:utopian)
  (:export #:*routes*))
(in-package #:quickdocs-api/config/routes)

(defroutes *routes* ()
  (:controllers #P"../controllers/"))

(route :GET "/dists/:version" "dists:show")
