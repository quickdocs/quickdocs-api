(defpackage #:quickdocs-api/config/routes
  (:use #:cl
        #:utopian)
  (:export #:*routes*))
(in-package #:quickdocs-api/config/routes)

(defroutes *routes* ()
  (:controllers #P"../controllers/"))

(route :GET "/dists/:version" "dists:show")
(route :GET "/dists/:version/releases" "dists/releases:listing")
(route :GET "/releases/:name" "releases:show")
(route :GET "/releases/:name/versions" "releases/versions:listing")
