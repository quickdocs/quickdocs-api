(defpackage #:quickdocs-api/config/routes
  (:use #:cl
        #:utopian)
  (:export #:*routes*))
(in-package #:quickdocs-api/config/routes)

(defroutes *routes* ()
  (:controllers #P"../controllers/"))

(route :GET "/dists/:version" "dists:show")
(route :GET "/dists/:version/releases" "dists/releases:listing")
(route :GET "/dists/:version/releases/:name" "dists/releases:show")
(route :GET "/projects/:name/versions" "projects/versions:listing")
(route :GET "/projects/:name" "projects:show")
(route :GET "/projects" "projects:search")
(route :GET "/badge/:name.svg" "projects:badge")

(route :GET "/ping" "root:ping")
(route :GET "/" "root:index")
