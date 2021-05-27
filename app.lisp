(defpackage #:quickdocs-api/app
  (:use #:cl
        #:quickdocs-api/config/routes
        #:quickdocs-api/config/application))
(in-package #:quickdocs-api/app)

(make-instance 'quickdocs-api-app
               :routes *routes*
               :models #P"models/")
