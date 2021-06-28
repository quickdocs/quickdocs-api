(defpackage #:quickdocs-api/config/environments/production
  (:use #:cl))
(in-package #:quickdocs-api/config/environments/production)

`(:databases
  ((:maindb . (:postgres
               :port 25432
               :database-name "quickdocs"
               :username "quickdocs"
               :password "quickdocs"))))
