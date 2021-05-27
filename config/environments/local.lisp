(defpackage #:quickdocs-api/config/environments/local
  (:use #:cl))
(in-package #:quickdocs-api/config/environments/local)

`(:databases
  ((:maindb . (:postgres
               :database-name "quickdocs-api"
               :username "quickdocs-api"
               :password ""))))
