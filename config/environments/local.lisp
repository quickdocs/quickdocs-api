(defpackage #:quickdocs-api/config/environments/local
  (:use #:cl))
(in-package #:quickdocs-api/config/environments/local)

`(:databases
  ((:maindb . (:postgres
               :database-name "quickdocs"
               :username "quickdocs"
               :password "quickdocs"
               :microsecond-precision t))))
