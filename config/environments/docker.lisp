(defpackage #:quickdocs-api/config/environments/docker
  (:use #:cl))
(in-package #:quickdocs-api/config/environments/docker)

`(:databases
  ((:maindb . (:postgres
               :host "maindb"
               :database-name "quickdocs"
               :username "quickdocs"
               :password "quickdocs"
               :microsecond-precision t))))
