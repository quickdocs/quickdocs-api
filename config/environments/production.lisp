(defpackage #:quickdocs-api/config/environments/production
  (:use #:cl
        #:utopian/config))
(in-package #:quickdocs-api/config/environments/production)

`(:databases
  ((:maindb . (:postgres
               :host ,(or (getenv "DB_HOST") "localhost")
               :port ,(or (getenv-int "DB_PORT") 5432)
               :database-name ,(or (getenv "DB_NAME") "quickdocs")
               :username ,(or (getenv "DB_USERNAME") "quickdocs")
               :password ,(or (getenv "DB_PASSWORD") "quickdocs")))))
