(defpackage #:quickdocs-api/config/application
  (:use #:cl
        #:utopian)
  (:import-from #:lack.component
                #:to-app
                #:call)
  (:import-from #:lack
                #:builder)
  (:import-from #:cl-ppcre)
  (:export #:quickdocs-api-app))
(in-package #:quickdocs-api/config/application)

(defapp quickdocs-api-app ()
  ()
  (:config #P"environments/")
  (:content-type "text/html; charset=utf-8"))

(defmethod to-app ((app quickdocs-api-app))
  (builder
   (:static
    :path (lambda (path)
            (if (ppcre:scan "^(?:/assets/|/robot\\.txt$|/favicon\\.ico$)" path)
                path
                nil))
    :root (asdf:system-relative-pathname :quickdocs-api #P"public/"))
   :accesslog
   (:mito (db-settings :maindb))
   :session
   (call-next-method)))
