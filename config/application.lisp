(defpackage #:quickdocs-api/config/application
  (:use #:cl
        #:utopian)
  (:import-from #:apispec)
  (:import-from #:lack.component
                #:to-app
                #:call)
  (:import-from #:lack
                #:builder)
  (:import-from #:cl-ppcre)
  (:export #:quickdocs-api-app))
(in-package #:quickdocs-api/config/application)

(defparameter *spec*
  (apispec:load-from-file
    (asdf:system-relative-pathname :quickdocs-api #P"./spec/openapi.yaml")))

(defapp quickdocs-api-app ()
  ()
  (:config #P"environments/"))

(defmethod call ((app quickdocs-api-app) env)
  (multiple-value-bind (operation path-parameters)
      (apispec:find-route (apispec:spec-router *spec*)
                          (getf env :request-method)
                          (getf env :path-info))
    (let ((*request* (if operation
                         (apispec:validate-request operation env
                                                   :path-parameters path-parameters)
                         *request*)))
      (call-next-method))))

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
   (:session :keep-empty nil)
   (call-next-method)))
