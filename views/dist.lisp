(defpackage #:quickdocs-api/views/dist
  (:use #:cl)
  (:import-from #:quickdocs-api/views)
  (:import-from #:quickdocs-api/models
                #:dist)
  (:import-from #:com.inuoe.jzon
                #:coerced-fields))
(in-package #:quickdocs-api/views/dist)

(defmethod coerced-fields :around ((dist dist))
  (let ((fields (call-next-method)))
    (remove-if-not (lambda (key)
                     (member key '(name version provided-releases-count)
                             :test 'string=))
                   fields
                   :key #'first)))
