(defpackage #:quickdocs-api/views
  (:use #:cl)
  (:import-from #:utopian/views
                #:utopian-view
                #:utopian-view-class
                #:utopian-view-direct-superclasses)
  (:import-from #:mito
                #:dao-class)
  (:import-from #:com.inuoe.jzon
                #:coerced-fields
                #:coerce-element
                #:stringify)
  (:import-from #:local-time
                #:timestamp
                #:format-timestring)
  (:import-from #:kebab
                #:to-snake-case)
  (:export #:make-pagination
           #:jzon-view-class))
(in-package #:quickdocs-api/views)

(defmethod coerced-fields :around ((object mito:dao-class))
  (let ((fields (call-next-method)))
    (remove 'synced fields
            :key #'first
            :test 'string-equal)))

(defmethod coerce-element ((timestamp local-time:timestamp) coerce-key)
  (declare (ignore coerce-key))
  (format-timestring nil timestamp
                     :format local-time:+iso-8601-format+
                     :timezone local-time:+gmt-zone+))

;; XXX: Not to convert as alist/plist.
;;   Because jzon takes a list of objects as a plist, and tries an object to convert with coerce-key
(defmethod coerce-element ((element cons) coerce-key)
  (coerce element 'vector))

(defstruct pagination
  (per-page nil :type integer)
  (page nil :type integer)
  (count nil :type integer)
  (items '() :type list))

(defun render-json (object stream)
  (stringify object
             :stream stream
             :coerce-key #'to-snake-case))

(defclass jzon-view (utopian-view) ())
(defclass jzon-view-class (utopian-view-class)
  ()
  (:default-initargs
   :content-type "application/json"
   :render-element 'render-json))

(defmethod utopian-view-direct-superclasses ((class jzon-view-class))
  '(jzon-view))
