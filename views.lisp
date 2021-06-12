(defpackage #:quickdocs-api/views
  (:use #:cl)
  (:import-from #:mito
                #:dao-class)
  (:import-from #:com.inuoe.jzon
                #:coerced-fields
                #:coerce-element)
  (:import-from #:local-time
                #:timestamp
                #:format-timestring)
  (:export #:make-pagination))
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
