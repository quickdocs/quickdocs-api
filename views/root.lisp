(defpackage #:quickdocs-api/views/root
  (:use #:cl)
  (:import-from #:utopian/views
                #:defview)
  (:import-from #:quickdocs-api/views
                #:jzon-view-class)
  (:export #:ping))
(in-package #:quickdocs-api/views/root)

(defview ping ()
  ()
  (:metaclass jzon-view-class)
  (:render
   '(("pong" . "ok"))))
