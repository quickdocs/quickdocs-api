(defpackage #:quickdocs-api/views/release
  (:use #:cl)
  (:import-from #:utopian/views
                #:defview)
  (:import-from #:quickdocs-api/views
                #:jzon-view-class
                #:make-pagination)
  (:import-from #:quickdocs-api/models
                #:release-name
                #:release-description
                #:release-authors
                #:release-maintainers
                #:release-upstream-url
                #:release-licenses
                #:release-depends-on
                #:release-required-by)
  (:export #:listing
           #:show))
(in-package #:quickdocs-api/views/release)

(defstruct release-json
  (name nil :type string)
  (description nil :type t)
  (authors nil :type list)
  (maintainers nil :type list)
  (upstream-url nil :type t)
  (licenses nil :type list)
  (depends-on nil :type list)
  (required-by nil :type list))

(defstruct release-for-listing-json
  (name nil :type string)
  (description nil :type t)
  (authors nil :type list))

(defview listing ()
  (releases per-page page count)
  (:metaclass jzon-view-class)
  (:render
   (make-pagination
     :per-page per-page
     :page page
     :count count
     :items (mapcar (lambda (release)
                      (make-release-for-listing-json
                        :name (release-name release)
                        :description (release-description release)
                        :authors (release-authors release)))
                    releases))))

(defview show ()
  (release)
  (:metaclass jzon-view-class)
  (:render
   (make-release-json
     :name (release-name release)
     :description (release-description release)
     :authors (release-authors release)
     :maintainers (release-maintainers release)
     :upstream-url (release-upstream-url release)
     :licenses (release-licenses release)
     :depends-on (release-depends-on release)
     :required-by (release-required-by release))))
