(defpackage #:quickdocs-api/views/release
  (:use #:cl)
  (:import-from #:utopian/views
                #:defview)
  (:import-from #:quickdocs-api/views
                #:jzon-view-class
                #:make-pagination)
  (:import-from #:quickdocs-api/models
                #:release-dist-version
                #:release-name
                #:release-description
                #:release-authors
                #:release-maintainers
                #:release-upstream-url
                #:release-licenses
                #:release-depends-on
                #:release-required-by
                #:release-readme-files
                #:readme-file-filename
                #:readme-file-content)
  (:import-from #:alexandria
                #:when-let)
  (:export #:listing
           #:show
           #:release-versions
           #:make-release-for-listing-json))
(in-package #:quickdocs-api/views/release)

(defstruct readme-json
  (filename nil :type string)
  (content nil :type string))

(defstruct release-json
  (name nil :type string)
  (description nil :type t)
  (dist-version nil :type string)
  (authors nil :type list)
  (maintainers nil :type list)
  (upstream-url nil :type t)
  (licenses nil :type list)
  (depends-on nil :type list)
  (required-by nil :type list)
  (readme nil :type (or readme-json null)))

(defstruct release-for-listing-json
  (name nil :type string)
  (description nil :type t)
  (dist-version nil :type string)
  (upstream-url nil :type (or string null))
  (authors nil :type list)
  (licenses nil :type list))

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
                        :dist-version (release-dist-version release)
                        :upstream-url (release-upstream-url release)
                        :authors (release-authors release)
                        :licenses (release-licenses release)))
                    releases))))

(defview show ()
  (release dist)
  (:metaclass jzon-view-class)
  (:render
   (make-release-json
     :name (release-name release)
     :description (release-description release)
     :dist-version (release-dist-version release)
     :authors (release-authors release)
     :maintainers (release-maintainers release)
     :upstream-url (release-upstream-url release)
     :licenses (release-licenses release)
     :depends-on (release-depends-on release dist)
     :required-by (release-required-by release dist)
     :readme (when-let (readme-file (first (release-readme-files release)))
               (make-readme-json
                 :filename (readme-file-filename readme-file)
                 :content (readme-file-content readme-file))))))

(defview release-versions ()
  (versions per-page page count)
  (:metaclass jzon-view-class)
  (:render
   (make-pagination
     :per-page per-page
     :page page
     :count count
     :items versions)))
