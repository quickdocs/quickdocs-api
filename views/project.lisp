(defpackage #:quickdocs-api/views/project
  (:use #:cl)
  (:shadow #:search)
  (:import-from #:quickdocs-api/views/release
                #:make-release-for-listing-json)
  (:import-from #:quickdocs-api/models
                #:release-name
                #:release-description
                #:release-dist-version
                #:release-authors)
  (:import-from #:utopian/views
                #:defview)
  (:import-from #:quickdocs-api/views
                #:jzon-view-class)
  (:export #:search))
(in-package #:quickdocs-api/views/project)

(defstruct search-results
  (query nil :type (or string null))
  (items nil :type list))

(defview search ()
  (releases query)
  (:metaclass jzon-view-class)
  (:render
   (make-search-results
     :query query
     :items (mapcar (lambda (release)
                      (make-release-for-listing-json
                        :name (release-name release)
                        :description (release-description release)
                        :dist-version (release-dist-version release)
                        :authors (release-authors release)))
                    releases))))
