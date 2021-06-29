(defpackage #:quickdocs-api/views/project
  (:use #:cl)
  (:shadow #:search)
  (:import-from #:quickdocs-api/views/release
                #:make-release-for-listing-json)
  (:import-from #:quickdocs-api/models
                #:release-name
                #:release-description
                #:release-dist-version
                #:release-upstream-url
                #:release-authors
                #:release-licenses)
  (:import-from #:utopian/views
                #:defview)
  (:import-from #:quickdocs-api/views
                #:jzon-view-class)
  (:export #:search
           #:project-badge))
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
                        :upstream-url (release-upstream-url release)
                        :authors (release-authors release)
                        :licenses (release-licenses release)))
                    releases))))

(defview project-badge ()
  ((dist-name :initform "quicklisp")
   (dist-version :initform nil))
  (:render
   (if dist-version
       (format nil
               "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"137\" height=\"20\"><linearGradient id=\"b\" x2=\"0\" y2=\"100%\"><stop offset=\"0\" stop-color=\"#bbb\" stop-opacity=\".1\"/><stop offset=\"1\" stop-opacity=\".1\"/></linearGradient><mask id=\"a\"><rect width=\"137\" height=\"20\" rx=\"3\" fill=\"#fff\"/></mask><g mask=\"url(#a)\"><path fill=\"#555\" d=\"M0 0h61v20H0z\"/><path fill=\"#007ec6\" d=\"M61 0h76v20H61z\"/><path fill=\"url(#b)\" d=\"M0 0h137v20H0z\"/></g><g fill=\"#fff\" text-anchor=\"middle\" font-family=\"DejaVu Sans,Verdana,Geneva,sans-serif\" font-size=\"11\"><text x=\"30.5\" y=\"15\" fill=\"#010101\" fill-opacity=\".3\">~@(~A~)</text><text x=\"30.5\" y=\"14\">~:*~@(~A~)</text><text x=\"98\" y=\"15\" fill=\"#010101\" fill-opacity=\".3\">~A</text><text x=\"98\" y=\"14\">~:*~A</text></g></svg>"
               dist-name
               dist-version)
       (format nil
               "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"144\" height=\"20\"><linearGradient id=\"b\" x2=\"0\" y2=\"100%\"><stop offset=\"0\" stop-color=\"#bbb\" stop-opacity=\".1\"/><stop offset=\"1\" stop-opacity=\".1\"/></linearGradient><mask id=\"a\"><rect width=\"144\" height=\"20\" rx=\"3\" fill=\"#fff\"/></mask><g mask=\"url(#a)\"><path fill=\"#555\" d=\"M0 0h61v20H0z\"/><path fill=\"#9f9f9f\" d=\"M61 0h83v20H61z\"/><path fill=\"url(#b)\" d=\"M0 0h144v20H0z\"/></g><g fill=\"#fff\" text-anchor=\"middle\" font-family=\"DejaVu Sans,Verdana,Geneva,sans-serif\" font-size=\"11\"><text x=\"30.5\" y=\"15\" fill=\"#010101\" fill-opacity=\".3\">~@(~A~)</text><text x=\"30.5\" y=\"14\">~:*~@(~A~)</text><text x=\"101.5\" y=\"15\" fill=\"#010101\" fill-opacity=\".3\">not available</text><text x=\"101.5\" y=\"14\">not available</text></g></svg>"
               dist-name)))
  (:content-type "image/svg+xml"))
