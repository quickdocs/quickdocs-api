(defpackage #:quickdocs-api/views/system
  (:use #:cl)
  (:import-from #:utopian/views
                #:defview)
  (:import-from #:quickdocs-api/views
                #:jzon-view-class)
  (:import-from #:quickdocs-api/models
                #:release-name
                #:release-description
                #:release-dist-version

                #:system-name
                #:system-primary-p
                #:system-filename
                #:system-long-name
                #:system-version
                #:system-description
                #:system-long-description
                #:system-authors
                #:system-maintainers
                #:system-mailto
                #:system-license
                #:system-homepage
                #:system-bug-tracker
                #:system-source-control-url
                #:system-dependencies

                #:system-dependency-type
                #:system-dependency-name
                #:system-dependency-version
                #:system-dependency-feature)
  (:export #:release-systems))
(in-package #:quickdocs-api/views/system)

(defstruct dependency-json
  (name nil :type string)
  (version nil :type (or string null))
  (feature nil :type (or string null)))

(defstruct system-json
  (name nil :type string)
  (is-primary nil :type boolean)
  (filename nil :type (or string null))
  (long-name nil :type (or string null))
  (version nil :type (or string null))
  (description nil :type (or string null))
  (long-description nil :type (or string null))
  (authors nil :type sequence)
  (maintainers nil :type sequence)
  (mailto nil :type (or string null))
  (license nil :type (or string null))
  (homepage nil :type (or string null))
  (bug-tracker nil :type (or string null))
  (source-control-url nil :type (or string null))
  (depends-on nil :type sequence)
  (weakly-depends-on nil :type sequence)
  (defsystem-depends-on nil :type sequence))

(defun filter-dependencies (dependencies type)
  (remove-if-not
    (lambda (dep)
      (equal (system-dependency-type dep) type))
    dependencies))

(defview release-systems ()
  (release systems)
  (:metaclass jzon-view-class)
  (:render
   `(("name" . ,(release-name release))
     ("description" . ,(release-description release))
     ("dist_version" . ,(release-dist-version release))
     ("systems" . ,(mapcar (lambda (system)
                             (flet ((dependency-to-json (dependency)
                                      (make-dependency-json
                                        :name (system-dependency-name dependency)
                                        :version (system-dependency-version dependency)
                                        :feature (system-dependency-feature dependency))))
                               (let ((dependencies (system-dependencies system)))
                                 (make-system-json
                                   :name (system-name system)
                                   :is-primary (system-primary-p system)
                                   :filename (system-filename system)
                                   :long-name (system-long-name system)
                                   :version (system-version system)
                                   :description (system-description system)
                                   :long-description (system-long-description system)
                                   :authors (system-authors system)
                                   :maintainers (system-maintainers system)
                                   :mailto (system-mailto system)
                                   :license (system-license system)
                                   :homepage (system-homepage system)
                                   :bug-tracker (system-bug-tracker system)
                                   :source-control-url (system-source-control-url system)
                                   :defsystem-depends-on (map 'vector
                                                              #'dependency-to-json
                                                              (filter-dependencies dependencies "defsystem"))
                                   :depends-on (map 'vector
                                                    #'dependency-to-json
                                                    (filter-dependencies dependencies "normal"))
                                   :weakly-depends-on (map 'vector
                                                           #'dependency-to-json
                                                           (filter-dependencies dependencies "weakly"))))))
                           systems)))))
