(defpackage #:quickdocs-api/views/release
  (:use #:cl)
  (:import-from #:quickdocs-api/views)
  (:import-from #:quickdocs-api/models
                #:release
                #:release-name
                #:release-description
                #:release-authors
                #:release-maintainers
                #:release-upstream-url
                #:release-licenses
                #:release-dependencies)
  (:import-from #:com.inuoe.jzon
                #:coerced-fields))
(in-package #:quickdocs-api/views/release)

(defmethod coerced-fields :around ((release release))
  (list `(name ,(release-name release) string)
        `(description ,(release-description release) t)
        `(authors ,(release-authors release) list)
        `(maintainers ,(release-maintainers release) list)
        `(upstream-url ,(release-upstream-url release) t)
        `(licenses ,(release-licenses release) list)
        `(required-releases ,(mapcar #'release-name
                                     (release-dependencies release))
                            list)))
