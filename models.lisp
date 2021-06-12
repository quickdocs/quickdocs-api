(uiop:define-package #:quickdocs-api/models
  (:use #:cl
        #:sxql)
  (:use-reexport #:dist-updater/models)
  (:import-from #:dist-updater/models
                #:system-authors
                #:system-maintainers
                #:system-dependency
                #:system-dependency-name)
  (:import-from #:alexandria
                #:when-let)
  (:export #:release-systems
           #:release-primary-system
           #:release-description
           #:release-authors
           #:release-maintainers
           #:release-licenses
           #:release-dependencies))
(in-package #:quickdocs-api/models)

(defun release-systems (release)
  (mito:select-dao 'system
    (where (:= :release release))
    (order-by :name)))

(defun release-primary-system (release)
  (check-type release release)
  (mito:find-dao 'system
                 :release release
                 :name (release-name release)))

(defun release-description (release)
  (check-type release release)
  (when-let ((system (release-primary-system release)))
    (system-description system)))

(defun release-authors (release)
  (check-type release release)
  (let ((systems (release-systems release)))
    (delete-duplicates
      (loop for system in systems
            append (coerce (system-authors system) 'list))
      :test #'string=
      :from-end t)))

(defun release-maintainers (release)
  (check-type release release)
  (let ((systems (release-systems release)))
    (delete-duplicates
      (loop for system in systems
            append (coerce (system-maintainers system) 'list))
      :test #'string=
      :from-end t)))

(defun release-licenses (release)
  (check-type release release)
  (let ((systems (release-systems release)))
    (delete-duplicates
      (loop for system in systems
            if (system-license system)
            collect (system-license system))
      :test #'string=
      :from-end t)))

(defun release-dependencies (release)
  (let* ((systems (release-systems release))
         (required-system-names
           (and systems
                (mapcar #'system-dependency-name
                        (mito:select-dao 'system-dependency
                                         (where (:in :system systems)))))))
    (when required-system-names
      (delete-duplicates
        (mito:select-dao 'release
          (left-join 'system :on (:= :release.id :system.release_id))
          (where (:and (:= :dist (release-dist release))
                       (:in :system.name required-system-names))))
        :key #'release-name
        :test 'string=
        :from-end t))))
