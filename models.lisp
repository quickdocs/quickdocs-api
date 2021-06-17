(uiop:define-package #:quickdocs-api/models
  (:use #:cl
        #:sxql)
  (:use-reexport #:dist-updater/models)
  (:import-from #:alexandria
                #:when-let)
  (:export #:release-systems
           #:release-primary-system
           #:release-description
           #:release-authors
           #:release-maintainers
           #:release-licenses
           #:release-depends-on
           #:release-required-by))
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

(defun release-depends-on (release)
  (let ((systems (release-systems release)))
    (mapcar (lambda (row)
              (getf row :name))
            (mito:retrieve-by-sql
              (select (:system_dependency.name)
                (from :system_dependency)
                (left-join :system :on (:= :system.id :system_dependency.system_id))
                (left-join :release :on (:= :release.id :system.release_id))
                (where `(:and (:= :release.id ,(mito:object-id release))
                              ,@(and systems
                                     `((:not-in :system_dependency.name ,(mapcar #'system-name systems))))))
                (group-by :system_dependency.name)
                (order-by :system_dependency.name))))))

(defun release-required-by (release dist)
  (when-let ((systems (release-systems release)))
    (mapcar (lambda (row)
              (getf row :name))
            (mito:retrieve-by-sql
              (select (:release.name)
                (from :system_dependency)
                (left-join :system :on (:= :system.id :system_dependency.system_id))
                (left-join :release :on (:= :release.id :system.release_id))
                (left-join :dist_release :on (:= :dist_release.release_id :release.id))
                (where (:and (:= :dist_release.dist_id (mito:object-id dist))
                             (:in :system_dependency.name (mapcar #'system-name systems))
                             (:!= :release.name (release-name release))))
                (group-by :release.name)
                (order-by :release.name))))))
