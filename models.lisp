(uiop:define-package #:quickdocs-api/models
  (:use #:cl
        #:sxql)
  (:use-reexport #:dist-updater/models)
  (:import-from #:alexandria
                #:when-let
                #:starts-with-subseq)
  (:export #:release-systems
           #:release-primary-system
           #:release-description
           #:release-authors
           #:release-maintainers
           #:release-licenses
           #:release-depends-on
           #:release-required-by
           #:find-latest-dist))
(in-package #:quickdocs-api/models)

(defun release-systems (release)
  (mito:select-dao 'system
    (where (:= :release release))
    (order-by :name)))

(defun release-primary-system (release)
  (check-type release release)
  (mito:find-dao 'system
                 :release release
                 :is-primary t))

(defun release-description (release)
  (check-type release release)
  (getf (first
          (mito:retrieve-by-sql
            (select (:description)
              (from :system)
              (where (:and (:= :release_id (mito:object-id release))
                           (:= :is_primary "true")
                           (:not-null :description)))
              (limit 1))))
        :description))

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

(defun release-depends-on (release dist)
  (let* ((systems (release-systems release))
         (depends-system-names
           (mapcar (lambda (row)
                     (getf row :name))
                   (mito:retrieve-by-sql
                     (select (:system_dependency.name)
                       (from :system_dependency)
                       (join :system :on (:= :system.id :system_dependency.system_id))
                       (join :release :on (:= :release.id :system.release_id))
                       (where `(:and (:= :release.id ,(mito:object-id release))
                                     ,@(and systems
                                            `((:not-in :system_dependency.name ,(mapcar #'system-name systems))))))
                       (group-by :system_dependency.name))))))
    (when depends-system-names
      (mapcar (lambda (row)
                (getf row :name))
              (mito:retrieve-by-sql
                (select (:release.name)
                  (from :release)
                  (join :dist_release :on (:= :dist_release.release_id :release.id))
                  (join :system :on (:= :system.release_id :release.id))
                  (where (:and (:= :dist_release.dist_id (mito:object-id dist))
                               (:in :system.name depends-system-names)))
                  (group-by :release.name)
                  (order-by :release.name)))))))

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

(defun find-latest-dist ()
  (first (mito:select-dao 'dist
           (where (:= :name "quicklisp"))
           (order-by (:desc :version))
           (limit 1))))
