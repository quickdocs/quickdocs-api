(defpackage #:quickdocs-api/controllers/projects
  (:use #:cl
        #:utopian
        #:sxql)
  (:shadowing-import-from #:quickdocs-api/views/project
                          #:search)
  (:import-from #:dist-updater/models
                #:dist
                #:release
                #:release-name)
  (:import-from #:cl-ppcre)
  (:import-from #:alexandria
                #:ensure-list)
  (:import-from #:assoc-utils
                #:aget)
  (:export #:listing
           #:search))
(in-package #:quickdocs-api/controllers/projects)

(defun find-latest-dist ()
  (first (mito:select-dao 'dist
           (where (:= :name "quicklisp"))
           (order-by (:desc :version))
           (limit 1))))

(defun listing (params)
  (declare (ignore params))
  (let ((dist (find-latest-dist)))
    (unless dist
      (throw-code 404))
    (let ((releases
            (mito:select-dao 'release
              (join :dist_release :on (:= :dist_release.release_id :release.id))
              (left-join :project_download_stats :on (:= :project_download_stats.project_name :release.name))
              (where (:= :dist_id (mito:object-id dist)))
              (order-by (:splicing-raw "download_count DESC NULLS LAST")))))
      (render 'search
              :query nil
              :releases releases))))

(defun retrieve-exact-match-project (name dist)
  (first
    (mito:select-dao 'release
      (join :dist_release :on (:= :dist_release.release_id :release.id))
      (where (:and (:= :dist_id (mito:object-id dist))
                   (:= :name name)))
      (limit 1))))

(defun escape-sql-similar-to-meta-char (string)
  (flet ((match-part (match first-register)
           (declare (ignore first-register))
           (format nil "\\~A" match)))
    (ppcre:regex-replace-all "([%_|*+?{}\\(\\)\\[\\]])" string #'match-part :simple-calls t)))

(defun search-projects-by-topic (query dist)
  (mapcar (lambda (row)
            (apply #'mito:make-dao-instance 'release row))
          (mito:retrieve-by-sql
            (select :release.*
              (from :release)
              (join :dist_release :on (:= :dist_release.release_id :release.id))
              (right-join :project_topic :on (:= :project_topic.project_name :release.name))
              (left-join :project_download_stats :on (:= :project_download_stats.project_name :release.name))
              (where (:and (:= :dist_id (mito:object-id dist))
                           (:raw (format nil "topic SIMILAR TO '%[[:<:]]~A%'"
                                         (escape-sql-similar-to-meta-char query)))))
              (order-by (:splicing-raw "download_count DESC NULLS LAST")
                        (:desc :release.dist_version))
              (group-by :release.id :download_count)))))

(defun search-projects-by-name (query dist)
  (mito:select-dao 'release
    (join :dist_release :on (:= :dist_release.release_id :release.id))
    (left-join :project_download_stats :on (:= :project_download_stats.project_name :release.name))
    (where (:and (:= :dist_id (mito:object-id dist))
                 (:raw (format nil "name SIMILAR TO '%[[:<:]]~A%'"
                               (escape-sql-similar-to-meta-char query)))))
    (order-by (:splicing-raw "download_count DESC NULLS LAST")
              (:desc :release.dist_version))))

(defun search-projects-by-description (query dist)
  (mito:select-dao 'release
    (join :dist_release :on (:= :dist_release.release_id :release.id))
    (left-join :project_download_stats :on (:= :project_download_stats.project_name :release.name))
    (left-join :system :on (:= :system.name :release.name))
    (where (:and (:= :dist_id (mito:object-id dist))
                 (:raw (format nil "system.description SIMILAR TO '%[[:<:]]~A%'"
                               (escape-sql-similar-to-meta-char query)))))
    (order-by (:splicing-raw "download_count DESC NULLS LAST")
              (:desc :release.dist_version))))

(defun search (params)
  (let ((query (aget params "q")))
    (when (or (null query)
              (= 0 (length query)))
      (return-from search (listing params)))

    (let ((dist (find-latest-dist)))
      (unless dist
        (throw-code 404))
      (let ((releases
              (delete-duplicates
                (append
                  (ensure-list (retrieve-exact-match-project query dist))
                  (search-projects-by-topic query dist)
                  (search-projects-by-name query dist)
                  (search-projects-by-description query dist))
                :key #'release-name
                :test #'string=
                :from-end t)))
        (render 'search
                :query query
                :releases releases)))))
