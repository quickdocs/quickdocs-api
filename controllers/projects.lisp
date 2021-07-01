(defpackage #:quickdocs-api/controllers/projects
  (:use #:cl
        #:utopian
        #:sxql)
  (:shadowing-import-from #:quickdocs-api/views/project
                          #:search
                          #:project-badge)
  (:import-from #:quickdocs-api/views/release
                #:show)
  (:import-from #:quickdocs-api/models
                #:find-latest-dist
                #:release
                #:release-name
                #:release-dist-version)
  (:import-from #:cl-ppcre)
  (:import-from #:alexandria
                #:ensure-list)
  (:import-from #:assoc-utils
                #:aget)
  (:export #:listing
           #:search
           #:badge))
(in-package #:quickdocs-api/controllers/projects)

(defun listing (params)
  (declare (ignore params))
  (let ((dist (find-latest-dist)))
    (unless dist
      (throw-code 404))
    (let ((releases
            (mito:select-dao 'release
              (join :dist_release :on (:= :dist_release.release_id :release.id))
              (join :project_download_stats :on (:= :project_download_stats.project_name :release.name))
              (where (:= :dist_id (mito:object-id dist)))
              (order-by (:desc :download_count))
              (limit 200))))
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
    (ppcre:regex-replace-all "([%_|*+?{}\\(\\)\\[\\]'])" string #'match-part :simple-calls t)))

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
                           (:similar-to :topic
                                        (format nil "%[[:<:]]~A%"
                                                (escape-sql-similar-to-meta-char query)))))
              (order-by (:desc :download_count :nulls :last)
                        (:desc :release.dist_version))
              (group-by :release.id :download_count)))))

(defun search-projects-by-name (query dist)
  (mito:select-dao 'release
    (join :dist_release :on (:= :dist_release.release_id :release.id))
    (left-join :project_download_stats :on (:= :project_download_stats.project_name :release.name))
    (where (:and (:= :dist_id (mito:object-id dist))
                 (:similar-to :name
                              (format nil "%[[:<:]]~A%"
                                      (escape-sql-similar-to-meta-char (string-downcase query))))))
    (order-by (:desc :download_count :nulls :last)
              (:desc :release.dist_version))))

(defun search-projects-by-description (query dist)
  (mito:select-dao 'release
    (join :dist_release :on (:= :dist_release.release_id :release.id))
    (left-join :project_download_stats :on (:= :project_download_stats.project_name :release.name))
    (left-join :system :on (:= :system.name :release.name))
    (where (:and (:= :dist_id (mito:object-id dist))
                 (:similar-to :system.description
                              (format nil "%[[:<:]]~A%"
                                      (escape-sql-similar-to-meta-char (string-downcase query))))))
    (order-by (:desc :download_count :nulls :last)
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

(defun show (params)
  (let* ((name (aget params :name))
         (dist (find-latest-dist)))
    (unless dist
      (throw-code 404))
    (let ((release (retrieve-exact-match-project name dist)))
      (unless release
        (throw-code 404))
      (render 'show
              :dist dist
              :release release))))

(defun badge (params)
  (let* ((name (aget params :name))
         (release
           (first
             (mito:select-dao 'release
               (where (:and (:= :dist_name "quicklisp")
                            (:= :name name)))
               (order-by (:desc :dist-version))
               (limit 1)))))
    (render 'project-badge
            :dist-name "quicklisp"
            :dist-version (if release
                              (release-dist-version release)
                              nil))))
