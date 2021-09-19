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

(defun search-projects-by-topic (queries dist)
  (mapcar (lambda (row)
            (apply #'mito:make-dao-instance (find-class 'release) row))
          (mito:retrieve-by-sql
            (select :release.*
              (from :release)
              (join :dist_release :on (:= :dist_release.release_id :release.id))
              (right-join :project_topic :on (:= :project_topic.project_name :release.name))
              (left-join :project_download_stats :on (:= :project_download_stats.project_name :release.name))
              (where `(:and (:= :dist_id ,(mito:object-id dist))
                            ,@(mapcar (lambda (query)
                                        `(:similar-to :topic
                                                      ,(format nil "%[[:<:]]~A%"
                                                               (escape-sql-similar-to-meta-char query))))
                                      queries)))
              (order-by (:desc :download_count :nulls :last)
                        (:desc :release.dist_version))
              (group-by :release.id :download_count)))))

(defun search-projects-by-name (query dist)
  (mito:select-dao 'release
    (join :dist_release :on (:= :dist_release.release_id :release.id))
    (left-join :project_download_stats :on (:= :project_download_stats.project_name :release.name))
    (where (:and (:= :dist_id (mito:object-id dist))
                 (:similar-to :name
                              (format nil "%[[:<:]]~(~A~)%"
                                      (escape-sql-similar-to-meta-char query)))))
    (order-by (:desc :download_count :nulls :last)
              (:desc :release.dist_version))))

(defun search-projects-by-description (queries dist)
  (mito:select-dao 'release
    (join :dist_release :on (:= :dist_release.release_id :release.id))
    (left-join :project_download_stats :on (:= :project_download_stats.project_name :release.name))
    (left-join :system :on (:= :system.name :release.name))
    (where `(:and (:= :dist_id ,(mito:object-id dist))
                  ,@(mapcar (lambda (query)
                              `(:similar-to (:lower :system.description)
                                            ,(format nil "%[[:<:]]~A%"
                                                     (escape-sql-similar-to-meta-char query))))
                       queries)))
    (order-by (:desc :download_count :nulls :last)
              (:desc :release.dist_version))))

(defun normalize-query (q)
  (cond
    ((member q '("regex" "regexp") :test 'equal)
     "regular expression")
    ((member q '("aws") :test 'equal)
     "amazon web services")
    ((member q '("db") :test 'equal)
     "database")
    (t (string-downcase q))))

(defun space-char-p (char)
  (and (member char '(#\Space #\Tab #\Newline #\Return))
       t))

(defun parse-query (q &key (start 0) (end (length q)))
  (let ((split-pos
          (if (char= (aref q start) #\")
              (position #\" q :start (min (1+ start) end) :end end)
              (position-if #'space-char-p
                           q
                           :start start
                           :end end))))
    (if (eql split-pos start)
        (let ((next-start
                (position-if-not #'space-char-p q :start start :end end)))
          (if next-start
              (parse-query q
                           :start next-start
                           :end end)
              nil))
        (cons
          (subseq q
                  (if (char= (aref q start) #\")
                      (1+ start)
                      start)
                  split-pos)
          (when (and split-pos
                     (< split-pos end))
            (let ((next-split-pos
                    (position-if-not #'space-char-p
                                     q
                                     :start (1+ split-pos)
                                     :end end)))
              (when next-split-pos
                (parse-query q :start next-split-pos :end end))))))))

(defun search (params)
  (let ((q (aget params "q")))
    (when (or (not (stringp q))
              (= 0 (length q)))
      (return-from search (listing params)))

    (let ((queries (mapcar #'normalize-query (parse-query q)))
          (dist (find-latest-dist)))
      (unless dist
        (throw-code 404))
      (let ((releases
              (delete-duplicates
                (append
                  (when (null (rest queries))
                    (ensure-list (retrieve-exact-match-project (first queries) dist)))
                  (search-projects-by-topic queries dist)
                  (when (null (rest queries))
                    (search-projects-by-name (first queries) dist))
                  (search-projects-by-description queries dist))
                :key #'release-name
                :test #'string=
                :from-end t)))
        (render 'search
                :query q
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
