(defpackage #:quickdocs-api/views/root
  (:use #:cl
        #:lsx
        #:utopian)
  (:export #:index-page))
(in-package #:quickdocs-api/views/root)

(named-readtables:in-readtable :lsx-syntax)

(defview index-page ()
  ()
  (:render
   <html>
     <head>
       <title>index | quickdocs-api</title>
     </head>
     <body>
       <h1>index</h1>
     </body>
   </html>))
