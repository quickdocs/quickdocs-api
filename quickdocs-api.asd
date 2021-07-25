(defsystem "quickdocs-api"
  :class :package-inferred-system
  :author "Eitaro Fukamachi"
  :license "BSD 3-Clause"
  :version "0.1.2"
  :description "Quickdocs.org API server"
  :depends-on ("quickdocs-api/main"
               "clack"))

(register-system-packages "lack-component" '(#:lack.component))
