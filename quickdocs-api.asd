(defsystem "quickdocs-api"
  :class :package-inferred-system
  :author "Eitaro Fukamachi"
  :version "0.0.1"
  :description "Quickdocs.org API server"
  :depends-on ("quickdocs-api/main"))

(register-system-packages "lack-component" '(#:lack.component))
