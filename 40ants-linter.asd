#-asdf3.1 (error "40ants-linter requires ASDF 3.1 because for lower versions pathname does not work for package-inferred systems.")
(defsystem "40ants-linter"
  :description "A custom wrapper to run SBLINT and also to validate package imports."
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :homepage "https://40ants.com/linter/"
  :source-control (:git "https://github.com/40ants/linter")
  :bug-tracker "https://github.com/40ants/linter/issues"
  :class :40ants-asdf-system
  :defsystem-depends-on ("40ants-asdf-system")
  :pathname "src"
  :depends-on ("40ants-linter/main"
               "40ants-linter/imports")
  :in-order-to ((test-op (test-op "40ants-linter-tests"))))
