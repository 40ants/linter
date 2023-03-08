(defsystem "40ants-linter-ci"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :homepage "https://40ants.com/linter/"
  :class :package-inferred-system
  :description "Provides CI settings for 40ants-linter."
  :source-control (:git "https://github.com/40ants/linter")
  :bug-tracker "https://github.com/40ants/linter/issues"
  :pathname "src"
  :depends-on ("40ants-linter-ci/ci"))
