(defsystem "40ants-linter-tests"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :homepage "https://40ants.com/linter/"
  :class :package-inferred-system
  :description "Provides tests for 40ants-linter."
  :source-control (:git "https://github.com/40ants/linter")
  :bug-tracker "https://github.com/40ants/linter/issues"
  :pathname "t"
  :depends-on ("40ants-linter-tests/core")
  :perform (test-op :after (op c)
                    (symbol-call :rove :run c))  )
