#-asdf3.1 (error "40ants-linter requires ASDF 3.1")
(defsystem 40ants-linter
  :author "Alexander Artemenko"
  :license "BSD"
  :class :package-inferred-system
  :pathname "src"
  :depends-on ("40ants-linter/main"
               "40ants-linter/imports")
  :description "A custom wrapper to run SBLINT and specify ASDF systems instead of files")
