(uiop:define-package #:40ants-linter-docs/changelog
  (:use #:cl)
  (:import-from #:40ants-doc/changelog
                #:defchangelog))
(in-package #:40ants-linter-docs/changelog)


(defchangelog (:ignore-words ("SLY"
                              "ASDF"
                              "REPL"
                              "SBCL"
			      "1Gb"
                              "DYNAMIC_SPACE_SIZE"
                              "HTTP"))
  (0.7.0 2025-12-24
         "* A new feature was added to the imports linter. Now it will warn you if you forgot to include some files into the ASDF system definition. For now it works only for package-inferred ASDF systems.")
  (0.6.0 2025-05-11
         "* Now imports linter will ignore packages which are not correspond to ASDF system.")
  (0.5.0 2025-05-02
         "* Now imports linter recognizes :shadowing-import-from forms.")
  (0.4.0 2024-12-14
         "* Now `DYNAMIC_SPACE_SIZE` env variable can be used to control amount of memory available to SBCL, by default it is `1Gb`.")
  (0.3.0 2023-12-01
         "* Fixed issue when package was mentioned as unused whereas it was used inside a backquoted part of the macro.")
  (0.2.0 2023-11-19
         "* Now imports linter can warn on packages mentioned in defpackage but missing.")
  (0.1.0 2021-04-06
         "* Initial version."))
