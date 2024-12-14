(uiop:define-package #:40ants-linter-docs/changelog
  (:use #:cl)
  (:import-from #:40ants-doc/changelog
                #:defchangelog))
(in-package #:40ants-linter-docs/changelog)


(defchangelog (:ignore-words ("SLY"
                              "ASDF"
                              "REPL"
                              "SBCL"
                              "DYNAMIC_SPACE_SIZE"
                              "HTTP"))
  (0.4.0 2024-12-14
         "* Now DYNAMIC_SPACE_SIZE env variable can be used to control amount of memory available to SBCL, by default it is 1Gb.")
  (0.3.0 2023-12-01
         "* Fixed issue when package was mentioned as unused whereas it was used inside a backquoted part of the macro.")
  (0.2.0 2023-11-19
         "* Now imports linter can warn on packages mentioned in defpackage but missing.")
  (0.1.0 2021-04-06
         "* Initial version."))
