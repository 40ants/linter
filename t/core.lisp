(uiop:define-package #:40ants-linter-tests/core
  (:use #:cl)
  (:import-from #:rove
                #:deftest
                #:ok
                #:testing)
  (:import-from #:40ants-linter/imports
                #:used-symbols))
(in-package #:40ants-linter-tests/core)


(deftest test-symbol-forms-extractor ()
  (let ((symbols (used-symbols '`,bar)))
    (ok (member 'bar symbols))))
