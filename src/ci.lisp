(uiop:define-package #:40ants-linter-ci/ci
  (:use #:cl)
  (:import-from #:40ants-ci/jobs/linter)
  (:import-from #:40ants-ci/jobs/run-tests
                #:run-tests)
  (:import-from #:40ants-ci/jobs/docs
                #:build-docs)
  (:import-from #:40ants-ci/workflow
                #:defworkflow))
(in-package #:40ants-linter-ci/ci)


(defworkflow linter
  :on-push-to "master"
  :by-cron "0 10 * * 1"
  :on-pull-request t
  :cache t
  :jobs ((40ants-ci/jobs/linter:linter
          ;; Until the issue with
          ;; Bug in readtable iterators or concurrent access?
          ;; will be resolved in the named-readtables.
          :lisp "sbcl-bin/2.5.10"
          :asdf-systems ("40ants-linter"
                         "40ants-linter-docs"
                         "40ants-linter-tests"))))

(defworkflow docs
  :on-push-to "master"
  :by-cron "0 10 * * 1"
  :on-pull-request t
  :cache t
  :jobs ((build-docs
          ;; Until the issue with
          ;; Bug in readtable iterators or concurrent access?
          ;; will be resolved in the named-readtables.
          :lisp "sbcl-bin/2.5.10"
          :asdf-system "40ants-linter-docs")))


(defworkflow ci
  :on-push-to "master"
  :by-cron "0 10 * * 1"
  :on-pull-request t
  :cache t
  :jobs ((run-tests
          ;; Until the issue with
          ;; Bug in readtable iterators or concurrent access?
          ;; will be resolved in the named-readtables.
          :lisp ("sbcl-bin/2.5.10"
                 "ccl-bin")
          :coverage t)))
