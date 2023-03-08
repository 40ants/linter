(defpackage #:40ants-linter/main
  (:use #:cl)
  (:import-from #:str)
  (:import-from #:cl-ppcre)
  (:import-from #:defmain
                #:defmain)
  (:import-from #:sblint/utilities/quicklisp)
  (:import-from #:sblint/run-lint)
  (:import-from #:sblint/utilities/pathname)
  (:import-from #:sblint/utilities/logger)
  (:import-from #:sblint/utilities/streams)
  (:export #:main))
(in-package 40ants-linter/main)


(defun should-be-ignored (condition)
  ;; DEFUN redefinitions occur when DEFUN form in enclosed into EVAL-WHEN form
  ;; DEFMACRO seems issues warning every time. Probably because after redifinition
  ;; you need to recompile all places where this macro was used:
  (cl-ppcre:all-matches "^redefining .* in (DEFUN|DEFMACRO)$"
                        (princ-to-string condition)))


(defun run-lint-system (system-name &optional (stream *standard-output*))
  (let* ((system (asdf:find-system system-name))
         ;; Here we'll count errors.
         ;; We need it because run-lit-fn may signal error
         ;; and without error-map we'll never know how many
         ;; errors were there.
         (error-map (sblint/run-lint::make-error-map))
         (num-ignored 0))

    #+quicklisp
    (sblint/utilities/quicklisp::install-required-systems system-name)
    (sblint/run-lint::ensure-dependencies-are-loaded system)

    (let ((directory (asdf:system-relative-pathname system ""))
          (errout *error-output*))
      (labels ((ignore-and-continue (e)
                 (let ((accept (find-restart 'asdf/action:accept e)))
                   (when accept
                     (invoke-restart accept)))
                 (let ((continue (find-restart 'continue e)))
                   (when continue
                     (invoke-restart continue))))
               (handle-compile-error (e)
                 (let ((*error-output* errout))
                   (if (remove-if-not (lambda (comp)
                                        (sblint/utilities/pathname::file-in-directory-p
                                         (asdf:component-pathname comp)
                                         directory))
                                      (mapcar #'cdr
                                              (uiop/lisp-build::compile-condition-context-arguments e)))
                       (warn "Compilation failed in a system ~S."
                             (asdf:component-name system))
                       (ignore-and-continue e))))
               (handle-compile-file-error (e)
                 (handle-compile-error e)
                 (return-from run-lint-system
                   (sblint/run-lint::num-errors error-map))))
        (sblint/run-lint::run-lint-fn
         (lambda ()
           (sblint/utilities/logger::do-log :info
             "Loading a system: ~A" (asdf:component-name system))
           (handler-bind ((asdf:compile-error #'handle-compile-error)
                          #+asdf3
                          (uiop:compile-file-error #'handle-compile-file-error)
                          #+sbcl
                          (sb-int:package-at-variance #'ignore-and-continue))
             
             (let ((already-reloaded (make-hash-table :test 'equal)))
               (flet ((from-the-same-primary-system-p (component)
                        "This predicate makes ASDF correctly reload all package-inferred subsystems."
                        (let ((should-be-reloaded-p
                                (and (string-equal (asdf:primary-system-name component)
                                                   (asdf:primary-system-name system))
                                     (null (gethash component
                                                    already-reloaded)))))
                          (when should-be-reloaded-p
                            (setf (gethash component
                                           already-reloaded)
                                  t)
                            
                            (sblint/utilities/logger::do-log :info
                              "System \"~A\" will be reloaded." component))
                          (values should-be-reloaded-p)))
                      (maybe-ignore-condition (condition)
                        (let* ((*error-output* errout)
                               (sb-int:*print-condition-references* nil))
                          (multiple-value-bind (file position)
                              (sblint/run-lint::get-location condition)
                            (when (should-be-ignored condition)
                              ;; Here we just register that we've seen this condition
                              ;; and don't need to print a note for it again.
                              ;; This will suppress output from SBLINT's handler
                              (let ((key (list file position (princ-to-string condition))))
                                (unless (gethash key error-map)
                                  (setf (gethash key error-map) t)
                                  (incf num-ignored))))))))
                 (sblint/run-lint::call-with-handle-condition
                  #'maybe-ignore-condition
                  (lambda ()
                    (sblint/utilities/streams::with-muffled-streams
                      (asdf:load-system system :force #'from-the-same-primary-system-p)))))))
           (sblint/utilities/logger::do-log :info "Done"))
         stream
         *error-output*
         directory
         error-map)

        (- (sblint/run-lint::num-errors error-map)
           num-ignored)))))


(defun quit-unless-interactive (code)
  (let* ((symbol (uiop:find-symbol* "*CHANNEL*" "SLYNK-API"
                                    nil))
         (bound (when symbol
                  (boundp symbol))))
    (unless bound
      (format t "EXITING with code: ~A~%"
              code)
      ;; (uiop:quit code)
      )))


(defun real-main (version system)
  "This function is for convenients, because it can be called in the repl
   without quit."
  (when version
    (let* ((system (asdf:find-system :40ants-linter))
           (version (asdf:component-version system)))
      (format t "Version: ~A~%" version)
      (quit-unless-interactive 0)))
  
  (when system
    (let ((systems (mapcar #'str:trim
                           (str:split "," system
                                      :omit-nulls t))))

      (loop for system in systems
            for errors-count = (progn
                                 (format t "Linting system ~A...~%" system)
                                 (let ((errors-count (run-lint-system system)))
                                   (if (zerop errors-count)
                                       (format t "everything is OK!~%")
                                       (format t "~%"))
                                   errors-count))
            summing errors-count into total-errors-count
            finally (quit-unless-interactive total-errors-count)))))


(defmain (main) ((version "Show program version and exit."
                          :flag t)
                 (system "ASDF system (or multiple systems, separated by comma)."))
  "Show information about Lisp implementation and given systems. Useful when collecting information for bugreports."
  (real-main version system))
