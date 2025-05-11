(uiop:define-package #:40ants-linter/imports
  (:use #:cl)
  (:import-from #:named-readtables)
  (:import-from #:alexandria
                #:curry
                #:with-input-from-file)
  (:export
   #:analyze-imports))
(in-package 40ants-linter/imports)


(defun system-files (system-name &optional (visited (make-hash-table :test 'equal)))
  (let* ((system (asdf:find-system system-name))
         (primary-name (asdf:primary-system-name system)))
    (unless system
      (error "Unable to find system \"~A\""
             system-name))
    (setf (gethash system-name visited)
          t)
    
    (let ((files
            (loop for child in (asdf:component-children system)
                  when (typep child
                              'asdf/lisp-action:cl-source-file)
                    collect (asdf:component-pathname child)))
          (deps (asdf:system-depends-on system)))
      (loop for dep in deps
            when (string= (asdf:primary-system-name dep)
                          primary-name)
              do (setf files
                       (union files
                              (unless (gethash dep visited)
                                (system-files dep visited))
                              :test #'equal)))
      
      (values (sort files
                    #'string<
                    :key #'namestring)))))


(defun package-definition-p (form)
  (or (eql (car form)
           'uiop/package:define-package)
      (eql (car form)
        'defpackage)))


(defun imported-packages (form)
  "Returns a package list of all packages mentioned in :use or :import-from sections of the FORM."
  (loop with result = nil
        for (option-name . option-value) in (cddr form)
        when (eql option-name
                  :use)
          do (setf result
                   (nunion result
                           (mapcar #'symbol-name option-value)
                           :test #'string-equal))
        when (member option-name
                     '(:import-from
                       :shadowing-import-from))
          do (pushnew (symbol-name (car option-value))
                      result
                      :test #'string-equal)
        finally (return (let ((sorted-results
                                (sort result
                                      #'string<))
                              (current-package
                                (find-package (second form))))
                          (loop for package-name in sorted-results
                                for package = (find-package package-name)
                                if package
                                collect package into imported-packages
                                else collect package-name into not-found-packages
                                finally (return
                                          (values
                                           ;; The current package itself should be ignored
                                           ;; during the search of missing imports
                                           current-package
                                           ;; All packages, mentioned in :USE and :IMPORT-FROM
                                           imported-packages
                                           ;; These names are mentioned in :USE or :IMPORT-FROM
                                           ;; but there is no such packages. This sometimes
                                           ;; happens when you are trying to make a dependency
                                           ;; on some package inferred system by it's primary system
                                           ;; name, but it does not define such package.
                                           not-found-packages)))))))


(defun form-symbols (form)
  (typecase form
    (cons
     (append (form-symbols (car form))
             (form-symbols (cdr form))))
    (symbol
     (list form))
    #+sbcl
    (sb-impl::comma
     (alexandria:mappend
      #'form-symbols
      (uiop:ensure-list
       (slot-value form 'sb-impl::expr))))
    (t
     nil)))


(defun used-symbols (forms)
  (loop with result = nil
        for form in forms
        do (loop with keywords = (find-package "KEYWORD")
                 for symbol in (form-symbols form)
                 for package = (symbol-package symbol)
                 ;; There could be uninterned symbols
                 ;; we are not interested in them when searching
                 ;; for missing imports. As well as we don't
                 ;; interested in keywords:
                 unless (or (null package)
                            (eql package keywords))
                   do (pushnew symbol
                               result))
        finally (return (sort result
                              (lambda (left right)
                                (and (string< (package-name (symbol-package left))
                                              (package-name (symbol-package right)))
                                     (string< (symbol-name left)
                                              (symbol-name right))))))))


(defun read-forms (filename)
  (with-input-from-file (input filename)
    (let* ((temp-package (uiop:ensure-package "IMPORTS-LINTER-WORKSPACE"
                                              :use '("COMMON-LISP")))
           (*package* temp-package))
      (unwind-protect
           (loop with eof = '#:eof
                 with *readtable* = (copy-readtable nil)
                 for form = (read-preserving-whitespace input nil eof)
                 until (eq form eof)
                 when (or (eql (car form)
                               'cl:in-package)
                          (eql (car form)
                               'named-readtables:in-readtable))
                   do (eval form)
                 collect form)
        (delete-package temp-package)))))


(defparameter *packages-to-ignore*
  '("sb-ext"
    "sb-int"
    "sb-c"
    "sb-mop"
    "sb-pcl"
    "uiop"
    "asdf"
    "40ants-linter"))


(defun package-should-be-ignored (package)
  (let* ((name (asdf:primary-system-name
                (package-name package))))
    (or
     ;; If there is no ASDF system corresponding to
     ;; package name, then it should be ignored
     (null (asdf:registered-system name))
     ;; or package can be ignored by a blacklist:
     (member name *packages-to-ignore*
             :test #'string-equal))))


(defun search-in-package (symbol package)
  (loop for candidate being the symbol of package
          thereis (eql symbol
                       candidate)))


(defun search-in-packages (symbol imported-packages)
  (loop for package in imported-packages
        when (search-in-package symbol package)
          collect package))


(defun package-of-some-known-non-package-inferred-system-p (package)
  (let ((name (package-name package)))
    (and
     (not
      ;; When there is no real ASDF system behind the package
      (asdf:registered-system name))
     ;; but it was registered using asdf:register-system-packages function
     (asdf/package-inferred-system::package-name-system name)
     ;; then we'll require user to import exactly this name instead of
     ;; it's real system name. This way when checked system uses SWANK/BACKEND,
     ;; we'll require it to import-from SWANK/BACKEND instead of SWANK.
     t)))


(defun analyze-file-imports (checked-system-name filename system-dependencies)
  (let* ((all-forms (read-forms filename))
         (package-def (find-if #'package-definition-p
                               all-forms)))
    (multiple-value-bind (current-package imported not-found-packages)
        (imported-packages package-def)
      (loop with used-imports = nil
            with missing-imports = nil
            with external-symbols = (loop for s being the external-symbol of current-package
                                          collect s)
            with packages-of-exported-symbols = (loop for s in external-symbols
                                                      collect (symbol-package s))

            ;; A map package -> list of symbols,
            ;; where "package" is one of items
            ;; from MISSING-IMPORTS list.
            with not-imported-symbols = (make-hash-table)
            with all-symbols = (used-symbols all-forms)
            for symbol in all-symbols
            for package = (symbol-package symbol)
            for primary-system-name = (asdf/package-inferred-system::package-name-system (package-name package))
            unless (package-of-some-known-non-package-inferred-system-p package)
              ;; We'll use this list to ensure, that all primary
              ;; systems are included into dependencies, because otherwise
              ;; there will be problems when downloading the checked
              ;; system from official Quicklisp distribution. Official
              ;; distribution does not include secondary ASDF systems
              ;; and you whould explicitly depend on primary one.
              collect primary-system-name into primary-system-names
            do (let ((found-in-packages
                       (if (member package imported)
                           (list package)
                           (search-in-packages symbol
                                               imported))))
                 (cond
                   (found-in-packages
                    (setf used-imports
                          (union used-imports
                                 found-in-packages)))
                   ((and (not (eql package current-package))
                         (not (package-should-be-ignored package)))
                    (push symbol
                          (gethash package not-imported-symbols))
                    (pushnew (symbol-package symbol)
                             missing-imports))))
            finally (return
                      (let* ((missing-primary-systems
                               (loop with results = nil
                                     for name in primary-system-names
                                     unless (or
                                             ;; We consider UIOP is always available, becase it is
                                             ;; a part of ASDF:
                                             (string-equal name "uiop")
                                             (string-equal name "asdf")
                                             ;; We don't want to warn about systems
                                             ;; which already dependencies at ASD file level:
                                             (member name system-dependencies
                                                     :test #'string-equal)
                                             ;; Also, we don't need to warn if system is there
                                             ;; is already :import-from for this system
                                             (member name imported
                                                     :test #'string-equal
                                                     :key #'package-name)
                                             ;; We don't want to warn about currently checked system.
                                             ;; Here we are comparing name with primary-system-name
                                             ;; of the checked system, because they both could
                                             ;; be from the same primary system and in this case,
                                             ;; name is already downloaded and available for compilation
                                             (string-equal name
                                                           (asdf:primary-system-name checked-system-name)))
                                     do (pushnew
                                         (or (asdf:registered-system name)
                                             (asdf:registered-system (string-downcase name))
                                             name)
                                         results
                                         :test #'equal)
                                     finally (return results)))
                             (missing-imports (append
                                               missing-primary-systems
                                               missing-imports))
                             (missing-imports
                               (sort missing-imports
                                     #'string<
                                     :key (lambda (item)
                                            (etypecase item
                                              (package (package-name item))
                                              (asdf:system (asdf:component-name item))))))
                             (unused-imports
                               (nset-difference
                                (nset-difference
                                 (nset-difference imported
                                                  (list* current-package
                                                         used-imports))
                                 ;; We don't want to warn about symbols
                                 ;; which were imported from some package
                                 ;; and exported from the current-package:
                                 packages-of-exported-symbols)
                                primary-system-names
                                :test #'string-equal
                                :key (lambda (item)
                                       (etypecase item
                                         (string item)
                                         (package (package-name item)))))))
                        (list missing-imports
                              unused-imports
                              not-imported-symbols
                              not-found-packages)))))))


(defun downcased-package-name (package)
  (string-downcase
   (package-name package)))


(defun analyze-imports (system-name &key (allow-unused-imports nil))
  "Prints report about issues of IMPORT-FROM
   clauses in a given package-inferred ASDF system.

   This function is able to detect unused and missing imports.

   Roadmap:

   - Warn on USE of packages with too large amount of external symbols.
   - Allow to turn off linter for some forms.
   - Integrate with SBLINT and Emacs to highlight issues in the editor.

   Returns integer with number of found problems or zero."
  (asdf:load-system system-name)
  
  (flet ((format-missing-import (not-imported-symbols package-or-system)
           (let ((*package* (find-package "KEYWORD"))
                 (*print-case* :downcase)
                 (symbols (gethash package-or-system not-imported-symbols))
                 (name (etypecase package-or-system
                         (package (downcased-package-name package-or-system))
                         (asdf:system (asdf:component-name package-or-system)))))
             
             (if symbols
                 (format nil "~A (~{~S~^, ~})"
                         name
                         symbols)
                 (format nil "~A (for proper loading from Quicklisp)"
                         name)))))
    (loop with system = (asdf:registered-system system-name)
          with system-dependencies = (asdf:system-depends-on system)
          with num-problems = 0
          for filename in (system-files system-name)
          for (missing-imports
               unused-imports
               not-imported-symbols
               not-found-packages) = (analyze-file-imports system-name
               filename
               system-dependencies)
          when (or missing-imports
                   not-found-packages
                   (and unused-imports
                        (not allow-unused-imports)))
          do (format t "~2&~A:~%"
                     filename)
             (incf num-problems
                   (+ (length missing-imports)
                      (length not-found-packages)
                      (if allow-unused-imports
                          0
                          (length unused-imports))))
          when missing-imports
            do (format t "  Missing imports: ~{~A~^, ~}~%"
                       (mapcar (curry #'format-missing-import
                                      not-imported-symbols)
                               missing-imports))
               (incf num-problems (length missing-imports))
          when not-found-packages
            do (format t "  Depends on missing packages: ~{~A~^, ~}~%"
                       not-found-packages)
               (incf num-problems (length not-found-packages))
          when unused-imports
            do (format t "  Unused imports: ~{~A~^, ~}~%"
                       (mapcar #'downcased-package-name
                               unused-imports))
               (incf num-problems (length unused-imports))
          finally (return num-problems))))
