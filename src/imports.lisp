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
  (loop with result = nil
        for (option-name . option-value) in (cddr form)
        when (eql option-name
                  :use)
          do (setf result
                   (nunion result
                           (mapcar #'symbol-name option-value)
                           :test #'string-equal))
        when (eql option-name
                  :import-from)
          do (pushnew (symbol-name (car option-value))
                      result
                      :test #'string-equal)
        finally (return (values
                         ;; All packages, mentioned in :USE and :IMPORT-FROM
                         (mapcar #'find-package
                                 (sort result
                                       #'string<))
                         ;; The current package itself should be ignored
                         ;; during the search of missing imports
                         (find-package (second form))))))


(defun form-symbols (form)
  (typecase form
    (cons
     (append (form-symbols (car form))
             (form-symbols (cdr form))))
    (symbol
     (list form))
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
    (loop with eof = '#:eof
          with *package* = (uiop:ensure-package "IMPORTS-LINTER-WORKSPACE"
                                                :use '("COMMON-LISP"))
          with *readtable* = (copy-readtable nil)
          for form = (read-preserving-whitespace input nil eof)
          until (eq form eof)
          when (or (eql (car form)
                        'cl:in-package)
                   (eql (car form)
                        'named-readtables:in-readtable))
            do (eval form)
          collect form)))


(defparameter *packages-to-ignore*
  '("sb-ext"
    "sb-int"
    "sb-c"
    "sb-mop"
    "sb-pcl"
    "uiop"
    "asdf"
    "40ants-linter"))


(defun should-be-ignored (package)
  (let* ((name (asdf:primary-system-name
                (package-name package))))
    (member name *packages-to-ignore*
            :test #'string-equal)))


(defun search-in-package (symbol package)
  (loop for candidate being the symbol of package
          thereis (eql symbol
                       candidate)))


(defun search-in-packages (symbol imported-packages)
  (loop for package in imported-packages
        when (search-in-package symbol package)
          collect package))


(defun analyze-file-imports (filename)
  (let* ((all-forms (read-forms filename))
         (package-def (find-if #'package-definition-p
                               all-forms)))
    (multiple-value-bind (imported current-package)
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
            do (let ((found-in-packages
                         (or (and (member package imported)
                                  (list package))
                             (search-in-packages symbol
                                                 imported))))
                   (cond
                     (found-in-packages
                      (setf used-imports
                            (union used-imports
                                   found-in-packages)))
                     ((and (not (eql package current-package))
                           (not (should-be-ignored package)))
                      (push symbol
                            (gethash package not-imported-symbols))
                      (pushnew (symbol-package symbol)
                               missing-imports))))
            finally (return
                      (let ((unused-imports
                              (nset-difference
                               (nset-difference imported
                                                (list* current-package
                                                       used-imports))
                               ;; We don't want to warn about symbols
                               ;; which were imported from some package
                               ;; and exported from the current-package:
                               packages-of-exported-symbols))
                            (missing-imports
                              (sort missing-imports
                                    #'string<
                                    :key #'package-name)))
                        (list missing-imports
                              unused-imports
                              not-imported-symbols)))))))


(defun downcased-package-name (package)
  (string-downcase
   (package-name package)))


(defun analyze-imports (system-name)
  "Prints report about issues of IMPORT-FROM
   clauses in a given package-inferred ASDF system.

   This function is able to detect unused and missing imports.

   Roadmap:

   - Warn on USE of packages with too large amount of external symbols.
   - Allow to turn off linter for some forms.
   - Integrate with SBLINT and Emacs to highlight issues in the editor.

   Returns integer with number of found problems or zero."
  (asdf:load-system system-name)
  
  (flet ((format-missing-import (not-imported-symbols package)
           (let ((*package* (find-package "KEYWORD"))
                 (*print-case* :downcase))
             (format nil "~A (~{~S~^, ~})"
                     (downcased-package-name package)
                     (gethash package not-imported-symbols)))))
    (loop with num-problems = 0
          for filename in (system-files system-name)
          for (missing-imports unused-imports not-imported-symbols) = (analyze-file-imports filename)
          when (or missing-imports
                   unused-imports)
            do (format t "~2&~A:~%"
                       filename)
          when missing-imports
            do (format t "  Missing imports: ~{~A~^, ~}~%"
                       (mapcar (curry #'format-missing-import
                                      not-imported-symbols)
                               missing-imports))
               (incf num-problems (length missing-imports))
          when unused-imports
            do (format t "  Unused imports: ~{~A~^, ~}~%"
                       (mapcar #'downcased-package-name
                               unused-imports))
               (incf num-problems (length unused-imports))
          finally (return num-problems))))
