(uiop:define-package #:40ants-linter-docs/index
  (:use #:cl)
  (:import-from #:pythonic-string-reader
                #:pythonic-string-syntax)
  #+quicklisp
  (:import-from #:quicklisp)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:40ants-doc
                #:defsection
                #:defsection-copy)
  (:import-from #:40ants-linter-docs/changelog
                #:@changelog)
  (:import-from #:docs-config
                #:docs-config)
  (:export #:@index
           #:@readme
           #:@changelog))
(in-package #:40ants-linter-docs/index)

(in-readtable pythonic-string-syntax)


(defmethod docs-config ((system (eql (asdf:find-system "40ants-linter-docs"))))
  ;; 40ANTS-DOC-THEME-40ANTS system will bring
  ;; as dependency a full 40ANTS-DOC but we don't want
  ;; unnecessary dependencies here:
  #+quicklisp
  (ql:quickload "40ants-doc-theme-40ants")
  #-quicklisp
  (asdf:load-system "40ants-doc-theme-40ants")
  
  (list :theme
        (find-symbol "40ANTS-THEME"
                     (find-package "40ANTS-DOC-THEME-40ANTS")))
  )


(defsection @index (:title "40ants-linter - A custom wrapper to run SBLINT and also to validate package imports."
                    :ignore-words ("JSON"
                                   "HTTP"
                                   "TODO"
                                   "Unlicense"
                                   "ASDF"
                                   "SBLINT"
                                   "REPL"
                                   "GIT")
                    :external-docs ("https://40ants.com/ci/"))
  (40ants-linter system)
  "
[![](https://github-actions.40ants.com/40ants/linter/matrix.svg?only=ci.run-tests)](https://github.com/40ants/linter/actions)

![Quicklisp](http://quickdocs.org/badge/40ants-linter.svg)

This system is a command line wrapper around [SBLint](https://github.com/cxxxr/sblint). Additionally, it provides a way to validate imports of a package-inferred ASDF systems to find missing or unnecessary imports.
"
  (@installation section)
  (@usage section))


(defsection-copy @readme @index)


(defsection @installation (:title "Installation")
  """
You can install this library from Quicklisp, but you want to receive updates quickly, then install it from Ultralisp.org:

```
(ql-dist:install-dist "http://dist.ultralisp.org/"
                      :prompt nil)
```

Then use Roswell to build a command line tool:

```
ros install 40ants-asdf-system 40ants-linter
```
""")


(defsection @usage (:title "Usage"
                    :ignore-words ("ASDF:PACKAGE-INFERRED-SYSTEM"
                                   "ASDF"
                                   "40A"))
  "
Run linter from a command line like this:

```
40ants-linter -s cl-telegram-bot
```

To validate imports, add `--imports` key:

```
40ants-linter --imports -s cl-telegram-bot
```

Utility's exit code contains a number of found problems. Thus when it exits with code 0, run is considered successful. You can use this to build a continuous integration pipeline. 40ANTS-CI system already includes support for this linter and is able to generate proper config for GitHub actions.
")
