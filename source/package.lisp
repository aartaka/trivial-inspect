;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-2 Clause

(uiop:define-package :trivial-inspect
  (:use :common-lisp)
  (:export #:fields #:description)
  (:import-from
   #+sbcl      #:sb-ext
   #+clozure   #:ccl
   #+ecl       #:ext
   #+abcl      #:ext
   #+clasp     #:ext
   #+lispworks #:hcl
   #+allegro   #:excl
   #:package-local-nicknames)
  (:import-from
   #+abcl      #:mop
   #+allegro   #:mop
   #+clisp     #:clos
   #+clozure   #:ccl
   #+cmu       #:clos-mop
   #+ecl       #:clos
   #+clasp     #:clos
   #+lispworks #:clos
   #+mcl       #:ccl
   #+sbcl      #:sb-mop
   #+scl       #:clos
   #+mezzano   #:mezzano.clos
   #+sicl      #:sicl-clos
   #:class-slots
   #:slot-definition-name)
  (:documentation "`trivial-inspect' provides building blocks for interactive inspectors.
Two main functions it exports are:
- `fields' to get a list of inspect fields for an object.
- `description' for a concise description of an object to stream."))
