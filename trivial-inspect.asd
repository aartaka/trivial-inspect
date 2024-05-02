;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-2 Clause

(defsystem "trivial-inspect"
  :description "Portable toolkit for interactive inspectors."
  :author "Artyom Bologov"
  :homepage "https://github.com/aartaka/trivial-inspect"
  :bug-tracker "https://github.com/aartaka/trivial-inspect/issues"
  :source-control (:git "https://github.com/aartaka/trivial-inspect.git")
  :license  "BSD-2 Clause"
  :version "0.0.0"
  :serial t
  :depends-on ("trivial-arguments")
  :pathname "source/"
  :components ((:file "package")
               (:file "trivial-inspect")))
