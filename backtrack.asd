;;;; backtrack.asd

(defpackage :backtrack-system
  (:use :cl :asdf))
(in-package :backtrack-system)

(defsystem :backtrack
  :name "backtrack"
  :serial t
  :components
  ((:static-file "backtrack.asd")
   (:module :src
            :components ((:file "package")
                         (:file "cond")
                         (:file "throw")
                         (:file "wrap"))
            :serial t))
  :depends-on (:alexandria :iterate))

(defsystem :backtrack-test
  :name "backtrack-test"
  :serial t
  :components
  ((:module :test
            :components ((:file "package")
                         (:file "backtrack-test"))))
  :depends-on (:backtrack :stefil))
