(defsystem "typel"
  :depends-on ()
  :version "0.1.1"
  :author "Ivan Boldyrev <lispnik@gmail.com>"
  :license "MIT License"
  :components ((:file "runtime")
               (:file "unification" :depends-on ("runtime"))
               (:file "st-types")
               (:file "lang")
               (:file "compiler" :depends-on ("lang"
                                              "st-types"
                                              "unification"
                                              "runtime"))
               (:file "library" :depends-on ("runtime" "unification" "st-types"))
               (:file "user" :depends-on ("compiler" "lang" "library"))))
