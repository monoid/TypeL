(defsystem "typel"
  :depends-on ()
  :version "0.2"
  :author "Ivan Boldyrev <lispnik@gmail.com>"
  :license "MIT License"
  :components ((:file "src/runtime")
               (:file "src/unification" :depends-on ("src/runtime"))
               (:file "src/st-types")
               (:file "src/lang")
               (:file "src/compiler" :depends-on ("src/lang"
                                                  "src/st-types"
                                                  "src/unification"
                                                  "src/runtime"))
               (:file "src/library" :depends-on ("src/runtime"
                                                 "src/unification"
                                                 "src/st-types"))
               (:file "src/user" :depends-on ("src/compiler"
                                              "src/lang"
                                              "src/library"))))
