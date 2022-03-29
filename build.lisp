;; This software is Copyright (c) Ivan Boldyrev 2003
;; Ivan Boldyrev grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.


;;;  Simple compilation process

(with-compilation-unit ()
  (cl:compile-file "runtime.lisp")
  (cl:compile-file "unification.lisp")
  (cl:compile-file "st-types.lisp")
  (cl:compile-file "lang.lisp")
  (cl:compile-file "compiler.lisp")
  (cl:compile-file "library.lisp")
  (cl:compile-file "user.lisp")
)

(cl:load "load")
