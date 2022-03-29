;; This software is Copyright (c) Ivan Boldyrev 2004
;; Ivan Boldyrev grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(cl:defpackage #:typel
  (:export #:+ #:- #:/ #:* #:*. #:/. #:+. #:-. #:0? #:int-to-float
           #:if #:lambda #:let #:letrec #:typel #:deftypel
           #:true #:false #:not #:and #:or #:xor
           #:equal #:= #:=.
           #:match
           #:first #:rest #:cons #:mapcar)
  (:import-from #:common-lisp
                #:quote)
  (:use))
