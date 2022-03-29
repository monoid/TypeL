;; This software is Copyright (c) Ivan Boldyrev 2004
;; Ivan Boldyrev grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;; Loads all files in correct order
(cl:load "runtime")
(cl:load "unification")
(cl:load "st-types")
(cl:load "lang")
(cl:load "compiler")
(cl:load "library")
(cl:load "user")
