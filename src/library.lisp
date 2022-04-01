;; Copyright (c) 2004, 2022 Ivan Boldyrev
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(cl:defpackage #:typel-lib
  (:use #:cl)
  (:import-from #:typel-runtime
                #:lambda-curried)
  (:import-from #:typel-types
                #:make-function-type
                #:fresh-generic-type
                #:parse-type
                #:+integer+
                #:+boolean+
                #:+string+
                #:+float+)
  (:import-from #:typel-compiler
                #:lookup-info
                #:lookup-type-info
                #:make-id-info
                #:+true+
                #:+false+
                #:*namespace*
                #:polymorphic-type-info 
                #:get-value))

(cl:in-package #:typel-lib)

(defmacro defcore (symbol type definition)
  (let ((arg (gensym))
        (type-info (parse-type type)))
    `(progn
      (define-symbol-macro ,symbol (get-value ',symbol))
      (setf (lookup-info ',symbol *namespace*)
       (make-id-info                      ; TODO representation info
        :name ',symbol
        :type-info (polymorphic-type-info (parse-type ',type))))
      (setf (get-value ',symbol) ,definition)
      ,(when (typep type-info
                    'typel-types:function-type)
             `(defun ,symbol (&rest ,arg)
               (apply ,symbol ,arg))))))

(defcore typel:+. (float -> float -> float)
  (lambda-curried (x y) (+ x y)))

(defcore typel:-. (float -> float -> float) 
  (lambda-curried (x y) (- x y)))

(defcore typel:*. (float -> float -> float)
  (lambda-curried (x y) (* x y)))

(defcore typel:/. (float -> float -> float)
  (lambda-curried (x y) (/ x y)))

(defcore typel:+ (integer -> integer -> integer)
  (lambda-curried (x y) (+ x y)))

(defcore typel:- (integer -> integer -> integer)
  (lambda-curried (x y) (- x y)))

(defcore typel:* (integer -> integer -> integer)
  (lambda-curried (x y) (* x y)))

(defcore typel:/ (integer -> integer -> integer)
  (lambda-curried (x y) (/ x y)))

(defcore typel:true boolean
  +true+)

(defcore typel:false boolean
  +false+)

(defcore typel:not (boolean -> boolean)
  (lambda (x) (if (eq x +true+) typel:false +true+)))

(defcore typel:and (boolean -> boolean -> boolean)
  (lambda (x y) (if (eq x +false+)
                    +false+
                    y)))

(defcore typel:or (boolean -> boolean -> boolean)
  (lambda (x y) (if (eq x +false+)
                    y
                    +true+)))

(defcore typel:xor (boolean -> boolean -> boolean)
  (lambda (x y) (if (eq x y)
                    +false+
                    +true+)))

(defcore typel:0? (integer -> boolean)
  (lambda (n) (if (zerop n) +true+ +false+)))

(defcore typel:int-to-float (integer -> float)
  (lambda (x) (coerce x 'double-float)))

(defcore typel:equal ('a -> 'a -> boolean)
  (lambda-curried (x y)
                  (if (equal x y)
                      +true+
                      +false+)))

(defcore typel:= (integer -> integer -> boolean)
  (lambda-curried (x y)
                  (if (= x y)
                      +true+
                      +false+)))

(defcore typel:=. (float -> float -> boolean)
  (lambda-curried (x y)
                  (if (= x y)
                      +true+
                      +false+)))

;;;;;
;;;
;;; List operations
;;;

;;; NIL
(setf (lookup-info 'nil *namespace*)
       (make-id-info                      ; TODO representation info
        :name nil
        :type-info (polymorphic-type-info (parse-type '(:list 'a)))))

(defcore typel:first ((:list 'a) -> 'a)
  (lambda (x) (first x)))

(defcore typel:rest ((:list 'a) -> (:list 'a))
  (lambda (x) (rest x)))

(defcore typel:cons ('a -> (:list 'a) -> (:list 'a))
  (lambda-curried (x y)
                  (cons x y)))

(defmacro typel:let (&rest body)
  `(typel:typel
    (typel:let ,@body)))

(defmacro typel:letrec (&rest body)
  `(typel:typel
    (typel:letrec ,@body)))

(defmacro typel:if (&rest body)
  `(typel:typel
    (typel:if ,@body)))

(defmacro typel:lambda (&rest body)
  `(typel:typel
    (typel:lambda ,@body)))
