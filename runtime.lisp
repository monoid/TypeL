;; This software is Copyright (c) Ivan Boldyrev 2004
;; Ivan Boldyrev grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(cl:defpackage #:typel-runtime
  (:export #:curry-wrapper #:defcurried #:lambda-curried)
  (:use #:cl))

(cl:in-package #:typel-runtime)
    
(defun curry-wrapper (function arguments-number &optional known-arguments)
  (declare (type function function)
           (type fixnum arguments-number)
           (type list known-arguments))
  #'(lambda (&rest new-arguments)
      (let ((tail (nthcdr (1- arguments-number) new-arguments)))
        (cond
          ((null tail)
           (curry-wrapper function
                          (- arguments-number (length new-arguments))
                          (append known-arguments new-arguments)))
           ((null (rest tail))
            (apply function (append known-arguments new-arguments)))
           (t
            (apply
             (apply function
                    (append known-arguments
                            (subseq new-arguments 0 arguments-number)))
             (rest tail)))))))

(defmacro defcurried (function-name args &body body)
  "Define function function-name with simple arglist args and body as
curried function.  Body may contain a docstring."
  (let* ((doc-string
          (if (and (stringp (car body))
                   (not (null (rest body))))
              (list (car body))
              nil))
         (real-body (if doc-string (rest body) body))
         (arguments (gensym (symbol-name function-name))))
    `(defun ,function-name (&rest ,arguments)
      ,@doc-string
      (apply
       (curry-wrapper #'(lambda ,args ,@real-body) ,(length args))
       ,arguments))))

(defmacro lambda-curried (args &body body)
  "Curried lambda expression."
  (let ((arguments (gensym)))
    `(lambda (&rest ,arguments)
      (apply (curry-wrapper #'(lambda ,args ,@body) ,(length args))
       ,arguments))))

(defconstant +value-key+ '+value-key+)

(defun get-value (symbol)
  "Get value of symbol."
  (declare (type symbol symbol))
  (get symbol +value-key+))

(defun (setf get-value) (value symbol)
  "Set value of symbol."
  (declare (type symbol symbol))
  (setf (get symbol +value-key+) value))
