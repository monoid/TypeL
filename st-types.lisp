;; This software is Copyright (c) Ivan Boldyrev 2004
;; Ivan Boldyrev grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(cl:defpackage #:typel-types
  (:export
   #:type #:standard-type #:integer #:string #:list #:boolean
   #:generic-type #:function-type #:function-type-dom #:function-type-cod
   #:clone-type #:fresh-generic-type #:make-function-type
   #:make-algebraic-type-info
   #:typed-list-type
   #:parse-type
   #:+integer+ #:+string+ #:+list+ #:+boolean+ #:+float+ #:+symbol+)
  (:use #:cl))

(cl:in-package #:typel-types)

(defclass solution () ((var :initarg :var :accessor solution-var)
                       (exp :initarg :exp :accessor solution-exp)))

(defmethod cl:print-object ((s solution) stream)
  (format stream "#<:= ~S ~S>"
          (solution-var s)
          (solution-exp s)))

(defclass basic-type () ())

(defclass standard-type (basic-type) ((symbol :initarg :symbol :type symbol)))

(defmethod cl:print-object ((type standard-type) stream)
  (format stream "~S" (slot-value type 'symbol)))

(defvar +integer+ (make-instance 'standard-type :symbol 'integer))
(defvar +float+   (make-instance 'standard-type :symbol 'float))
(defvar +string+  (make-instance 'standard-type :symbol 'string))
(defvar +list+    (make-instance 'standard-type :symbol 'list))
(defvar +boolean+ (make-instance 'standard-type :symbol 'boolean))
(defvar +symbol+  (make-instance 'standard-type :symbol 'symbol))

(defvar *counter* 0)

(defclass generic-type (basic-type)
  ((counter :initform (incf *counter*)  ; This field is only for
					; printing representation
            :type integer)))

(defun fresh-generic-type ()
  (make-instance 'generic-type))

(defmethod cl:print-object ((type generic-type) stream)
  (format stream "'TYPE~S" (slot-value type 'counter)))


(defclass function-type (basic-type)
  ((dom
    :initarg :from
    :initarg :dom
    :accessor function-type-dom)
   (cod
    :initarg :to
    :initarg :cod
    :accessor function-type-cod)))

(defmethod cl:print-object ((type function-type) stream)
  (let ((dom (function-type-dom type))
        (cod (function-type-cod type)))
    (if (typep dom 'function-type)
        (format stream "(~S) -> ~S" dom cod)
        (format stream "~S -> ~S" dom cod))))

(defun make-function-type (&rest param &key from to dom cod)
  (declare (ignore from to dom cod))
  (apply #'make-instance 'function-type param))

;;;  This class contains metainformation on algebraic type.
;;; NAME is type name
;;; ARGUMENTS is list of symbols -- type arguments name
;;; VARIANTS is list of type variants.  Each type variant is list of form:
;;;   (TAG TYPE1 TYPE2 TYPE3 ...)
;;; Each TYPE-N is sexp type definition.  Type arguments are referred to
;;; in quoted from.
(defclass algebraic-type-info ()
  ((name :initarg :name)
   (arguments
    :initarg :arguments)
   (variants
    :initarg :variants)))

;;;  TODO: change
(defmethod cl:print-object ((type algebraic-type-info) stream)
  (with-slots (name arguments variants) type
    (format stream "#<~S ~{(~S)~}>)))" name arguments)))

(defun make-algebraic-type-info (name arguments variants)
  (make-instance 'algebraic-type-info
                 :name name
                 :arguments arguments
                 :variants variants))

(defclass algebraic-type (basic-type)
  ((name :initarg :name)
   (arguments
    :initarg :arguments)
   (metainfo
    :initarg :metainfo)))

(defun make-algebraic-type (name arguments metainfo)
  (make-instance 'algebraic-type-info
                 :name name
                 :arguments arguments
                 :metainfo metainfo))

(defclass typed-list-type (basic-type)
  ((element-type :initarg :element-type)))

(defmethod cl:print-object ((type typed-list-type) stream)
  (with-slots (element-type) type
    (format stream "(:LIST ~S)" element-type)))

(defun make-typed-list-type (element-type)
  (make-instance 'typed-list-type
                 :element-type element-type))

(defclass type-ref ()
  ((ref :initarg :ref
        :accessor get-type-by-ref)))

(defgeneric clone (object))

(defmethod clone ((type generic-type))
  (make-instance 'generic-type))

(defmethod clone ((type basic-type))
  type)

(defmethod clone ((ref type-ref))
  (make-instance 'type-ref
                 :ref (clone (get-type-by-ref ref))))

(defgeneric clone-type (type hash)
  (:documentation "Make clone of a type.  Elements of same type are
stored in hash."))

(defmethod clone-type ((type generic-type) hash)
  (let ((lookup (gethash type hash)))
    (or lookup
        (setf (gethash type hash) (make-instance 'generic-type)))))

(defmethod clone-type ((type standard-type) hash)
  type)

(defmethod clone-type ((type function-type) hash)
  (let ((lookup (gethash type hash)))
    (if lookup
        lookup
        (let ((dom-clone (clone-type (function-type-dom type ) hash))
              (cod-clone (clone-type (function-type-cod type) hash)))
          (setf (gethash type hash)
                (if (and (eq dom-clone (function-type-dom type))
                         (eq cod-clone (function-type-cod type)))
                    type
                    (make-instance 'function-type
                                   :dom dom-clone
                                   :cod cod-clone)))))))

(defmethod clone-type ((type typed-list-type) hash)
  (let ((lookup (gethash type hash)))
    (or lookup
        (let ((tp (clone-type (slot-value type 'element-type) hash)))
          (setf (gethash type hash)
                (if (eq tp (slot-value type 'element-type))
                    type
                    (make-instance 'typed-list-type
                                   :element-type tp)))))))

(defgeneric contains-p (exp var))

(defmethod contains-p ((exp function-type) var)
  (or (contains-p (function-type-dom exp) var)
      (contains-p (function-type-cod exp) var)))

(defmethod contains-p ((exp typed-list-type) var)
  (contains-p (slot-value exp 'element-type) var))

(defmethod contains-p ((exp algebraic-type) var)
  (some #'(lambda (variant)
            (some #'(lambda (type)
                      (contains-p type var))
                  (rest variant)))
        (slot-value exp 'variants)))

(defmethod contains-p ((exp generic-type) var)
  (eq exp var))

(defmethod contains-p ((exp type-ref) var)
  (contains-p (deref exp) var))

(defmethod contains-p (exp var)
  nil)

;;; TYPEL-UNIFICATION related definitions

(defgeneric deref (ref))
(defmethod deref (ref)
  ref)
(defmethod deref ((ref type-ref))
  (deref (get-type-by-ref ref)))

(defmethod typel-unification:reccurent-solution-p ((solution solution))
  (let ((var (deref (solution-var solution)))
        (exp (deref (solution-exp solution))))
    (and
     (not (eq var exp))
     (contains-p exp var))))

(defmethod typel-unification:resolve-equation
    ((left-side generic-type)  right-side)
  (values nil
          (list (make-instance 'solution :var left-side :exp right-side))))

(defmethod typel-unification:resolve-equation
    (left-side (right-side generic-type))
  (values nil
          (list (make-instance 'solution :var right-side :exp left-side))))

(defmethod typel-unification:resolve-equation ((right-side function-type)
                                                (left-side  function-type))
  (values (list (cons (function-type-dom right-side)
                      (function-type-dom left-side))
                (cons (function-type-cod right-side)
                      (function-type-cod left-side)))
          nil))

(defmethod typel-unification:resolve-equation ((right-side algebraic-type)
                                                (left-side algebraic-type))
  (if (eq (slot-value right-side 'name)
          (slot-value left-side 'name))
      (values 
       (mapcar #'(lambda (r l)
                   (cons (cdr r)
                         (cdr l)))
               (slot-value right-side 'arguments)
               (slot-value left-side 'arguments))
       nil)))

(defmethod typel-unification:resolve-equation ((right-side typed-list-type)
                                                (left-side typed-list-type))
  (typel-unification:resolve-equation
   (slot-value right-side 'element-type)
   (slot-value left-side 'element-type)))

(defmethod typel-unification:resolve-equation ((right-side standard-type)
                                                (left-side standard-type))
  (if (eq left-side right-side)
      (values nil nil)
      (throw :type-unificatiln-error
        (list :type-unificatiln-error right-side left-side))))

(defmethod typel-unification:resolve-equation (right-side left-side)
  (throw :type-unificatiln-error
    (list :type-unificatiln-error right-side left-side)))

(defgeneric subst-obj (obj solution))

(defmethod subst-obj ((obj generic-type) solution)
  (if (eq obj (solution-var solution))
      (solution-exp solution)
      obj))

(defmethod subst-obj ((obj function-type) solution)
  (let ((subst-dom (subst-obj (function-type-dom obj) solution))
        (subst-cod (subst-obj (function-type-cod obj) solution)))
    (if (and (eq (function-type-dom obj)
                 subst-dom)
             (eq (function-type-cod obj)
                 subst-dom))
        obj
        (make-function-type :dom subst-dom :cod subst-cod))))

(defmethod subst-obj ((obj algebraic-type) solution)
  (let ((subst-args (mapcar #'(lambda (arg)
                                (subst-obj arg solution))
                            (slot-value obj 'arguments))))
    (if (equal subst-args (slot-value obj 'arguments))
        obj
        (make-algebraic-type
         (slot-value obj 'name)
         subst-args
         (slot-value obj 'metainfo)))))

(defmethod subst-obj ((obj typed-list-type) solution)
  (let ((subst-elem (subst-obj (slot-value obj 'element-type) solution)))
    (if (eq subst-elem (slot-value obj 'element-type))
        obj
        (make-typed-list-type subst-elem))))

(defmethod subst-obj (obj solution)
  obj)

(defmethod typel-unification:substitute-solution (equation solution)
  (let ((left (subst-obj (car equation) solution))
        (rigt (subst-obj (cdr equation) solution)))
    (if (and (eq left (car equation))
             (eq rigt (cdr equation)))
        equation
        (cons left rigt))))
;;;;;;
;;;
;;;  Type parser
;;;
;;;  Rules:
;;;
;;;  If type is symbol, it is standard type (INTEGER, BOOLEAN, etc)
;;;
;;;  If type is (QUOTE symbol), it is generic type, and equal symbols
;;;  produce same instance of generic type.  It it possible, but not
;;;  advised, to use names of standard types in quoted generic types.
;;;
;;;  If type is list that starts from keyword, the list has special
;;;  meaning depending on the keyword (see below).
;;;
;;;  If type is a list, it is a function from (FIRST list) to (REST
;;;  list).  Single-element lists are invalid.
;;;
;;;  Types with keywords as first elements:
;;;  *  (:LIST type1)  List with elements of type type1.
;;;  *  (:ALG name type1 type2 ...)  Algebraic type with defined name
;;;     and arguments typeN.

(defvar *type-dictionary* nil
  "Dictionary of generic types")

(defun get-generic-type (symbol)
  "Get generic type for symbol.  New generic types are generated on-demand."
  (let ((type (gethash symbol *type-dictionary*)))
    (if type
        type
        (setf (gethash symbol *type-dictionary*) (fresh-generic-type)))))

(defgeneric parse-list-type (head tail)
  (:documentation "Parse list type"))

(defun get-standard-type (standard-type-symbol)
  (ecase standard-type-symbol
    (string  +string+)
    (boolean +boolean+)
    (symbol  +symbol+)
    (integer +integer+)
    (float   +float+)))

(defun parse-type (type-info)
  (let ((*type-dictionary* (make-hash-table :test 'eq)))
    (parse-type-1 type-info)))

(defun parse-type-1 (type-info)
  (etypecase type-info
    (symbol (get-standard-type type-info))
    (cons   (parse-list-type (first type-info) (rest type-info)))))

;;;  (:LIST type)
(defmethod parse-list-type ((head (eql :list)) type-of-a-list)
  (make-typed-list-type (parse-type-1 (first type-of-a-list))))

;;;  (QUOTE a)
(defmethod parse-list-type ((head (eql 'quote)) symbol)
  (get-generic-type (first symbol)))

;;;  Funcall
(defmethod parse-list-type (head tail)
  (let ((rev (reverse (cons head tail))))
    (let ((cod (first rev))
          (args (rest rev)))
      (do* ((rest-args args (cddr rest-args))
            (arg (second rest-args) (second rest-args))
            (type (make-function-type :from (parse-type-1 arg)
                                      :to   (parse-type-1 cod))
                  (make-function-type :from (parse-type-1 arg)
                                      :to type)))
           ((null (cddr rest-args))
            type)
        #|Nothing|#))))
