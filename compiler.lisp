;; This software is Copyright (c) Ivan Boldyrev 2004
;; Ivan Boldyrev grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(cl:defpackage #:typel-compiler
  (:use #:cl)
  (:import-from #:typel-runtime
                #:get-value
                #:lambda-curried)
  (:import-from #:typel-types
                #:fresh-generic-type
                #:make-function-type
                #:make-algebraic-type-info
                #:+integer+
                #:+boolean+
                #:+string+
                #:+float+
                #:+symbol+))

(cl:in-package #:typel-compiler)

;;;;;;
;;;
;;; Nested namespaces

;;;  Information on identifier.  If REPRESENTATION is :FUNCTION,
;;;  identifier is represented at Common Lisp as a function (with
;;;  DEFUN, LABELS etc.) and must be curried if passed as argument
;;;  (and in some other cases as well).  If PRESENTATION is :VALUE, it
;;;  is a LET/LETREC-established value of global, though it can have
;;;  type of function.  ARITY is number of arguments of function (note
;;;  that if function returns other function, arity is smaller than
;;;  possible number of arguments for curried variant.
(defstruct id-info
  "Name is name of id.
TYPE-INFO is type information.
REPRESENTATION is :FUNCTION or :VALUE or NIL (as unknown).
ARITY is number of arguments of function.
"
  (name nil :type symbol)
  (type-info)
  (representation nil :type (member nil :function :value))
  (arity 0 :type fixnum))

;;; Namespace is table SYMBOL->ID-INFO.
(defstruct namespace
  (names (make-hash-table :test 'eq) :type hash-table)
  (parent nil :type (or namespace null)))

;;; Global namespace (for global functions etc.)
(defvar *namespace* (make-namespace))

;;;  Lookup ID-INFO for symbol.  If current namespace doesn't know
;;;  anything, parent namespace is examined, and so on.
(defun lookup-info (sym &optional (*namespace* *namespace*))
  (declare (type symbol sym))
  (let ((info (gethash sym (namespace-names *namespace*)))
        (parent (namespace-parent *namespace*)))
    (or info
        (lookup-info sym parent))))

(defun lookup-type-info (key &optional (*namespace* *namespace*))
  "Lookup ID-INFO and fetch type information"
  (declare (type symbol key))
    (let ((id-info (lookup-info key)))
      (and id-info (id-info-type-info id-info))))

(defun (setf lookup-info) (value key &optional (*namespace* *namespace*))
  (setf (gethash key (namespace-names *namespace*)) value))

(defmacro with-namespace (namespace &body body)
  `(let ((*namespace* ,namespace))
    ,@body))

(defmacro with-new-namespace (&body body)
  `(with-namespace (make-namespace :parent *namespace*)
    ,@body))

;;;;;;
;;;
;;;  Functions for type info representation
;;;
;;;  Type information is function.  It is (constantly smth.) for
;;;  simple values.  It is function that create instance of type
;;;  equations for polymorphic values.
;;;
;;;  There are three functions: for simple values, for polymorphic
;;;  LET-bound values and for polymorphic LETREC-bound values.  This
;;;  function return two values: type of item and additional type
;;;  equations
(defgeneric type-description (ir)
  (:documentation "Return two values: type and list of type "))

(defun simple-type-info (type)
  (lambda ()
    (values
     type
     '())))

(defun polymorphic-type-info (type)
  (lambda ()
    (values
     (typel-types:clone-type type (make-hash-table :test 'eq))
     nil)))

;;;;;;
;;;
;;;  IR

(eval-when (:compile-toplevel :load-toplevel :execute)
   (defgeneric make-ir (head tail))

   (defclass variable-elem (ir-element)
     ((name :initarg form)))

   (defclass constant (ir-element)
     ((type-info :initarg type-info :accessor type-info)
      (value :initarg form)))

   (defun make-ir-const (form type)
     (make-instance 'constant 'form form 'type-info (simple-type-info type)))

   (defun make-ir-var (form)
     (make-instance 'variable-elem
                    'form form)))

(defgeneric compile-to-lisp (ir))

(defun convert-to-lambda-form (decl)
  (destructuring-bind (obj value) decl
    (if (consp obj)
        (list (first obj)
              `(typel:lambda ,(rest obj) ,value))
        decl)))

(defclass ir-element ()
  ((lisp-form :initarg form :accessor lisp-form)))

(defmethod compile-to-lisp ((var variable-elem))
  (lisp-form var))

(defmethod type-description ((var variable-elem))
  (funcall (lookup-type-info (slot-value var 'name))))

(defmethod type-description ((form constant))
  (funcall (type-info form)))

(defmethod compile-to-lisp ((cons constant))
  (lisp-form cons))

(defmethod cl:print-object ((form constant) output)
  (format output
          "#<~S :VALUE ~S :TYPE-INFO ~S>"
          'constant
          (slot-value form 'value)
          (type-info form)))

(defclass form (ir-element)
  ())

(defclass funcall-elem (ir-element)
  ((function :initarg function :reader funcall-function)
   (arguments :initarg arguments :reader funcall-arguments)))

(defun process-fun-call (result-type rev-arguments asserts)
  (if (null rev-arguments)
      (values result-type asserts)
      (let ((arg (first rev-arguments)))
        (multiple-value-bind (arg-type arg-asserts) (type-description arg)
          (process-fun-call
           (make-function-type :from arg-type :to result-type)
           (rest rev-arguments)
           (nconc arg-asserts asserts))))))

(defmethod type-description ((ir-element funcall-elem))
  (with-slots (function arguments) ir-element
    (multiple-value-bind (fun-type fun-asserts) (type-description function)
      (let ((result-type (fresh-generic-type)))
        (multiple-value-bind (called-type called-asserts)
            (process-fun-call result-type (reverse arguments) fun-asserts)
          (values
           result-type
           (list*
            (cons called-type fun-type)
            called-asserts)))))))

(defmethod compile-to-lisp ((funcall funcall-elem))
  `(funcall
    ,(compile-to-lisp (funcall-function funcall))
    ,@(mapcar #'compile-to-lisp (funcall-arguments funcall))))

(defmethod cl:print-object ((obj funcall-elem) output)
  (format output "#<FUNCALL :FUNCTION ~S :ARGUMENTS ~S>"
          (slot-value obj 'function)
          (slot-value obj 'arguments)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +TRUE+ 'typel:true)
  (defconstant +FALSE+ 'typel:false)

  (defun convert-to-ir (form)
    (if (or (eq form +TRUE+)
            (eq form +FALSE+))
        (make-ir-const form +boolean+)
      (etypecase form
        (symbol
         (make-ir-var form))
        (integer
         (make-ir-const form +integer+))
        (string
         (make-ir-const form +string+))
        (float
         (make-ir-const form +float+))
        (cons
         (make-ir (first form) (rest form))))))

  (defun fill-namespace (var-list)
    (dolist (var var-list)
      (let ((id-info (make-id-info :name var
                                   :type-info (simple-type-info (fresh-generic-type)))))
        (setf (lookup-info var *namespace*) id-info)))))

(defmacro defform (class-name
                   form
                   superclasses
                   fields
                   pattern)
  (let ((head-var (gensym))
        (body-var (gensym))
        (field-names (mapcar #'(lambda (var)
                                 (if (consp var)
                                     (first var)
                                     var))
                             fields)))
    (let ((body-of-make-ir
           ` (let ,(mapcar
                       #'(lambda (var)
                           (if (symbolp var)
                               `(,var (convert-to-ir ,var))
                               var))
                       fields)
                 (make-instance (quote ,class-name)
                                ,.(mapcan #'(lambda (name)
                                              `((quote ,name) ,name))
                                          field-names)
                                'form (cons ,head-var ,body-var)))))
      `(progn
        ;; Class definition
        (defclass ,class-name ,superclasses
          ,(mapcar #'(lambda (var) (list var ':initarg var))
                   field-names))

        ;; Creation method
        (defmethod make-ir ((,head-var (eql (quote ,form))) ,body-var)
          (destructuring-bind ,pattern ,body-var
            ,body-of-make-ir))

        ;; Print object; this is very simple implementation for debug
        ;; proposes only
        (defmethod cl:print-object ((obj ,class-name) output)
          (format output "~S"
                  (list 'cl:make-instance '',class-name
                         ''form (list 'quote (lisp-form obj))
                         ,@(mapcan #'(lambda (fieldname)
                                      `('',fieldname
                                        (slot-value obj ',fieldname)))
                                  field-names))))))))

(defform let-form typel:let (form)
         ((arguments (mapcar
                      #'(lambda (argdesc)
                          (list (first argdesc)
                                (with-namespace (namespace-parent *namespace*)
                                  (convert-to-ir (second argdesc)))))
                      (mapcar #'convert-to-lambda-form arguments)))
          body
          (namespace *namespace*))
         (arguments body))

(defmethod compile-to-lisp ((let-form let-form))
  (with-slots (arguments body) let-form
  `(let ,(mapcar #'(lambda (var)
                      (list (first var) (compile-to-lisp (second var))))
                  arguments)
    ,(compile-to-lisp body))))

(defun letform-fill-namespace (arguments asserts)
  (if (null arguments)
      asserts
      (let* ((arg (first arguments))
             (var (first arg))
             (val (second arg)))
        (if (typep val 'lambda-form)
            (let ((id-info
                   (make-id-info :name var
                                 :type-info (lambda ()
                                              (type-description val)))))
              (setf (lookup-info var *namespace*) id-info)
              (letform-fill-namespace (rest arguments) asserts))
            (multiple-value-bind (val-type val-asserts)
                (type-description val)
              (let ((id-info
                     (make-id-info :name var
                                   :type-info (simple-type-info val-type))))
                (setf (lookup-info var *namespace*) id-info)
                (letform-fill-namespace (rest arguments)
                                       (nconc val-asserts asserts))))))))

;;;  Type description for LET form
;;;
;;;  If value is simple (i.e. not a lambda-form) constant type is stored
;;;  in namespace and type assertions are generated only once.
;;;
;;;  If value is lambda-form, polymorphic type is stored.  Type and
;;;  asserions are regenerated at each use of value.
(defmethod type-description ((let-form let-form))
  (with-slots (arguments body) let-form
    (with-new-namespace
        ;; Fill namespace with names
        (let ((arg-asserts (letform-fill-namespace arguments nil)))
        ;;; Calculate type description
          (multiple-value-bind (body-type body-asserts) (type-description body)
            (values
             body-type
             (nconc
              body-asserts
              arg-asserts)))))))

(defform letrec-form typel:letrec (form)
         ((arguments (mapcar
                      #'(lambda (argdesc)
                          (list (first argdesc)
                                (convert-to-ir (second argdesc))))
                      (mapcar #'convert-to-lambda-form arguments)))
          body
          (namespace *namespace*))
         (arguments body))

(defmethod compile-to-lisp ((letrec-form letrec-form))
  (with-slots (arguments body) letrec-form
  `(let ,(mapcar #'first
                  arguments)
     (setf ,@(mapcan #'(lambda (var)
                      (list (first var) (compile-to-lisp (second var))))
                  arguments))
    ,(compile-to-lisp body))))

;;;  Type info for LETREC form
;;;  Every LETREC-bund variable must be polymorphic lambda (real type may be
;;;  integer -> integer, but we don't know it before unification, so we can't
;;;  make assumptoins).

(defun letrec-func-description (fun arguments)
  (lambda ()
    (with-new-namespace
        (fill-namespace (mapcar #'first arguments))
      (values
       (funcall (lookup-type-info fun))
       (mapcan #'(lambda (argdesc)
                   (multiple-value-bind (type asserts)
                       (type-description (second argdesc))
                     (cons
                      (cons (funcall (lookup-type-info (first argdesc)))
                            type)
                      asserts)))
               arguments)))))
              
(defmethod type-description ((letrec-form letrec-form))
  (with-slots (arguments body) letrec-form
    (with-new-namespace
        (dolist (argdesc arguments)
          (setf (lookup-info (first argdesc) *namespace*)
                (make-id-info :name (first argdesc)
                              :type-info (letrec-func-description
                                          (first argdesc)
                                          arguments))))
      (type-description body))))


(defform lambda-form typel:lambda (form)
         ((arguments arguments)
          body
          (namespace *namespace*))
         (arguments body))

(defmethod compile-to-lisp ((lambda-form lambda-form))
  `(typel-runtime:lambda-curried ,(slot-value lambda-form 'arguments)
     ,(compile-to-lisp (slot-value lambda-form 'body))))

(defmethod type-description ((lambda-form lambda-form))
  (with-slots (arguments body) lambda-form
    (with-new-namespace
        (fill-namespace arguments)
      (multiple-value-bind (body-type body-assertions) (type-description body)
        (values
         (reduce #'(lambda (var cur-type)
                     (make-function-type
                      :from (funcall (lookup-type-info var))
                      :to cur-type))
                 arguments
                 :initial-value body-type
                 :from-end t)
         body-assertions)))))

(defform if-form typel:if (form)
         (condition true-branch false-branch)
         (condition true-branch false-branch))

(defmethod type-description ((if-form if-form))
  (with-slots (condition true-branch false-branch) if-form
    (multiple-value-bind (cond-type cond-asserts)
        (type-description condition)
      (multiple-value-bind (true-type true-asserts)
          (type-description true-branch)
        (multiple-value-bind (false-type false-asserts)
            (type-description false-branch)
          (values true-type
                  (nconc
                   (list (cons true-type false-type)
                         (cons +boolean+ cond-type))
                   cond-asserts
                   true-asserts
                   false-asserts)))))))

(defmethod compile-to-lisp ((if-form if-form))
  (with-slots (condition true-branch false-branch) if-form
    `(if (eq +TRUE+ ,(compile-to-lisp condition))
      ,(compile-to-lisp true-branch)
      ,(compile-to-lisp false-branch))))

(defform quote-form quote (form)
         ((symbol symbol))
         (symbol))

(defmethod compile-to-lisp ((quote-form quote-form))
  (with-slots (symbol) quote-form
    `(quote ,symbol)))

(defmethod type-description ((quote-form quote-form))
  (values +symbol+ nil))

(defmethod make-ir (function args)
  (make-instance 'funcall-elem
                 'function  (convert-to-ir function)
                 'arguments (mapcar #'convert-to-ir args)))

(defun compile-ir (ir unificator)
  (declare (ignore ir unificator))
  nil
  )

(defmacro typel:deftypel (&rest declarations)
  (let ((declarations
         ;; Convert ((f a) ...) into (f (lambda (a) ...))
         (mapcar #'convert-to-lambda-form
                 declarations))
        (own-type-table (make-hash-table :test 'eq)))
    ;; Fill namespace with preliminary data (generic types are
    ;; assigned to all variables)
    (loop :for decl :in declarations :do
          (let* ((id-type (fresh-generic-type))
                 (id-info (make-id-info
                          :name (first decl)
                          :type-info (simple-type-info id-type))))
            (setf (lookup-info (first decl) *namespace*) id-info)
            (setf (gethash (first decl) own-type-table) id-type)))
    (let* ((ir-list      ; List of internal representations
            (loop :for decl :in declarations
                  :collect (convert-to-ir (second decl))))
           (unificator   ; Type substitutions
            (typel-unification:unify
             (nconc
              ;; Type of variable equals to type of expression
              (loop :for decl :in declarations
                    :for ir   :in ir-list
                    :nconc
                    (multiple-value-bind (type asserts)
                        (type-description ir)
                      (list* (cons type (gethash (first decl) own-type-table))
                             asserts)))))))
      ;; Replace preliminary types with found ones
      (loop :for decl :in declarations :do
            (setf (gethash (first decl) own-type-table)
                  (reduce #'typel-types::subst-obj
                          unificator
                          :initial-value (gethash (first decl)
                                                  own-type-table))))
      ;; Printing types
      (loop :for decl :in declarations
            :for ir   :in ir-list :do
            (format t "~S: ~S~%"
                    (first decl)
                    (gethash (first decl) own-type-table)))
      ;; Produce a code
      (cons 'progn
            (append
             (loop :for decl :in declarations
                  :for ir   :in ir-list
                  :collect
                  `(define-symbol-macro ,(car decl) (get-value ',(car decl))))
             (loop :for decl :in declarations
                  :for ir   :in ir-list
                  :collect
                  `(setf (get-value ',(car decl)) ,(compile-to-lisp ir)))
             '((values)))))))

(defmacro typel:typel (declarations form)
  "FORM is processed as TypeL code"
  (with-new-namespace
      (dolist (decl declarations)
        (setf (lookup-info (first decl) *namespace*)
              (make-id-info
               :name (first decl)
               :type-info (simple-type-info
                      (typel-types:parse-type (second decl))))))
    (let ((ir (convert-to-ir form)))
      (multiple-value-bind (type asserts) (type-description ir)
        (declare (ignore type))
        (typel-unification:unify asserts)
        (compile-to-lisp ir)))))


;;;;;;
;;;
;;;  Algebraic types
;;;
;;;  Information is stored in property lists

(defconstant +type-variant-propery+ '+type-propery+)
(defconstant +alg-type-info+        '+alg-type-info+)

(defun type-of-variant (variant-symbol)
  (get variant-symbol +type-variant-propery+))

(defun (setf type-of-variant) (value variant-symbol)
  (setf (get variant-symbol +type-variant-propery+) value))

(defun alg-type-info (type-symbol)
  (get type-symbol +alg-type-info+))

(defun (setf alg-type-info) (value type-symbol)
  (setf (get type-symbol +alg-type-info+) value))

;;;
;;;  Sample:
;;;
;;;  (defstype wtree (x)
;;;     (node 'x integer (:alg tree x) (:alg tree x))
;;;     (leaf))

(defmacro defstype (name args . variants)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (let ((at (make-algebraic-type-info
               ',name 
               (mapcar #'(lambda (arg)
                           (cons arg (fresh-generic-type)))
                       ',args)
               ',variants)))
      (setf (alg-type-info ',name) at)
      (dolist (variant ',variants)
        (setf (type-of-variant (first variant)) at))
      ',name)))
