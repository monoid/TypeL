;;;;;  This file is Public Domain

;;;;;;
;;;
;;;  Some samples

(common-lisp:in-package "COMMON-LISP-USER")

;;;  Expression inside typel:typel is type-checked.
(defun test (a b)
  (declare (type integer a b))
  (typel:typel ((a integer) (b integer))
     (typel:+ a b)))

(test 1 2)
;;; => 3

(in-package "TYPEL-USER")

;;; Lists
;;; MAPCAR: ('TYPE31 -> 'TYPE28) -> (:LIST 'TYPE31) -> (:LIST 'TYPE28)
(deftypel ((mapcar fun list)
            (if (equal nil list)
                nil
                (cons (fun (first list))
                      (mapcar fun (rest list))))))

;;; Curring: (+ 2) has type INTEGER -> INTEGER, because
;;; + has type INTEGER -> INTEGER -> INTEGER.
(typel ()
  (mapcar (+ 2) (cons 1 (cons 2 nil))))
;;; => (3 4)

;;; Lambda expression.  Don't use #' for lambdas.
(typel ()
  (mapcar (lambda (x) (cons x nil)) (cons 1 (cons 2 nil))))
;;; => ((1) (2))

;;;  This example demonstrates LETREC and curring
;;;  FACT: INTEGER -> INTEGER
(deftypel
    (fact ((letrec (((fact-acc acc n)
                     (if (0? n)
                         acc
                         (fact-acc (* acc n) (- n 1)))))
                   (fact-acc 1)))))

;;; Floating point math
;;; EXPONENTA: FLOAT -> INTEGER -> FLOAT
(deftypel
    ((exponenta x n)
     (letrec (((exp-acc e p fact i)
               (if (= i n)
                   e
                   (let ((new-fact (* fact i)))
                     (exp-acc (+. e (/. (*. p x) (int-to-float new-fact)))
                              (*. p  x)
                              new-fact
                              (+ i 1))))))
             (exp-acc 1.0 1.0 1 1))))

;;; Usage
(typel ()
  (exponenta 1.0 100))
;;; => 2.7182818284590455d0

(typel ()
  (exponenta 2.0 100))
;;; => 7.389056098930649d0

;;; Do not try to call EXPONENTA directly:
;;; (exponenta 2.0 100) ; <- will fail
