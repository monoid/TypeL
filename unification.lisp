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

;;;  unification as solution of set of equations

;;;  We have list of equations and list of found solutions
;;; TODO: s/solution/substitution/g

(cl:defpackage #:typel-unification
  (:use #:cl)
  (:export
   #:resolve-equation
   #:substitute-solution
   #:reccurent-solution-p
   #:apply-solutions
   #:unify))

(cl:in-package "TYPEL-UNIFICATION")

(defgeneric resolve-equation (left-side right-side)
  (:documentation "Split equation into list of simplier and/or
 return list of found solutions"))

(defgeneric substitute-solution (equation solution)
  (:documentation "Substitute solution in unprocessed terms"))

(defgeneric reccurent-solution-p (solution)
  (:documentation "Check if solution is reccurent: X=f(X)"))

(defun unify (equation-list)
  (resolve-step equation-list nil))

(typel-runtime:defcurried apply-solutions (solution-list equation)
  "Apply solution-list to equation"
  (reduce
   #'(lambda (sol theeq)
       (substitute-solution sol theeq))
   solution-list
   :initial-value equation))

;;; TODO: keep substitutions in a hash
(defun resolve-step (equation-list solution-list)
  (if (null equation-list)
      solution-list
      (let ((equation (first equation-list)))
        (let ((left (car equation))
              (rigt (cdr equation)))
          (multiple-value-bind (new-equ new-sol)
              (if (eq left rigt) (values nil nil) (resolve-equation left rigt))
            (let ((recc-sol (find-if #'reccurent-solution-p new-sol)))
              (if recc-sol
                  (throw :reccurent-equation
                    (list :reccurent-equation recc-sol))
                  (let ((new-equation-list (nconc new-equ
                                                  (rest equation-list)))
                        (new-solution-list (nconc solution-list new-sol)))
                    (resolve-step
                     (if (and new-sol new-equation-list)
                         (map-into new-equation-list
                                   (apply-solutions new-sol)
                                   new-equation-list)
                         new-equation-list)
                     new-solution-list)))))))))
