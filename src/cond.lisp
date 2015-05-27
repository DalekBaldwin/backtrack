(in-package :backtrack-cond)

;; Define a parent type for flexibility...
(define-condition backtracking-condition () ())
;; NOTE can hold some data about how it failed
(define-condition failure (backtracking-condition) ((note :initarg :note)))
;; VALUE holds the solution
(define-condition success (backtracking-condition) ((value :initarg :value)))
 
(defun fail (&rest args)
  "For whatever reason, this has failed.  Backtrack."
  (signal 'failure :note args) )
 
(defvar *solutions*)
 
(defvar *mode*)
 
(defun success (&rest args)
  "We found a solution.  Either return it, or add it onto the list of solutions
depending on the value of *MODE* \(as set by WITH-BACKTRACKING)."
  (cond ((eql *mode* 'find-one)
         (signal 'success :value args) )
        ((eql *mode* 'find-all)
         (push args *solutions*)
         (signal 'failure :value args) )))

(defmacro bt-let* (bindings &body body)
  "Like LET*, but if you find a special nondeterministic choice form like ONE-OF
or ONE-IN, treat it specially by setting up the framework for nondeterministic
search."
  (let (bt-var
        (option-list (gensym))
        rest-bindings )
    `(let* ,(iter (for (binding . rest) on bindings)
              (until bt-var)
              (cond ((and (consp binding)
                          (consp (second binding))
                          (eql 'one-of (first (second binding))) )
                     (setf bt-var (first binding)
                           rest-bindings rest )
                     (collect (list option-list
                                    (cons 'list (rest (second binding))) )))
                    ((and (consp binding)
                          (consp (second binding))
                          (eql 'one-in (first (second binding))) )
                     (setf bt-var (first binding)
                           rest-bindings rest )
                     (collect (list option-list
                                    (second (second binding)) )))
                    (t (collect binding)) ))
       ,(if bt-var
            `(labels
                 ((try-with (,bt-var)
                    (handler-case (bt-let* ,rest-bindings ,@body)
                      (failure ()
                        (if ,option-list
                            (try-with (pop ,option-list))
                            (fail) )))))
               (try-with (pop ,option-list)) )
            `(progn ,@body) ))))

(defmacro with-backtracking ((mode) &body body)
  "Set up the environment where backtracking can be performed.  MODE can be set
as one of FIND-ONE or FIND-ALL in order to specify where just the first or all
possible solutions should be returned."
  `(let ((*mode* ',mode)
         *solutions* )
     (handler-case
         (progn ,@body)
       (failure ()
         (cond ((eql 'find-one *mode*)
                (error "No solutions found.") )
               ((eql 'find-all *mode*)
                *solutions* )))
       (success (cond)
         (slot-value cond 'value) ))))
 
(defun pyth-triples (n)
  (with-backtracking (find-all)
    (bt-let* ((a (one-in (iter (for i from 1 below n) (collect i))))
              (b (one-in (iter (for i from 1 below n) (collect i))))
              (c (one-in (iter (for i from 1 below n) (collect i)))) )
      (if (= (+ (* a a) (* b b)) (* c c))
          (success (list a b c))
          (fail) ))))
