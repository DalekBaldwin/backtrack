(in-package :backtrack-wrap)

(defclass wrapped-condition () ())

(defmacro wrapped-handler-case (form &rest cases)
  "An analogue of HANDLER-CASE built on THROW and CATCH instead of the standard
condition system."
  (with-gensyms (result result-type)
    `(let* ((,result
             (catch 'wrapped-transfer
               ,form))
            (,result-type (type-of ,result)))
       (cond
         ((not (subtypep ,result-type 'wrapped-condition))
          ,result)
         ,@(loop for (typespec (var) . body) in cases
              collect `((subtypep ,result-type ',typespec)
                        (let ,(when var `((,var ,result)))
                          (progn ,@body))))
         (t (throw 'wrapped-transfer ,result))))))

(defun wrapped-error (&optional (datum 'wrapped-condition) &rest arguments)
  "An analogue of ERROR meant to be caught by WRAPPED-HANDLER-CASE."
  (throw 'wrapped-transfer
    (apply #'make-instance datum arguments)))

(defclass success (wrapped-condition)
  ((value :initarg :value)))

(defclass failure (wrapped-condition)
  ((note :initarg :note)))

(defvar *solutions*)
(defvar *mode*)

(defun success (&rest args)
  "We found a solution.  Either return it, or add it onto the list of solutions
depending on the value of *MODE* \(as set by WITH-BACKTRACKING)."
  (cond ((eql *mode* 'find-one)
         (wrapped-error 'success :value args) )
        ((eql *mode* 'find-all)
         (push args *solutions*)
         (wrapped-error 'failure :note args) )))

(defun fail (&rest args)
  "For whatever reason, this has failed.  Backtrack."
  (wrapped-error 'failure :note args) )

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
                    (wrapped-handler-case (bt-let* ,rest-bindings ,@body)
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
     (wrapped-handler-case
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
