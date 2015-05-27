(in-package :backtrack-throw)

(defvar *success*)
 
(defun success (&rest args)
  (apply *success* args))
 
(defun fail ()
  (throw 'backtrack nil))
 
(defmacro bt-let* (bindings &body body)
  "Like LET*, but if you find a special nondeterministic choice form like ONE-OF
or ONE-IN, treat it specially by setting up the framework for nondeterministic
search."
  (let ((option-list (gensym))
        bt-var rest-bindings)
    `(let* ,(iter (for (binding . rest) on bindings)
                  (until bt-var)
                  (cond ((and (consp binding)
                              (consp (second binding))
                              (eql 'one-of (first (second binding))))
                         (setf bt-var (first binding)
                               rest-bindings rest)
                         (collect (list option-list
                                        (cons 'list (rest (second binding))))))
                        ((and (consp binding)
                              (consp (second binding))
                              (eql 'one-in (first (second binding))))
                         (setf bt-var (first binding)
                               rest-bindings rest)
                         (collect (list option-list
                                        (second (second binding)))))
                        (t (collect binding))))
       ,(if bt-var
            `(labels
                 ((try-with (,bt-var)
                    (block nil
                      (catch 'backtrack
                        (return (bt-let* ,rest-bindings ,@body)))
                      (if ,option-list
                          (try-with (pop ,option-list))
                          (throw 'backtrack nil)))))
               (try-with (pop ,option-list)))
            `(progn ,@body)))))

(defmacro with-backtracking ((mode) &body body)
  "Set up the environment where backtracking can be performed.  MODE can be set
as one of FIND-ONE or FIND-ALL in order to specify where just the first or all
possible solutions should be returned."
  (let ((success (gensym "SUCCESS"))
        (solutions (gensym "SOLUTIONS")))
    `(let (,@(when (eq 'find-all mode)
               `(,solutions)))
       (block nil
         (flet ((,success (&rest args)
                  ,@(ecase mode
                     (find-one
                      `((return args)))
                     (find-all
                      `((push args ,solutions)
                        (throw 'backtrack nil))))))
           (let ((*success* #',success))
             (catch 'backtrack
               ,(if (eq 'find-one mode)
                    `(return (progn ,@body))
                    `(progn ,@body)))
             ,(if (eq 'find-one mode)
                  `(error "No solutions found.")
                  `,solutions)))))))
 
(defun pyth-triples (n)
  (with-backtracking (find-all)
    (bt-let* ((a (one-in (iter (for i from 1 below n) (collect i))))
              (b (one-in (iter (for i from 1 below n) (collect i))))
              (c (one-in (iter (for i from 1 below n) (collect i)))) )
      (if (= (+ (* a a) (* b b)) (* c c))
          (success (list a b c))
          (fail)))))
