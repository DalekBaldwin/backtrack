(in-package :backtrack-test)

(in-root-suite)

(defsuite* test-all)

(deftest all-equal ()
  (let ((n 100))
    (is
     (and
      (equal
       (backtrack-throw::pyth-triples n)
       (backtrack-cond::pyth-triples n))
      (equal
       (backtrack-throw::pyth-triples n)
       (backtrack-wrap::pyth-triples n))))))
