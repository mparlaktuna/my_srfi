;; Test file for SRFI1

(use-modules (srfi srfi-64))

(define (my-simple-runner)
  (let ((runner (test-runner-null))
        (num-passed 0)
        (num-failed 0))
    (test-runner-on-test-end! runner
      (lambda (runner)
        (case (test-result-kind runner)
          ((pass xpass) (set! num-passed (+ num-passed 1)))
          ((fail xfail)
	   (begin
	     (format #t "Expected ~a Actual ~a \n\n"
		     (test-result-ref runner 'expected-value)
		     (test-result-ref runner 'actual-value))
	     (set! num-failed (+ num-failed 1))))
          (else #t))))
    (test-runner-on-final! runner
       (lambda (runner)
          (format #t "Passing tests: ~d.~%Failing tests: ~d.~%"
                  num-passed num-failed)))
    runner))

(test-runner-factory
 (lambda () (my-simple-runner)))

(load "srfi1.scm")

(test-begin "SRFI1")
(test-equal '(#f #f) (make-list 2))
(test-equal '(10 10 10) (make-list 3 10))
(test-equal '(2 3 4) (list-tabulate 3 (lambda (x) (+ x 2))))
(test-equal '1 (cons* 1))
(test-equal '(1 . 2) (cons* 1 2))
(test-equal '(1 3 . 2) (cons* 1 3 2))
(test-equal '(1 3 2) (list-copy (list 1 3 2)))
(test-equal '(10 22) (list-copy (list 10 22)))
(test-equal '(0 1 2) (itoa 3))
(format #t "~a\n" (circular-list 1 2))
(test-eq #t (proper-list? '(1)))
(test-eq #f (proper-list? (circular-list 1 2 )))
(test-end "SRFI1")
