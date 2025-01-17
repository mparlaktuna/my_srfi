;; SRFI1 IMplementation by Mustafa Parlaktuna

(use-modules (ice-9 optargs))

(define (make-list len . el)
  (let ((el (cond ((null? el) #f)
		  ((null? (cdr el)) (car el))
		  (else (error "Too many arugments to make-list")))))
    (do ((i len (- i 1))
	 (ans '() (cons el ans)))
	((<= i 0) ans))))

(define (list-tabulate len proc)
  (do ((i (- len 1) (- i 1))
       (ans '() (cons (proc i) ans)))
      ((< i 0) ans)))

(define (cons* first . rest)
  (let loop ((f first) (r rest))
    (if (null? rest)
	r
	(cons r (loop (car rest) (cdr rest))))))

(define (list-copy l)
  (let loop ((l l))
    (if (null? l)
	l
	(cons (car l) (loop (cdr l))))))

(define (itoa count . r)
  (let-optional r ((start 0) (step 1))
		(let loop ((n 0)
			   (s start)
			   (r '()))
		  (if (= n count)
		      (reverse r)
		      (loop (+ 1 n)
			    (+ s step)
			    (cons s r))))))

(define (circular-list val1 . vals)
  (let ((ans (cons val1 vals)))
    (set-cdr! (last-pair ans) ans)
    ans))

(define (proper-list? x)
  (let lp ((x x) (l x))
    (if (pair? x)
	(let ((x (cdr x)))
	  (if (pair? x)
	      (let ((x (cdr x))
		    (l (cdr l)))
		(and (not (eq? l x)) (lp x l)))
	      (null? x)))
	(null? x))))
