;number: ... -2, -1, 0, 1, 2 ...
;user-symbol: 'a, 'b, 'c, ... 'Aa 'Ab ...
;internal-symbol: 0, 1, 2, 3, 4...

;rule1: '(user-symbol, internal-symbol)
;rule2; '(internal-symbol, number)

;db; ((list of rule1s) (list of rule2s) next_internal_symbol)

(define (add_rule1 user_symbol internal_symbol rules)
  (let ((x (cons user_symbol (cons internal_symbol '()))))
    (cond
     ((eq? rules '()) (cons x '()))
     (#t
      (let
	  ((us (caar rules))
	   (is (cadar rules)))
	(cond
	 ((and (eq? user_symbol us)
	       (eq? internal_symbol is))
	  rules)
	 ((eq? user_symbol us) 'fail)
	 ((or (eq? internal_symbol is) 
	      (> internal_symbol is))
	  (cons (car rules) (add_rule1 user_symbol internal_symbol (cdr rules))))
	 ((< internal_symbol is)
	  (cons x rules))))))))

(define (testar2)
  ;(add_rule1 'b 2 (add_rule1 'a 1 '())))
  ;(add_rule1 'b 1 (add_rule1 'a 2 '())))
  (add_rule2 1 17 (add_rule2 2 27 '())))
      
(define (add_rule2 internal_symbol number rules)
  (let ((x `(,internal_symbol ,number)))
    (cond
     ((eq? rules '()) (cons x '()))
     (#t
      (let
	  ((is (caar rules))
	   (n (cadar rules)))
	(cond
	 ((and (eq? internal_symbol is)
		(eq? number n))
	       rules)
	 ((eq? internal_symbol is) 'fail)
	 ((> internal_symbol is)
	  (cons (car rules) (add_rule2 internal_symbol number (cdr rules))))
	 ((< internal_symbol is)
	  (cons x rules))))))))
	 
(define (lookup user_symbol rules)
  (cond
   ((eq? rules '()) 'none)
   ((eq? (caar rules) user_symbol) (cadar rules))
   (#t (lookup user_symbol (cdr rules)))))
(define (testlr1)
  (lookup 'd '()))
  ;(lookup_rule1 'd '((b 2)(c 3)(a 1)(g 4))))

(define (add_synonym user_symbol_1 user_symbol_2 next_internal_symbol rule1s)
  (let ((l1 (lookup user_symbol_1 rule1s))
	(l2 (lookup user_symbol_2 rule1s)))
    (cond
     ((and (eq? l1 'none)
	   (eq? l2 'none))
      (add_rule1 user_symbol_2 next_internal_symbol 
		(add_rule1 user_symbol_1 next_internal_symbol rule1s)))
     ((eq? l1 'none)
      (add_rule1 user_symbol_1 l2 rule1s))
     ((eq? l2 'none)
      (add_rule1 user_symbol_2 l1 rule1s))
     ((eq? l1 l2) rule1s)
     (#t 'fail))))
(define (testas)
  ;(add_synonym 'a 'b 3 '()))
  (add_synonym 'a 'b 3 '((a 2))))

(define (add_fact user_symbol_1 number rule1s rule2s next_internal_symbol)
  (let ((is1 (lookup user_symbol_1 rule1s)))
    (cond
     ((eq? is1 'none)
      (cons (add_rule1 user_symbol_1 next_internal_symbol rule1s)
	    (cons (add_rule2 next_internal_symbol number rule2s)
		  (cons (+ 1 next_internal_symbol) '()))))
     (#t
      (let ((n (lookup is1 rule2s)))
	(cond
	 ((eq? n 'none)
	  (cons rule1s (cons (add_rule2 is1 number rule2s)
			     (cons next_internal_symbol '()))))
	 ((eq? n number)
	  (cons rule1s (cons rule2s (cons next_internal_symbol '()))))
	 (#t 'fail)))))))
(define (testaf)
  ;(add_fact 'a 3 '() '() 1))
  (add_fact 'a 3 '((a 2)) '() 1))
  ;(add_fact 'a 3 '((a 2)) '((2 2)) 1))

(define (atom? x)
  (and (not (pair? x))
       (not (null? x))
       (not (number? x))))
;rule: '(symbol, number)
(define (add_rule R rule1s rule2s next_internal_symbol)
  ;checks if it is a fact or a synonym, calls accordingly
  (cond
   ((atom? (cadr R)) `(,(add_synonym (car R) (cadr R) next_internal_symbol rule1s) 
		       rule2s next_internal_symbol))

   (#t (add_fact (car R) (cadr R) rule1s rule2s next_internal_symbol))))
(define (testar)
  ;(add_rule '(a 1) '() '() 1))
  (add_rule '(a 1) '((a 2)) '() 1))
  ;(add_rule '(a 1) '((a 2)) '((2 2)) 1))
  ;(add_rule '(a 1) '((a 2)) '((2 1)) 1))
  ;(add_rule '(a b) '((a 2)) '((2 1)) 1))

(define (empty_db) '(() () 1))

(define (rules_helper code db)
  (cond
   ((null? code) db)
   ((and (null? (caar code))
	 (null? (cadar code)))
    db)
   ((and (null? (caar code))
	 (list? (caar code)))
    'fail)
   ((and (null? (cadar code))
	 (list? (caar code)))
    'fail)
   ((list? (caar code)) ;code (((x) (y))) -> ((x y) (()()))
    (let ((x (cons `(,(caaar code) ,(caadar code))
		  (cons `(,(cdaar code) ,(cdadar code))
			(cdr code)))))
      (rules_helper x db)))
   (#t
    (rules_helper (cdr code) 
		  (add_rule (car code) (car db) (cadr db) (caddr db))))))

(define (testr)
  (rules_helper '(((A B) (2 3))) (empty_db)))

(define (get user_symbol rule1s rule2s)
  (let ((x (lookup user_symbol rule1s)))
    (cond
     ((eq? x 'none) 'none)
     (#t (lookup x rule2s)))))

(define (testg)
  (get 'a '((b 2)(c 2)(a 3)) '((2 3)(3 4))))

(define (replace_vars code db)
  (cond
   ((null? code) '())
   ((list? code) (cons (replace_vars (car code) db)
		       (replace_vars (cdr code) db)))
   (#t (let ((s (get code (car db) (cadr db))))
	  (cond
	   ((eq? s 'none) code)
	   (#t s))))))
(define (testrv)
  (replace_vars '(a (b a)) '(((a 1)(b 2))((1 26)(2 11)))))
(define (run_helper code db)
  (cond
   ((null? code) db)
   (#t (run_helper (cdr code) (rules_helper `((,(caar code),(eval (replace_vars(cadar code)db)))) db)))))
(define (run code) (run_helper code (empty_db)))
(define (testrun) (run '((a 1)(b 2))))
(define (runget code var)
  (let ((v (run code)))
    (cond
     ((eq? 'fail v) 'fail)
     (#t (get var (car v) (cadr v))))))

(define (sum a b)
  (runget `((c (+ ,a ,b))
	    (d (+ c ,a)))
	  'd))
(define (break)
  (runget `((a 1) (a 2)) 'a))
(define (testrg)
  (let ((a 1)(b 2))
    (runget `((a ,a)
	      (b ,b)
	      (c (+ a b)))
	    'c)))

(define (testcar)
  (runget '((a 1)(b 2)((c d) (cons 1 (cons 2 ())))) 'c))

(define (test)
  (runget `(((a b (c d)) '(1 2 (3 4)))
	    (e (+ a b c d)))
	  'e)))
