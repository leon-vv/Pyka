(assert-equal (append '() '() '()) '())
(assert-equal (append) '())
(assert-equal (append '() '(1 2 3) '() '(4 5)) '(1 2 3 4 5))

(assert-equal (+ 1 2) 3)
(assert-equal (last '(1 2 3)) 3)

(assert-equal (last '(1)) 1)
(assert-equal (begin 1 2 3) 3)

(assert-equal (quote (1 2 3)) '(1 2 3))
(assert-equal `(1 2 ,@(list 3 4)) (list 1 2 3 4))
(assert-equal `(1 2 ,(+ 1 2)) (list 1 2 3))
(assert-equal 
  (let ((name 'x))
    (quasiquote (let (((unquote name) 10)) (unquote name))))
    '(let ((x 10)) x))
(assert-equal
  (let ((a 1) (b 2) (c 3))
    `(,a ,b ,c))
  '(1 2 3))
(assert-equal
  (let ((a 1) (b 2) (c 3))
    `(a ,b `(,b ,,@x ,,(+ 1 ,,@y))))
  '(a 2 (quasiquote
          (2 (unquote-splicing x)
             (unquote (+ 1 (unquote (unquote-splicing y))))))))

(assert-equal
  (eval 
    `(quasiquote ,(cons 1 2))
    (get-env))
  (cons 1 2))

(assert-equal ((counter 5)) 5)
(assert-equal (let ((c (counter 0)))
                (c)
                (c)
                (c)) 2)
(assert-equal 
  (let ((x 0))
       (map (l-fun (v) (set! x (+ x 1)) x) '(0 0 0)))
  '(1 2 3))

(assert-equal
  (cond (#t 10)) 10)

(assert-equal
  (cond (#f) (else (+ 1 2))) 3)

(assert-equal
  (cond ((equal? 5 5) 10))
  10)

(assert-equal
  (cond
    ((and (equal? 5 5) (< 10 5)) 1)
    ((or (equal? 5 5) (< 10 5)) 2))
  2)

(assert-equal
  (begin
    (let ((pat '(+ 1 1)))
      (cond
        ((and (list? pat) (not (null? pat))) (+ 10 10)))))
  20)

(assert-equal
  (case 10
    ((10) 5)) 5)
(assert-equal
  (case 'abc
    ((def abc) 15)) 15)
(assert-equal
  (case 'abc
    ((abc) 5)
    (else 30)) 5)
 
(assert-equal
    ((macro (x) x) (+ 1 1))
    2)

(assert-equal
  (begin
    (call/cc (d-fun (finish) (finish)))
    10)
  10)

(assert-equal
  (+ 2 (with/cc r (r 5)))
  7)
