(assert-equal
  (lets g (numer-gen 1 3 1)
    (g #f) (g #f))
  2)

(assert-equal
  (lets g (numer-gen 1 -10 -3)
    (g #f) (g #f) (g #f) (g #f) (g (d-fun () #f)))
  #f)

(assert-equal
  (lets g (make-list-gen '(1 2 3))
    (g (d-fun () '())) (g (d-fun () '())))
  2)

(assert-equal
  (let ((x 5) (g (make-list-gen '())))
    (g (l-fun () (set! x 15)))
    x)
  15)
 
(assert-equal
  (iter (for x in '(1 2 3)) (if (equal? x 2) (leave x)))
  2)

(assert-equal
  (iter (generating x in '(1 2 3))
    (leave (next 'x)))
  2)

(assert-equal
  (lets x 3
    (iter (repeat 3) (set! x (+ x 1)))
    x)
  6)

(assert-equal
  (lets x 5
    (iter (for y in '(1 2 3 4 5))
      (initially (set! x 10))
      (if (equal? y 3) (finish))
      (finally x)))
  10)

