(define add1 (lambda (n) (+ n 1)))

(define sub1 (lambda (n) (- n 1)))

(define plus
  (lambda (a b)
    (cond
      ((zero? b) a)
      (else (add1 (plus a (sub1 b)))))))

(define minus
  (lambda (a b)
    (cond
      ((zero? b) a)
      (else (sub1 (minus a (sub1 b)))))))

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (plus (car tup) (addtup (cdr tup)))))))

(define mult
  (lambda (a b)
    (cond
      ((zero? b) 0)
      (else (plus a (mult a (sub1 b)))))))

(define tupsum
  (lambda (t1 t2)
    (cond
      ((null? t1) t2)
      ((null? t2) t1)
      (else (cons (plus (car t1) (car t2)) (tupsum (cdr t1) (cdr t2)))))))

(define gt
  (lambda (a b)
    (cond
      ((zero? a) #f)
      ((zero? b) #t)
      (else (gt (sub1 a) (sub1 b))))))

(define lt
  (lambda (a b)
    (cond
      ((zero? b) #f)
      ((zero? a) #t)
      (else (lt (sub1 a) (sub1 b))))))

(define eq
  (lambda (a b)
    (and (not (gt a b)) (not (lt a b)))))

(define ^
  (lambda (b e)
    (cond
      ((zero? e) 1)
      (else (mult b (^ b (sub1 e)))))))

(define div
  (lambda (a b)
    (cond
      ((lt a b) 0)
      (else (add1 (div (minus a b) b))))))

(define length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length (cdr lat)))))))

(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (a lat)
    (cond
      ((zero? (sub1 a)) (cdr lat))
      (else (cons (car lat) (rempick (sub1 a) (cdr lat)))))))

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) lat)
      ((number? (car lat)) (no-nums (cdr lat)))
      (else (cons (car lat) (no-nums (cdr lat)))))))

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) lat)
      ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
      (else (all-nums (cdr lat))))))

(define eqan?
  (lambda (a b)
    (cond
      ((and (number? a) (number? b)) (eq a b))
      ((or (number? a) (number? b)) #f)
      (else (eq? a b)))))

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((eq? a (car lat)) (add1 (occur a (cdr lat))))
      (else (occur a (cdr lat))))))

(define one?
  (lambda (n)
    (eq n 1)))

(define rempick2
  (lambda (a lat)
    (cond
      ((one? a) (cdr lat))
      (else (cons (car lat) (rempick (sub1 a) (cdr lat)))))))
