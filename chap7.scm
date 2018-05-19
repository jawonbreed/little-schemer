; Returns true if lat is a set.
(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set (cdr lat)))
    )
  )
)

; Returns lat with all duplicate members removed.
; written with member?
(define makeset
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
      (else (cons (car lat) (makeset (cdr lat))))
    )
  )
)

; written with multirember
(define makeset
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else (cons (car lat) (makeset (multirember (car lat) (cdr lat)))))
    )
  )
)

; Returns true if s1 is a subset of s2.
(define subset?
  (lambda (s1 s2)
    (cond
      ((null? s1) #t)
      ((member? (car s1) s2) (subset? (cdr s1) s2))
      (else #f)
    )
  )
)

; Returns true if s1 and s2 contain exactly the same items.
(define eqset?
  (lambda (s1 s2)
    ((subset? s1 s2) and (subset? s2 s1))
  )
)

; Returns true if s1 and s2 share at least one atom.
(define intersect?
  (lambda (s1 s2)
    (cond
      ((null? s1) #f)
      ((member? (car s1) s2) #t)
      (else (intersect? (cdr s1) s2))
    )
  )
)

; Returns the intersection between two sets s1 and s2.
(define intersect
  (lambda (s1 s2)
    (cond
      ((null? s1) '())
      ((member? (car s1) s2) (cons (car s1) (intersect (cdr s1) s2)))
      (else (intersect (cdr s1) s2))
    )
  )
)

; Returns the union of two sets s1 and s2.
(define union
  (lambda (s1 s2)
    (cond
      ((null? s1) s2)
      ((member? (car s1) s2) (union (cdr s1) s2))
      (else (cons (car s1) (union (cdr s1) s2)))
    )
  )
)

; Returns the intersection of all sets in a given list of sets.
(define intersectall
  (lambda (sets)
    (cond
      ((null? (cdr sets)) (car sets))
      (else (intersect (car sets) (intersectall (cdr sets))))
    )
  )
)

; Returns true iff x is a list with two items.
(define a-pair?
  (lambda (x)
    (cond
      ((atom? x) #f)             ; not a list
      ((null? x) #f)             ; empty list
      ((null? (cdr x)) #f)       ; list with 1 item
      ((null? (cdr (cdr x))) #t) ; list with 2 items
      (else #f)                  ; list with 3 or more items
    )
  )
)

; Returns the first item of a pair.
(define first
  (lambda (p)
    (car p)
  )
)

; Returns the second item of a pair.
(define second
  (lambda (p)
    (car (cdr p))
  )
)

; Returns the two items as a pair.
(define build
  (lambda (a b)
    (cons a (cons b '()))
  )
)

; Returns true if l is a relation, or a set of pairs.
(define rel?
  (lambda (l)
    (cond
      ((null? l) #t)
      (
