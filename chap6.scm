; Return true if aexp is an arithmetic expression.
; Incorrect!
; ex: (numbered? '(3 * 3 + 4 * 4 + 5 * 5))
(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
    )
  )
)

; Return true if aexp is an arithmetic expression.
; Correct! As long as aexp is an arithmetic expression...
(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      ((null? (cdr aexp)) (number? (car aexp)))
      (else (and (numbered? (car aexp)) (numbered? (cdr (cdr aexp)))))
    )
  )
)

; Calculate the value of an arithmetic expression.
; Not quite correct - doesn't respect order of operations.
(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((null? (cdr nexp)) (value (car nexp)))
      ((eq? (car (cdr nexp)) '+) (plus (car nexp) (value (cdr (cdr nexp)))))
      ((eq? (car (cdr nexp)) '*) (mult (car nexp) (value (cdr (cdr nexp)))))
      ((eq? (car (cdr nexp)) '^) (^ (car nexp) (value (cdr (cdr nexp)))))
    )
  )
)

; Calculate the value of an arithmetic expression written in prefix notation.
(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (car nexp) '+) (plus (value (car (cdr nexp))) (value (car (cdr (cdr nexp))))))
      ((eq? (car nexp) '*) (mult (value (car (cdr nexp))) (value (car (cdr (cdr nexp))))))
      ((eq? (car nexp) '^) (^ (value (car (cdr nexp))) (value (car (cdr (cdr nexp))))))
    )
  )
)
