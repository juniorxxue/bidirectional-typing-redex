#lang racket
(require redex)

(define-language L
  (x ::= variable-not-otherwise-mentioned)
  (tau ::= base (tau -> tau))
  (e ::= number x (lambda tau e) (e e)) ;; number for bounded variables, x for free variables
  )

(default-language L)

;; \x:b -> b -> b.\y:b -> b.\z:b. x z (y z)
(define demo-1 (term (lambda (base -> (base -> base))
                       (lambda (base -> base)
                         (lambda base
                           ((2 0) (1 0)))))))


;; substitution that replace a free variable with a term
(define-metafunction L
  subst : x e e -> e ;; match on e
  [(subst x e number) number]
  [(subst x e x) e] ;; it matches free-var x
  [(subst x_1 e x_2) x_2] ;; it doesn't match
  [(subst x e_1 (lambda tau e_2)) (lambda tau (subst x e_1 e_2))]
  [(subst x e_1 (e_2 e_3)) ((subst x e_1 e_2) (subst x e_1 e_3))]
  )

(test-equal (term (subst Y Z (lambda base (0 Y)))) (term (lambda base (0 Z))))
