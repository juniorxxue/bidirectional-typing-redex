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


;; substitution that replaces a free variable with a term
(define-metafunction L
  subst : x e e -> e ;; match on e
  [(subst x e number) number]
  [(subst x e x) e] ;; it matches free-var x
  [(subst x_1 e x_2) x_2] ;; it doesn't match
  [(subst x e_1 (lambda tau e_2)) (lambda tau (subst x e_1 e_2))]
  [(subst x e_1 (e_2 e_3)) ((subst x e_1 e_2) (subst x e_1 e_3))]
  )

(test-equal (term (subst Y Z (lambda base (0 Y)))) (term (lambda base (0 Z))))


;; substitution that replaces a index with a term
(define-metafunction L
  open-rec : number e e -> e
  [(open-rec number e number) e] ;; match
  [(open-rec number_1 e number_2) number_2] ;; doesn't match
  [(open-rec number e x) x]
  [(open-rec number e_1 (lambda tau e_2)) (lambda tau (open-rec ,(+ (term number) 1) e_1 e_2))]
  [(open-rec number e_1 (e_2 e_3)) ((open-rec number e_1 e_2) (open-rec number e_1 e_3))])

(define-metafunction L
  open-term : e e -> e
  [(open-term e_1 e_2) (open-rec 0 e_2 e_1)])


(term (open-term ((lambda base (1 0)) 0) Y))

(test-equal (term (open-term ((lambda base (1 0)) 0) Y)) (term ((lambda base (Y 0)) Y)))


;; ---- Local Closure ----
