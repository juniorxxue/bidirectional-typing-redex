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

(test-equal (term (open-term ((lambda base (1 0)) 0) Y)) (term ((lambda base (Y 0)) Y)))


;; ---- Local Closure ----

(define-metafunction L
  fv-rec : e (x ...) -> (x ...)
  [(fv-rec number (x ...)) ()]
  [(fv-rec x_1 (x_2 ...)) (x_1)]
  [(fv-rec (lambda tau e_1) (x_1 ...)) (fv-rec e_1 (x_1 ...))]
  [(fv-rec (e_1 e_2) (x ...)) ,(append (term (fv-rec e_1 (x ...)))
                                     (term (fv-rec e_2 (x ...))))])

;; compute the list of free variables in a term
(define-metafunction L
  fv : e -> (x ...)
  [(fv e) ,(remove-duplicates (term (fv-rec e ())))])

(test-equal (term (fv ((lambda base Y) (lambda base Y)))) '(Y))


;; (define-metafunction L
;;   not-in : any (any ...) -> boolean
;;   [(not-in any_1 (_ ... any_1 _ ...)) #f]
;;   [(not-in _ _) #t]
;;   )

(define-judgment-form L
  #:mode (not-in I I)
  #:contract (not-in any (any ...))
  [(not-in any_1 (_ ... any_1 _ ...))]
  )

(define-judgment-form L
  #:mode (lc I)
  #:contract (lc e)
  [--------------- "lc-var"
   (lc x)]
  ;; [(not-in x (fv e))
  ;;  (lc (open-term e x))
  ;;  ----------------- "lc-abs"
  ;;  (lc (lambda tau e))]

  ;; I need to make this rule algorithmic

  [(lc (open-term e NEVER))
   ------------------------- "lc-abs"
   (lc (lambda tau e))]
  [(lc e_1)
   (lc e_2)
   ------------------ "lc-app"
   (lc (e_1 e_2))]
  )
