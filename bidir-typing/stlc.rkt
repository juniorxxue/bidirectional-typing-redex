#lang racket
(require redex)

(define-language L
  (x ::= variable-not-otherwise-mentioned)
  (tau ::= int (tau -> tau))
  (e ::= x number (e : tau) (lambda x e) (e e))
  (v ::= number ((lambda x e) : (tau -> tau)))
  (i ::= number (e_1 e_2))
  (Gamma ::= empty (Gamma comma x : tau))
  (mode ::= => <=) ;; infer and check
  #:binding-forms
  (lambda x e #:refers-to x)
  )

(define-metafunction L
  lookup : Gamma x -> tau or #f
  [(lookup (Gamma comma x : tau) x) tau]
  [(lookup (Gamma comma x_1 : tau) x_2) (lookup Gamma x_2)]
  [(lookup empty x) #f])

(define-judgment-form L
  #:mode (infer I I I O)
  #:contract (infer Gamma e => tau)
  [------------------------------ "typing-int"
   (infer Gamma number => int)]
  [(where tau (lookup Gamma x))
   ------------------------------ "typing-var"
   (infer Gamma x => tau)]
  [(check Gamma e <= tau)
   ------------------------------ "typing-ann"
   (infer Gamma (e : tau) => tau)]
  [(infer Gamma e_1 => (tau_1 -> tau_2))
   (check Gamma e_2 <= tau_1)
   ------------------------------ "typing-app"
   (infer Gamma (e_1 e_2) => tau_2)]
  )

(define-judgment-form L
  #:mode (check I I I I)
  #:contract (check Gamma e <= tau)
  [(infer Gamma e => A)
   ------------------------------ "typing-chk"
   (check Gamma e <= A)]
  [(infer (Gamma comma x : tau_1) e => tau_2)
   ------------------------------ "typing-abs"
   (check Gamma (lambda x e) <= (tau_1 -> tau_2))]
  )


(define step
  (reduction-relation
   L
   #:domain e
   #:codomain e
   (--> (((lambda x e) : (tau_1 -> tau_2)) v)
        ((substitute e x v) : tau_2)
        "beta")
   (--> (((lambda x e_1) : (tau_1 -> tau_2)) (e_2 : tau_3))
        (((lambda x e_1) : (tau_1 -> tau_2)) e_2)
        "beta_ann")
   (--> (((lambda x e_1) : (tau_1 -> tau_2)) (lambda x e_2))
        ((substitute e_1 x ((lambda x e_2) : tau_1)) : tau_2)
        "beta_abs")
   (--> (e_1 e_2) (e_3 e_2)
        (side-condition (not (equal? '() (apply-reduction-relation step (term e_1)))))
        (where e_3 ,(first (apply-reduction-relation step (term e_1))))
        "app-l")
   (--> (v i_1) (v i_2)
        (side-condition (not (equal? '() (apply-reduction-relation step (term i_1)))))
        (where i_2 ,(first (apply-reduction-relation step (term i_1))))
        "app-r")
   (--> ((e : tau_1) : tau_2)
        (e : tau_2)
        "anno-anno")
   (--> (number : tau)
        number
        "anno-num")
   (--> ((e_1 e_2) : tau)
        (e_1 e_2)
        "anno-app")
   ))


(define multi-step (compatible-closure step L e))

(test-->> multi-step (term (((lambda x x) : (int -> int)) (1 : int))) (term 1))

(test-->>∃ multi-step (term (((lambda x x) : (int -> int)) (1 : int))) (term 1))

;; (traces multi-step (term (((lambda x x) : (int -> int)) (1 : int))))

;; found that app-l app-r actually don't make any real contribution to reduction
