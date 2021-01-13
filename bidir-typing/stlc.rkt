#lang racket
(require redex)

(define-language L
  (x ::= variable-not-otherwise-mentioned)
  (tau ::= int (tau -> tau))
  (e ::= x number (e : tau) (lambda x e) (e e))
  (v ::= number ((lambda x e) : (tau -> tau)))
  (Gamma ::= empty (Gamma comma x : tau))
  (mode ::= => <=) ;; infer and check
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
