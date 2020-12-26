#lang racket
(require redex/reduction-semantics)

(define-language F
  (X ::= variable-not-otherwise-mentioned) ;; type var
  (x ::= variable-not-otherwise-mentioned) ;; term var
  (tau ::= X (-> tau tau) (forall X tau)) ;; universal type
  (e ::=
     x (lambda (x : tau) e) (e e)
     (Lambda X e) ;; type abstraction
     (e [tau])) ;; type application
  (gamma ::= dot (gamma comma x : tau)) ;; term context
  (Gamma ::= dot (Gamma comma X)) ;; type context
  #:binding-forms
  (lambda (x : tau) e #:refers-to x)
  (Lambda X e #:refers-to X)
  (forall X tau #:refers-to X)
  )
