#lang racket


;; Arquivo para definição dos tipos de dados do programa.


(require 2htdp/image)
(require "constantes.rkt")
;inclua outros pacotes ou arquivos necessários usando (require ...)

(provide (all-defined-out)) ;permite que outros arquivos importem deste


;;DEFINIÇÕES DE DADOS
                           
(define-struct chupacabra (x dx y dy) #:transparent)
;; Chupacabra é (make-chupacabra Natural Inteiro Natural Inteiro)
;; interp. representa o chupacabra que está numa posição y
;; da tela e anda a uma velocidade dy (dy também indica a direção
;; em que ele está apontando)

;exemplos:
(define CC-INICIAL (make-chupacabra X-CC 0 0 DY-CC-DEFAULT))
(define CC-MEIO (make-chupacabra X-CC 0 (/ ALTURA 2) DY-CC-DEFAULT))
(define CC-ANTES-VIRAR (make-chupacabra X-CC 0 (+ ALTURA 5) DY-CC-DEFAULT))
(define CC-VIROU (make-chupacabra X-CC 0 ALTURA (- DY-CC-DEFAULT)))
(define CC-CHEGANDO (make-chupacabra X-CC 0 -5 (- DY-CC-DEFAULT)))
(define CC-VIROU-L-CIMA (make-chupacabra X-CC 0 0 DY-CC-DEFAULT))

#;
(define (fn-para-chupacabra cc)
  (... (chupacabra-y cc) (chupacabra-dy cc))
  )


(define-struct vaca (x dx)  #:transparent)
;;Vaca é (make-vaca Natural Inteiro)
;;interp. representa a vaca que está numa posição x
;;da tela e anda a uma velocidade dx (dx também indica a direção
;;em que ela está apontando)

;exemplos:
(define VACA-INICIAL (make-vaca 0 10))
(define VACA-MEIO (make-vaca (/ LARGURA 2) 10))
(define VACA-ANTES-VIRAR (make-vaca (+ LIMITE-DIREITO 5) 10))
(define VACA-VIRADA (make-vaca LIMITE-DIREITO -10))
(define VACA-MEIO-VORTANO (make-vaca (/ LARGURA 2) -10))
(define VACA-CHEGANDO (make-vaca 50 -10))
(define VACA-ULTRAPASSOU (make-vaca (+ LIMITE-DIREITO 20) 50))
(define VACA-NO-LIMITE (make-vaca LIMITE-DIREITO -50))

#;
(define (fn-para-vaca v)
  (... (vaca-x v) (vaca-dx v))
  )


;; ListaDeChupacabra é um desses:
;; - empty       
;; - (cons Chupacabra ListaDeChupacabra)    
;; interp. uma lista de chupacabras
(define LDCC-1 empty)
(define LDCC-2 (cons CC-INICIAL empty))
(define LDCC-3 (cons CC-INICIAL (cons CC-MEIO empty)))

#;
(define (fn-para-ldcc ldcc)
  (cond [(empty? ldcc) (...)]                   ;CASO BASE (CONDIÇÃO DE PARADA)
        [else (... (first ldcc)                 ;Chupacabra
                   (fn-for-ldcc (rest ldcc)))])) ;RECURSÃO EM CAUDA


(define-struct jogo (vaca chupacabras game-over?)  #:transparent)
;; Jogo é (make-jogo Vaca ListaDeChupacabra Boolean)
;; interp. representa um jogo que tem uma vaca
;; e VARIOS chupacabra.

(define JOGO-INICIAL (make-jogo VACA-INICIAL
                                (list CC-INICIAL)
                                #false))
(define JOGO-MEIO (make-jogo VACA-ANTES-VIRAR
                                (list CC-MEIO)
                                #false))
(define JOGO-ZICA (make-jogo
                   (make-vaca (- (/ LARGURA 2) MEIO-H-CC -5) 10)
                   (list CC-MEIO)
                   #false))
(define JOGO-ZICA-BRABA (make-jogo
                   (make-vaca (- (/ LARGURA 2) MEIO-H-CC -5) 10)
                   (list CC-MEIO)
                   #true))
(define JOGO-ACABOU (make-jogo VACA-MEIO
                               (list CC-MEIO)
                               #true))

(define JOGO-3-CHUPACABRAS (make-jogo VACA-INICIAL
                                      (list CC-INICIAL
                                            (make-chupacabra
                                             (* LARGURA 0.25)
                                             0
                                             (/ ALTURA 2)
                                             DY-CC-DEFAULT)
                                            (make-chupacabra
                                             (* LARGURA 0.75)
                                             0
                                             (* ALTURA 0.25)
                                             DY-CC-DEFAULT))
                                      #false))

#;
(define (fn-para-jogo j)
  (... (jogo-vaca j)
       (jogo-chupacabras j)
       (jogo-game-over? j)))

 