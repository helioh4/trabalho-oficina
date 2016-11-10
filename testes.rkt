#lang racket

(require rackunit)
(require "constantes.rkt")
(require "dados.rkt")
(require "jogo.rkt")


;; Constantes usadas nos testes
;; ...

;; Testes (separe-os por função, não misture):

;caso normal
(check-equal? (proximo-jogo JOGO-INICIAL)
              (make-jogo (make-vaca 10 10)
                         (list
                          (make-chupacabra X-CC 0 DY-CC-DEFAULT DY-CC-DEFAULT))
                         #false))

(check-equal? (proximo-jogo JOGO-ZICA)
              JOGO-ZICA-BRABA)



(check-equal? (colisao-vaca-algum-chupacabra? VACA-MEIO
                                              (list CC-MEIO
                                                    CC-INICIAL
                                                    CC-CHEGANDO))
              #true)
(check-equal? (colisao-vaca-algum-chupacabra? VACA-INICIAL
                                              (list CC-MEIO
                                                    CC-INICIAL
                                                    CC-CHEGANDO))
              #false)
(check-equal? (colisao-vaca-algum-chupacabra? VACA-INICIAL
                                              empty)
              #false)


;exemplos
(check-equal? (proximos-chupacabras empty) empty)
(check-equal? (proximos-chupacabras
               (list CC-ANTES-VIRAR
                     (make-chupacabra X-CC 0 0 10)))
              (list CC-VIROU
                    (make-chupacabra X-CC 0 10 10)))



(check-equal? (colisao-vaca-chupacabra?
               (make-vaca (- (/ LARGURA 2) MEIO-H-CC -5) 10)
               CC-MEIO)
              #true)
(check-equal? (colisao-vaca-chupacabra?
               VACA-INICIAL
               CC-INICIAL)
              #false)


(check-equal? (distancia 3 0 0 4) 5)

; exemplos / testes
;casos em que ela anda pra direita sem chegar no limite
(check-equal? (proximo-chupacabra (make-chupacabra X-CC 0 0 10))
              (make-chupacabra X-CC 0 10 10))
(check-equal? (proximo-chupacabra CC-MEIO)
              (make-chupacabra X-CC 0 (+ (/ ALTURA 2) DY-CC-DEFAULT)
                         DY-CC-DEFAULT))
; casos em que chega no limite direito e tem que ccirar
(check-equal? (proximo-chupacabra CC-ANTES-VIRAR)
              CC-VIROU)

; caso em que ela anda pra esquerda sem chegar no limite 
(check-equal? (proximo-chupacabra
               (make-chupacabra X-CC 0 (/ ALTURA 2) (- DY-CC-DEFAULT)))
              (make-chupacabra X-CC 0 (- (/ ALTURA 2) DY-CC-DEFAULT)
                                       (- DY-CC-DEFAULT)))

; casos em que chega no limite esquerdo e tem que virar
(check-equal? (proximo-chupacabra (make-chupacabra X-CC 0 -10 -10))
                            (make-chupacabra X-CC 0 0 10))
(check-equal? (proximo-chupacabra (make-chupacabra X-CC 0 -20 -50))
                            (make-chupacabra X-CC 0 0 50))


; exemplos / testes
;casos em que ela anda pra direita sem chegar no limite
(check-equal? (proxima-vaca (make-vaca 0 10))
              (make-vaca 10 10))
(check-equal? (proxima-vaca VACA-MEIO)
              (make-vaca (+ (/ LARGURA 2) 10)
                         10))
; casos em que chega no limite direito e tem que virar
(check-equal? (proxima-vaca VACA-ANTES-VIRAR)
              VACA-VIRADA)
(check-equal? (proxima-vaca VACA-ULTRAPASSOU)
                            VACA-NO-LIMITE)
; caso em que ela anda pra esquerda sem chegar no limite 
(check-equal? (proxima-vaca VACA-MEIO-VORTANO)
                            (make-vaca (- (/ LARGURA 2) 10)
                                       -10))

; casos em que chega no limite esquerdo e tem que virar
(check-equal? (proxima-vaca (make-vaca -10 -10))
                            (make-vaca 0 10))
(check-equal? (proxima-vaca (make-vaca -20 -50))
                            (make-vaca 0 50))



(check-equal? (trata-tecla (make-jogo
                            (make-vaca 100 10)
                            CC-INICIAL #false) " ")
              (make-jogo
               (make-vaca 100 -10)
               CC-INICIAL #false))

(check-equal? (trata-tecla JOGO-ZICA-BRABA "\r")
              JOGO-3-CHUPACABRAS)


(check-equal? (trata-tecla-vaca (make-vaca 100 10) " ")
              (make-vaca 100 -10))
(check-equal? (trata-tecla-vaca (make-vaca 200 -10) " ")
              (make-vaca 200 10))
(check-equal? (trata-tecla-vaca VACA-INICIAL "a")
              VACA-INICIAL)

