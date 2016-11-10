; Este arquivo deve conter as definições das funções do jogo (com exceção da main).
; As definições devem incluir assinatura, propósito, protótipo e templates utilizados.


#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require "constantes.rkt") ;importa as constantes definidas no arquivo "constantes.rkt"
(require "dados.rkt") ;importa as definições de dados definidas no arquivo "dados.rkt"

(provide (all-defined-out)) ;permite que outros arquivos importem deste


;; EstadoMundo -> EstadoMundo
;; produz o próximo ...
;; !!!
(define (tock estado) estado)    ;;protótipo


;; EstadoMundo -> Image
;; desenha ... 
;; !!!
(define (desenha estado) empty-image)  ;;protótipo


;; EstadoMundo -> Boolean
;; define se o jogo deve ser terminado quando ...
;; !!!
(define (terminou? estado) false)   ;;protótipo



;; EstadoMundo KeyEvent -> EstadoMundo
;; quando tecla ... é pressionada produz ...  <apagar caso não precise usar>
(define (trata-tecla estado ke)
  (cond [(key=? ke " ") estado]
        [else
         estado]))     ;;protótipo


;; EstadoMundo Integer Integer MouseEvent -> EstadoMundo
;; Quando fazer ... nas posições x y no mouse produz ...   <apagar caso não precise usar>
(define (trata-mouse estado x y me)
(cond [(mouse=? me "button-down") estado]
      [else
       estado]))    ;;protótipo