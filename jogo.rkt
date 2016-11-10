; Este arquivo deve conter as definições das funções do jogo (com exceção da main).
; As definições devem incluir assinatura, propósito, protótipo e templates utilizados.


#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require "constantes.rkt") ;importa as constantes definidas no arquivo "constantes.rkt"
(require "dados.rkt") ;importa as definições de dados definidas no arquivo "dados.rkt"

(provide (all-defined-out)) ;permite que outros arquivos importem deste

;; =================
;; Funções:

;; Jogo -> Jogo
;; inicie o mundo com (main JOGO-3-CHUPACABRAS)

(define (main j)
  (big-bang j      ; Jogo
            (on-tick proximo-jogo)
            (to-draw desenha-jogo)
            (on-key trata-tecla)))



;; INICIO DA PARTE LÓGICA DO JOGO

;; proximo-jogo : Jogo -> Jogo
;; atualiza o jogo
;(define (proximo-jogo j)  j)

(define (proximo-jogo j)
  (cond [(colisao-vaca-algum-chupacabra? (jogo-vaca j) (jogo-chupacabras j))
         (make-jogo (jogo-vaca j)
                    (jogo-chupacabras j)
                    #true)]

        [else (make-jogo (proxima-vaca (jogo-vaca j))
                         (proximos-chupacabras (jogo-chupacabras j))
                         (jogo-game-over? j))]
  ))





;; colisao-vaca-algum-chupacabra? : Vaca ListaDeChupacabra -> Boolean
;; verifica se vaca colidiu com algum dos chupacabras
;(define (colisao-vaca-algum-chupacabra? v ldcc) #false)

(define (colisao-vaca-algum-chupacabra? v ldcc)
  (cond [(empty? ldcc) #false]                   ;CASO BASE (CONDIÇÃO DE PARADA)
        [else (or (colisao-vaca-chupacabra? v (first ldcc))                 ;Chupacabra
                   (colisao-vaca-algum-chupacabra? v (rest ldcc)))])) ;RECURSÃO EM CAUDA






;; proximos-chupacabras : ListaDeChupacabra -> ListaDeChupacabra
;; faz o proximo-chupacabra para cada chupacabra da lista
;(define (proximos-chupacabras ldcc) ldcc)

;(define (proximos-chupacabras ldcc)
;  (cond [(empty? ldcc) empty]                   ;CASO BASE (CONDIÇÃO DE PARADA)
;        [else (cons (proximo-chupacabra (first ldcc))                 ;Chupacabra
;                   (proximos-chupacabras (rest ldcc)))])) ;RECURSÃO EM CAUDA

(define (proximos-chupacabras ldcc)
  (map proximo-chupacabra ldcc))



                    


;; colisao-vaca-chupacabra? : Vaca Chupacabra -> Boolean
;; verifica se vaca e chupacabra trombaram
;!!!
;(define (colisao-vaca-chupacabra? v cc) #false)
(define (colisao-vaca-chupacabra? v cc)
  (<= (distancia (vaca-x v) Y
             (chupacabra-x cc) (chupacabra-y cc))
      (+ MEIO-H-VACA MEIO-H-CC)))



;; distancia : Numero Numero Numero Numero -> Numero
;; calcula distancia
; !!!
(define (distancia x1 y1 x2 y2)
  (sqrt (+ (sqr (- x2 x1)) (sqr (- y2 y1)))))



;; proximo-chupacabra : Chupacabra -> Chupacabra
;; faz chupacabra andar no eixo y, e se trombar nos limites,
;; inverte dy

(define (proximo-chupacabra cc)
  (cond 
        [(> (chupacabra-y cc) ALTURA)
         (make-chupacabra (chupacabra-x cc)
                          (chupacabra-dx cc)
                          ALTURA (- (chupacabra-dy cc)))]
        [(< (chupacabra-y cc) 0)
         (make-chupacabra (chupacabra-x cc)
                          (chupacabra-dx cc)
          0 (- (chupacabra-dy cc)))]
        [else
         (make-chupacabra (chupacabra-x cc)
                          (chupacabra-dx cc)
          (+ (chupacabra-y cc) (chupacabra-dy cc))
             (chupacabra-dy cc))])
 )




;; proxima-vaca : Vaca -> Vaca
;; recebe uma vaca na posicao x e retorna uma vaca com posição
;; x atualizada com o dx
;(define (proxima-vaca v) v)
(define (proxima-vaca v)
  (cond 
        [(> (vaca-x v) LIMITE-DIREITO)
         (make-vaca LIMITE-DIREITO (- (vaca-dx v)))]
        [(< (vaca-x v) 0)
         (make-vaca 0 (- (vaca-dx v)))]
        [else
         (make-vaca (+ (vaca-x v) (vaca-dx v))
             (vaca-dx v))])
 )




              
;; FIM DA PARTE LÓGICA


;; INICIO DA PARTE VISUAL

;; desenha-jogo : Jogo -> Image
;; desenha o jogo
;!!!
(define (desenha-jogo j)
  (if (jogo-game-over? j) TELA-GAME-OVER
  (overlay
   (desenha-chupacabras (jogo-chupacabras j))
   (desenha-vaca (jogo-vaca j)))))


;; desenha-chupacabras : ListaDeChupacabra -> Image
;(define (desenha-chupacabras ldcc) ldcc)

(define (desenha-chupacabras ldcc)
  (cond [(empty? ldcc) CENARIO]                   ;CASO BASE (CONDIÇÃO DE PARADA)
        [else (overlay (desenha-chupacabra (first ldcc))                ;Chupacabra
                   (desenha-chupacabras (rest ldcc)))])) ;RECURSÃO EM CAUDA


;; desenha-chupacabra : Chupacabra -> Image
;; desenha o chupacabra
(define (desenha-chupacabra cc)
  (place-image IMG-CC (chupacabra-x cc) (chupacabra-y cc) CENARIO))


;; desenha-vaca: Vaca -> Image
;; retorna a representação do cenário com a vaca
#;
(define (fn-para-vaca v)
  (... (vaca-x v) (vaca-dx v))
  )

(define (desenha-vaca v)
  (place-image
   (if (< (vaca-dx v) 0)
       IMG-VACA-VORTANO
       IMG-VACA-INO)
   (vaca-x v)
   Y
   CENARIO)               
  )


;; FIM DA PARTE VISUAL

;; INICIO DA LOGICA DE INTERAÇÃO

;; trata-tecla : Jogo KeyEvent -> Jogo
;; trata tecla usando trata-tecla-vaca
;!!!
(define (trata-tecla j ke)
  (cond
    [(and (jogo-game-over? j) (key=? ke "\r"))
         JOGO-3-CHUPACABRAS]
    [else (make-jogo
           (trata-tecla-vaca (jogo-vaca j) ke)
           (jogo-chupacabras j)
           (jogo-game-over? j)
           )]))


;; trata-tecla-vaca: Vaca KeyEvent -> Vaca
;; quando tecla espaço é pressionada, vaca deve inverter direção (dx)
;(define (trata-tecla-vaca v ke) v)

(define (trata-tecla-vaca v ke)
  (cond [(key=? ke " ")
         (make-vaca (vaca-x v) (* (vaca-dx v) -1))]
        [else v]))


;; FIM DA LOGICA DE INTERAÇÃO


