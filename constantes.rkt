 #lang racket

;; Arquivo para definição das constantes do programa.


(require 2htdp/image)
;inclua outros pacotes ou arquivos necessários usando (require ...)

(provide (all-defined-out)) ;permite que outros arquivos importem deste

;; =================
;; Constantes:

;; TESTE DE COMMIT


;;CONSTANTES:
(define LARGURA 600)
(define ALTURA 400)
(define CENARIO (rectangle LARGURA ALTURA "outline" "black"))
(define IMG-VACA-INO (bitmap "vaca-ino.png"))
(define IMG-VACA-VORTANO (flip-horizontal IMG-VACA-INO))
(define Y (/ ALTURA 2))

(define IMG-CC (scale 0.5 (bitmap "chupacabra.jpg")))

(define X-CC (/ LARGURA 2))
(define DY-CC-DEFAULT 5)

(define MEIO-H-VACA (/ (image-width IMG-VACA-INO) 2 ))
(define MEIO-H-CC (/ (image-width IMG-CC) 2 ))

(define LIMITE-DIREITO (- LARGURA MEIO-H-VACA))

(define TELA-GAME-OVER (overlay (text "GAME OVER" 30 "red") CENARIO))
