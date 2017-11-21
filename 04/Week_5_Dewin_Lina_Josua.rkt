#lang racket

;Solution of exercise sheet 4,

;author:

;Dewin Bagci, 6815336
;Josua Spisak, 6944594
;Lina Kaine, 6499396

;Die Klammern werden von innen nach außen ausgewertet, deshalb ist das Minimum von 5 und 1 = 1. Das Maximum von 1 und 6 ergibt dann 6.
(max(min 5(- 8 7)) 6)

;Quote stops the function from evaluating. 
'(+ ,(- 13 11) 17)

;prints the first element from the tail of the list.
(cadr '(Good King Wenceslas))

;the nested list (On the feast of Steven) is the third element in the list looked out.
;We kick out the first three elements so get an empty list back.
(cdddr '(looked out (On the feast of Steven)))

;a pair is formed between the symbol 'When and the list (the snow lay round about).
(cons 'When '(the snow lay round about))

;since 'crisp is the second part of the pair, we get a pair instead of a combined list.
(cons '(Deep and) 'crisp)

;the second part '(and even) is equivalent to (list 'and 'even). 
(equal? (list 'and 'even) '(and even))

;in '(the 'red-nosed 'reindeer) we have doubled the quotes for red-nosed and reindeer. this is why the lists arent equal.
(eq? (list 'Rudolph 'the 'red−nosed 'reindeer) (cons 'Rudolph '(the 'red−nosed 'reindeer)))
;for better visibility
(list 'Rudolph 'the 'red-nosed 'reindeer)
(cons 'Rudolph '(the 'red-nosed 'reindeer))


;2
(define (funkspruch shipname callsign position caseOfEmergency notes)
  (string-append
   (ueberschrift shipname callsign) "\n"
   (positionInformation position) "\n"
   caseOfEmergency "\n"
   notes "\n"
   "ICH SENDE DEN TRÄGER --" "\n"
   (getKeyString callsign) "\n"
   "OVER"))

(define (ueberschrift shipname callsign)
  (string-append
   "MAYDAY MAYDAY MAYDAY \n"
   "HIER IST " shipname " " shipname " " shipname " " (getKeyString callsign) "\n"
   "MAYDAY " shipname " ICH BUCHSTABIERE " (getKeyString shipname) "\n"
   "RUFZEICHEN " (getKeyString callsign)))

(define (positionInformation position)
  (string-append
   "NOTFALLPOSITION UNGEFÄHR " position))

(define buchstabiertafel (list (cons "A" "ALPHA")
                               (cons "B" "BRAVO")
                               (cons "C" "CHARLIE")
                               (cons "D" "DELTA")
                               (cons "E" "ECHO")
                               (cons "F" "FOXTROT")
                               (cons "G" "GOLF")
                               (cons "H" "HOTEL")
                               (cons "I" "INDIA")
                               (cons "J" "JULIET")
                               (cons "K" "KILO")
                               (cons "L" "LIMA")
                               (cons "M" "MIKE")
                               (cons "N" "NOVEMBER")
                               (cons "O" "OSCAR")
                               (cons "P" "PAPA")
                               (cons "Q" "QUEBEC")
                               (cons "R" "ROMEO")
                               (cons "S" "SIERRA")
                               (cons "T" "TANGO")
                               (cons "U" "UNIFORM")
                               (cons "V" "VIKTOR")
                               (cons "W" "WHISKEY")
                               (cons "X" "X-RAY")
                               (cons "Y" "YANKEE")
                               (cons "Z" "ZULU")
                               (cons "0" "NADAZERO")
                               (cons "1" "UNAONE")
                               (cons "2" "DUOTWO")
                               (cons "3" "TERRATHREE")
                               (cons "4" "CARREFOUR")
                               (cons "5" "PENTAFIVE")
                               (cons "6" "SOXISIX")
                               (cons "7" "SETTESEVEN")
                               (cons "8" "OKTOEIGHT")
                               (cons "9" "NOVONINE")
                               (cons "," "DECIMAL")
                               (cons "." "STOP")))
(define (getKey input)
  (cdr (assoc input buchstabiertafel)))

(define (getKeyString input)
  (if (< 0 (string-length input))
      (let ([currentChar (substring input 0 1)]
            [input (substring input 1)])
        (string-append (string-append (getKey currentChar) " ") (getKeyString input)))
        " "))

(display (funkspruch "UNICORN" "UCRN" "5 SM NORDWESTLICH LEUCHTTURM ROTER SAND" "SCHWERE SCHLAGSEITE WIR SINKEN" "KEINE VERLETZTEN"))