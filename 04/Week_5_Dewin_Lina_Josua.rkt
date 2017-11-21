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
   (emergencyInformation caseOfEmergency) "\n"
   (additionalNotes notes) "\n"
   (pileSigns) "\n"
   "OVER"))

(define (ueberschrift shipname callsign)
  (string-append
   "MAYDAY MAYDAY MAYDAY \n"
   "HIER IST"
   shipname shipname shipname callsign
   "MAYDAY" shipname shipname
   callsign))


(define buchstabiertafel (list (cons "A" "Alpha")
                               (cons "B" "Bravo")
                               (cons "C" "Charlie")
                               (cons "D" "Delta")
                               (cons "E" "Echo")
                               (cons "F" "Foxtrot")
                               (cons "G" "Golf")
                               (cons "H" "Hotel")
                               (cons "I" "India")
                               (cons "J" "Juliet")
                               (cons "K" "Kilo")
                               (cons "L" "Lima")
                               (cons "M" "Mike")
                               (cons "N" "November")
                               (cons "O" "Oscar")
                               (cons "P" "Papa")
                               (cons "Q" "Quebec")
                               (cons "R" "Romeo")
                               (cons "S" "Sierra")
                               (cons "T" "Tango")
                               (cons "U" "Uniform")
                               (cons "V" "Viktor")
                               (cons "W" "Whiskey")
                               (cons "X" "X-ray")
                               (cons "Y" "Yankee")
                               (cons "Z" "Zulu")
                               (cons "0" "Nadazero")
                               (cons "1" "Unaone")
                               (cons "2" "Duotwo")
                               (cons "3" "Terrathree")
                               (cons "4" "Carrefour")
                               (cons "5" "Pentafive")
                               (cons "6" "Soxisix")
                               (cons "7" "Setteseven")
                               (cons "8" "Oktoeight")
                               (cons "9" "Novonine")
                               (cons "," "Decimal")
                               (cons "." "Stop")))
(define (getKey input)
  (cdr (assoc input buchstabiertafel)))

(define (getKeyString input)
  (if (< 0 (string-length input))
      (let ([currentChar (substring input 0 1)]
            [input (substring input 1)])
        (string-append (string-append (getKey currentChar) " ") (getKeyString input)))
        " "))