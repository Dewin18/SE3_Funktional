#lang racket

(require se3-bib/butterfly-module)

;Solution of exercise sheet 5,

;author:

;Dewin Bagci, 6815336
;Josua Spisak, 6944594
;Lina Kaine, 6499396

;;;1

;; we chose a list with every attribute of the butterfly

(define (butterfly color1 color2
                   palp1 palp2
                   pattern1 pattern2
                   wings1 wings2)
  (list  color1 color2
         palp1 palp2
         pattern1 pattern2
         wings1 wings2))

;;1.1.1
;this function returns the dominant color of the the given butterfly
(define (getDominantColor butterfly)
  (cond [(member "blue" butterfly) "blue "]
        [(member "green" butterfly) "green "]
        [(member "yellow" butterfly) "yellow "]
        [else "red "]))

;;1.1.1
;this function returns the dominant Fuehler of the the given butterfly
(define (getDominantPalp butterfly)
  (cond [(member "curved" butterfly) "curved "]    ;gekruemmt
        [(member "curly" butterfly) "curly "]      ;geschweift 
        [else "straight "]))                       ;gerade 

;;1.1.1
;this function returns the dominant Muster of the the given butterfly
(define (getDominantPattern butterfly)
  (cond [(member "star" butterfly) "star "]        ;Sterne
        [(member "dots" butterfly) "dots "]        ;Punkte
        [else "stripes "]))                        ;Streifen

;;1.1.1
;this function returns the dominant Fluegel of the the given butterfly
(define (getDominantWings butterfly)
  (cond [(member "ellipse" butterfly) "ellipse"]
        [(member "rhomb" butterfly) "rhomb"]
        [else "hexagon"]))

;;1.1.2
;this function returns the dominant attributes of the given butterfly
(define (getDominantAttributes butterfly)
  (string-append (getDominantColor butterfly)     ;Farbe
                 (getDominantPalp butterfly)      ;Fuehler
                 (getDominantPattern butterfly)   ;Muster
                 (getDominantWings butterfly)))   ;Fluegel

;exmaple use of getDominantAttributes  with a butterfly
;(getDominantAttributes (butterfly "blue" "red" "gerade" "gekruemmt" "streifen" "punkte" "elliptisch" "hexagonal"))

#|------------------------------------------------------------------------------------------------------------------|#

;;;Alternative approach

;1.1.1
;this datastructure stores all related attributes as lists in a list
(define attributes (list (list 'blue 'green 'yellow 'red)
                         (list 'curved 'curly 'straight)
                         (list 'star 'dots 'stripes)
                         (list 'ellipse 'rhomb 'hexagon)))

;1.1.2
;this function search for a substring in a given string and returns its list
(define (getAttrList attr)
  (cond [(string-contains? "bluegreenyellowred"  (symbol->string attr)) (car attributes)]
        [(string-contains? "curvedcurlystraight" (symbol->string attr)) (car (cdr attributes))]
        [(string-contains? "stardotsstripes"     (symbol->string attr)) (car (cddr attributes))]
        [(string-contains? "ellipserhombhexagon" (symbol->string attr)) (car (cdddr attributes))]
        [else "UNDEFINED ATTRIBUTE"]))

;(getAttrList 'blue)

;;1.2.1
;returns any other recessive attributes of a given attribute
(define (getRecessiveAttributes attr)
  (cond [(= 0 (index-of (getAttrList attr) attr)) (cdr (getAttrList attr))]
        [(= 1 (index-of (getAttrList attr) attr)) (cddr (getAttrList attr))]
        [(= 2 (index-of (getAttrList attr) attr)) (cdddr (getAttrList attr))]))
  
;(getRecessiveAttributes 'blue)

;;1.2.2
;compares two attributes in terms of dominance
(define (compareAttributes attr1 attr2)
  (if (< (index-of (getAttrList attr1)  attr1)
         (index-of (getAttrList attr2)  attr2))
      attr1
      attr2))

(compareAttributes 'yellow 'green)

;1.2.3
;creates a butterfly with dominant attributes and random recessive attributes
(define (makeButterfly visibleColor visiblePalp visiblePattern visibleWings)
  (list visibleColor   (getRandomAttribute visibleColor)
        visiblePalp    (getRandomAttribute visiblePalp)
        visiblePattern (getRandomAttribute visiblePattern)
        visibleWings   (getRandomAttribute visibleWings)))

;returns a random attribute of an attribute list
(define (getRandomAttribute attr)
(list-ref (getAttrList attr) (random (length (getAttrList attr)))))

(makeButterfly 'green 'dots 'curved 'rhomb)

;1.2.4
;returns all visible attributes of a butterfly
;(define (getVisAttributes butterfly)
;TODO einfach jedes zweite element von (makeButterfly ...) ausgeben. Start bei 0

;returns all invisible attributes of a butterfly
;(define (getInvisAttributes butterfly)
;TODO einfach jedes zweite element von (makeButterfly ...) ausgeben. Start bei 1

;1.2.3
(define butterfly1 (makeButterfly 'red 'dots 'curved 'rhomb))
(define butterfly2 (makeButterfly 'blue 'star 'curly 'hexagon))
(define butterfly3 (makeButterfly 'yellow 'dots 'straight 'ellipse))

(define (display-butterfly butterfly)
  (let ([color   (car butterfly)]
        [palp    (car (list-tail butterfly 2))]
        [pattern (car (list-tail butterfly 4))]
        [wings   (car (list-tail butterfly 6))])
        (show-butterfly color palp pattern wings)))

;displays the butterfly on the screen
(display-butterfly butterfly3)
