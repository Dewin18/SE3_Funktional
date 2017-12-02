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
(define (getSpecificAttrList attr)
  (cond [(string-contains? "bluegreenyellowred"  (symbol->string attr)) (car attributes)]
        [(string-contains? "curvedcurlystraight" (symbol->string attr)) (car (cdr attributes))]
        [(string-contains? "stardotsstripes"     (symbol->string attr)) (car (cddr attributes))]
        [(string-contains? "ellipserhombhexagon" (symbol->string attr)) (car (cdddr attributes))]
        [else "UNDEFINED ATTRIBUTE"]))

;;1.2.1
;returns any other recessive attributes of a given attribute
(define (getAnyOtherRecessiveAttributes attr)
  (cond [(= 0 (index-of (getSpecificAttrList attr) attr)) (cdr (getSpecificAttrList attr))]
        [(= 1 (index-of (getSpecificAttrList attr) attr)) (cddr (getSpecificAttrList attr))]
        [(= 2 (index-of (getSpecificAttrList attr) attr)) (cdddr (getSpecificAttrList attr))]))

;;1.2.2
;compares two attributes in terms of dominance
(define (getMoreDominantAttribute attr1 attr2)
  (if (< (index-of (getSpecificAttrList attr1)  attr1)
         (index-of (getSpecificAttrList attr2)  attr2))
      attr1
      attr2))

;1.2.3
;creates a butterfly with dominant attributes and random recessive attributes
(define (makeButterfly visibleColor visiblePalp visiblePattern visibleWings)
  (list visibleColor   (generateRandomAttribute visibleColor)
        visiblePalp    (generateRandomAttribute visiblePalp)
        visiblePattern (generateRandomAttribute visiblePattern)
        visibleWings   (generateRandomAttribute visibleWings)))

;returns a random attribute of an attribute list
(define (generateRandomAttribute attr)
(list-ref (getSpecificAttrList attr) (random (length (getSpecificAttrList attr)))))

;1.2.4
;returns all visible attributes of a butterfly.
(define (getDominantAttr butterfly)
  (flatten (getAttributes butterfly)))

;returns all invisible attributes of a butterfly.
(define (getRecessiveAttr butterfly)
  (flatten (getAttributes (cdr butterfly))))

;recursive step to get all dominant or all resessive attributes
(define (getAttributes butterfly)
  (if (empty? butterfly)
      '()
      (if (= 1 (length butterfly))
          (car butterfly)
          (cons (car butterfly) (getAttributes (cddr butterfly))))))

;1.2.5
;define some different butterflies for testing
(define butterfly1 (makeButterfly 'red 'dots 'curved 'rhomb))
(define butterfly2 (makeButterfly 'blue 'star 'curly 'hexagon))
(define butterfly3 (makeButterfly 'yellow 'dots 'straight 'ellipse))

;displays the butterfly on the screen
(define (display-butterfly butterfly)
  (let ([visAttr (getDominantAttr butterfly)])
        (show-butterfly (car visAttr) (cadr visAttr) (caddr visAttr) (cadddr visAttr))))

;1.2.6
;returns random one of the first two elements from a butterfly
(define (getRandomAttribute butterfly)
  (chooseRandomAttribute (take butterfly 2)))

;returns random one of the first two elements in a list
(define (chooseRandomAttribute list)
  (let ([rnd (random 2)])
    (if (= 0 rnd)
        (car list)
        (cadr list))))

;returns a child butterfly with all butterfly attributes. Recessive attributes are completely random generated
(define (getChildOf motherButterfly fatherButterfly)
  (let ([child (flatten (pairButterflies motherButterfly fatherButterfly))])
  (makeButterfly (car child) (cadr child) (caddr child) (cadddr child))))

;helper function to generate a random attribute for each specification e.g.
;the child color is a random color of mother and fathers dominant / recessive color.
(define (pairButterflies motherButterfly fatherButterfly)
  (if (empty? motherButterfly)
      '()
      (list (getMoreDominantAttribute (getRandomAttribute motherButterfly) (getRandomAttribute fatherButterfly))
            (pairButterflies (cddr motherButterfly) (cddr fatherButterfly)))))

;(display-butterfly (getChildOf butterfly1 butterfly2))

;returns a list of lists. Each nested list represents a child butterfly
(define (getChildren motherButterfly fatherButterfly numberOfChildren)
  (for/list ((i numberOfChildren))
    (getChildOf butterfly1 butterfly2)))

;displays all butterfly childrens from a list to the screen
(define (display-all-childrens childrenList)
  (display (for/list ((i (length childrenList)))
    (display-butterfly (list-ref childrenList i)))))
             
;;sample outputs
(getSpecificAttrList 'blue)
(getAnyOtherRecessiveAttributes 'blue)
(getMoreDominantAttribute 'yellow 'green)
(getDominantAttr butterfly2)
(getRecessiveAttr butterfly2)
(display-butterfly butterfly2)
(getChildren butterfly1 butterfly2 10)
(display-all-childrens (getChildren butterfly1 butterfly3 10))
