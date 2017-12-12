#lang racket

(require picturing-programs)

;Solution of exercise sheet 6,

;author:

;Dewin Bagci, 6815336
;Josua Spisak, 6944594
;Lina Kaine, 6499396

;;;2 Nikolausaufgabe

;our Simulation
(define (christmas-sim tick) ;tick decides the speed speed is higher if tick is smaller
  (big-bang
      (make-christmas-universe 1)
    (on-tick next-frame tick)
    (to-draw show-christmas-world
             christmas-canvas-w christmas-canvas-h)
    (name "Christmas")))

;width of the picture
(define christmas-canvas-w 1000)
;height of the picture
(define christmas-canvas-h 1000)
;our universe with the parameter year wich is the time that went by
(define-struct christmas-universe
  (year))
;whats supposed to happen for the next frame of the animation
(define (next-frame world)
  (let ([new-world (make-christmas-universe  ;in the next frame we create a new world
                    (add1 (christmas-universe-year world)))]) ;our new world is one year older then the last one
    new-world))
;what the animation should draw
(define (show-christmas-world world)
  (draw-christmas snowflake (christmas-universe-year world)))
;a snowflake
(define snowflake
  (star-polygon 20 7 3 "solid" "white"))
;our background
(define background
  (empty-scene 1000 1000 "black"))
;a text
(define text1
  (text "Merry Christmas" 40 "red"))
;another text
(define text2
  (text "Merry Christmas" 40 "gold"))
;a sled
(define (sled r) ;can be reversed
  (overlay/xy
                 (overlay/align
                  "center" "bottom"
                 (rectangle 200 10 "solid" "silver")
                 (rectangle 200 50 "solid" "red"))
                 (if r -50 150) -50
                 (circle 50 "outline" (pen "silver" 10 "solid" "round" "round"))))
;a present
(define present
  (above
   (beside
    (ellipse  30 20 "outline" (pen "gold" 5 "solid" "round" "round"))
    (ellipse  30 20 "outline" (pen "gold" 5 "solid" "round" "round")))
   (rectangle 80 80 "solid" "red")))
;our scene
(define scene
  (overlay/align
   "center" "center"
   text2
   background))
;a recursive function that draws snowflakes
(define (place-snowflakes number universe-year canvas plane)
  (cond [(= number 0) canvas] ;termination condition
        [else
         (place-snowflakes (- number 1) universe-year (plane (place-image snowflake
                                                           (random 10 (- 1000 10)) ;width in wich it can be placed
                                                           (random 10 (+ 10 (* universe-year 20))) ;height in wich it can be placed
                                                           canvas) text1 )plane)])) ;we basicly decide wheteher the text is gold or red by look if the year is odd or even
;uses the year to decide how many snoflakes we want
(define (snoflake-number universe-year)
  (if (< universe-year 100)
      (* universe-year 3)
      500))
;a tree
(define (christmasTree size)
  (above/align
   "center"
   (star-polygon (/ size 10) 5 2 "solid" "gold")
   (triangle  (/ size 10) "solid" "forestgreen")
   (triangle (/ size 5) "solid" "forestgreen")
   (triangle  (/ size 3) "solid" "forestgreen")
   (rectangle (/ size 10) (/ size 10) "solid" "brown")))
;uses the year to decide the tree size
(define (tree-size universe-year size)
  (if (< universe-year 40)
  (+ size (* universe-year 5))
  (+ size (* 40 5))))
;desides the size for a christmasforest
(define (christmasTrees-size n)
  (cond ([odd? n] *)
        ([even? n ] /)))
;draws multiple christmas trees
(define (christmasTrees n size universe-year canvas)
  (if (< 0 n)
      (christmasTrees (- n 1) ((christmasTrees-size n) size 2 ) universe-year (beside
                                                   (christmasTree size)
                                                   canvas))
      canvas))
;number of trees also using univerese-year
(define (tree-number universe-year)
  (+ 1 (floor (/ universe-year 10))))
;draws snow at the floor
(define (snow-floor universe-year)
  (cond ([> universe-year 50] (rectangle 1000  (- (/ universe-year 2) 20) "solid" "white"))
        [else (rectangle 0 0 "solid" "black")]));we only have a snowfloor once a certain time is passedso we have an image of nothing till then this could also be solved by making the cond in draw-christmas but i wanted to keep that clas as small as possilbe
;chooses if the text is gold or red
(define (choose-snoflake-plane universe-year)
  (if (even? universe-year)
      underlay
      overlay ))
;decides the position of a present
(define (present-pos universe-year)
  (cond ([< universe-year 20] 0)
        ([< universe-year 30] 100)
        ([< universe-year 40] 200)
        ([< universe-year 50] 300)
        ([< universe-year 60] 400)
        [else 400]))
;draws presents
(define (presents universe-year)
  (cond ([< universe-year 10] (empty-scene 0 0) )
         ([< universe-year 20] present )
         ([< universe-year 30] (overlay/offset present 200 0 present))
         ([< universe-year 40] (overlay/offset  (overlay/offset present 200 0 present) 300 0 present))
         ([< universe-year 50] (overlay/offset (overlay/offset  (overlay/offset present 200 0 present) 300 0 present) 400 0 present))
         [else (overlay/offset (overlay/offset (overlay/offset  (overlay/offset present 200 0 present) 300 0 present) 400 0 present) 500 0 present) ]))
;decides the sled position x
(define (sled-position-x universe-year)
  (if (< universe-year 60)
      (* 20 universe-year)
      (- 1600 (* 10 universe-year))))
;decides the sled position y
(define (sled-position-y universe-year)
  (if (< universe-year 60 )
      900
      (- 1500 (* 10 universe-year))))
;the real drawing begines here
;we place images in images until we got every part of the picture
(define (draw-christmas picture universe-year)
  (place-image
   (sled (if (< universe-year 60)
             false
             true))
   (sled-position-x  universe-year) (sled-position-y universe-year)
  (overlay/align
   "center" "bottom"
   (snow-floor universe-year)
   (christmasTrees 5 (tree-size universe-year 200) universe-year (empty-scene 0 0 "black"))
   (place-image
    (presents universe-year)
    (+ 50 (present-pos universe-year)) 950
  (place-snowflakes (snoflake-number universe-year) universe-year scene (choose-snoflake-plane universe-year))))))

;so we can see what happens
(animate (christmas-sim 0.5))

      