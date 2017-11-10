#lang racket

;Solution of exercise sheet 1,

;author:

;Dewin Bagci, 6815336
;Josua Spisak, 6944594
;Lina Kaine, 6499396

;1 [Konversionsfunktionen]

;1.1 converts degrees to radians
(define (degrees->radians value)
  (* (/ (* 2 pi) 360) value))

;1.1 converts radians to degrees
(define (radians->degrees value)
  (* (/ 360 (* 2 pi)) value))

;1.2
;calculates arccos(x)
(define (my-acos x)
  (* 2 (atan (sqrt (/ (- 1 x) (+ 1 x))))))

;1.3
;definition of a seamile
(define seamile 1.852) 

;converts seamiles into miles
(define (nm->km miles)
  (* miles seamile))

;2[Großkreisentfernung und Kurse]

;2.1
;calculates the distance between two locations (A, B)
(define (distanceAB latA lonA latB lonB)
   (nm->km (* 60 (radians->degrees (greatCircleDistance latA lonA latB lonB)))))

;helper method that calculates great circle distance
;lat = latitude, lon = longitude
(define (greatCircleDistance latA lonA latB lonB)
   (my-acos (+ (* (sin (degrees->radians latA)) (sin (degrees->radians latB)))
               (* (cos (degrees->radians latA)) (cos (degrees->radians latB))
                  (cos (degrees->radians (- lonA lonB)))))))


;function calls: directions N and E with positive(+) sign, S and W with negative(-) sign
(distanceAB 59.93 10.75 22.20 114.10)     ;distance between Oslo - Hongkong = 8589km
(distanceAB 37.75 -122.45 21.32 -157.8)   ;distance between San Francisco - Honolulu = 3842km
(distanceAB -27.10 -109.40 -12.10 -77.05) ;distance between Osterinsel - Lima = 3757km

;2.3
;returns the direction for a given degree 
(define (degree->direction deg)
  (cond [(and (>= deg 0)    (< deg 22.5))  "N"]
        [(and (>= deg 22.5) (< deg 45))    "NNE"]
        [(and (>= deg 45)   (< deg 67.5))  "NE"]
        [(and (>= deg 67.5) (< deg 90))    "ENE"]
        [(and (>= deg 90)   (< deg 112.5)) "E"]
        [(and (>= deg 112.5)(< deg 135))   "ESE"]
        [(and (>= deg 135)  (< deg 157.5)) "SE"]
        [(and (>= deg 157.5)(< deg 180))   "SSE"]
        [(and (>= deg 180)  (< deg 202.5)) "S"]
        [(and (>= deg 202.5)(< deg 225))   "SSW"]
        [(and (>= deg 225)  (< deg 247.5)) "SW"]
        [(and (>= deg 247.5)(< deg 270))   "WSW"]
        [(and (>= deg 270)  (< deg 292.5)) "W"]
        [(and (>= deg 292.5)(< deg 315))   "WNW"]
        [(and (>= deg 315)  (< deg 337.5)) "NW"]
        [(and (>= deg 337.5)(< deg 360))   "NNW"]
        [else "Please insert a value between 0 and 359 inclusive"]))

;returns the degree for a given direction 
(define (dircetion->degree dir)
  (cond [(eqv? "N"   dir) 0]
        [(eqv? "NNE" dir) 22.5]
        [(eqv? "NE"  dir) 45]
        [(eqv? "ENE" dir) 67.5]
        [(eqv? "E"   dir) 90]
        [(eqv? "ESE" dir) 112.5]
        [(eqv? "SE"  dir) 135]
        [(eqv? "SSE" dir) 157.5]
        [(eqv? "S"   dir) 180]
        [(eqv? "SSW" dir) 202.5]
        [(eqv? "SW"  dir) 225]
        [(eqv? "WSW" dir) 247.5]
        [(eqv? "W"   dir) 270]
        [(eqv? "WNW" dir) 292.5]
        [(eqv? "NW"  dir) 315]
        [(eqv? "NNW" dir) 337.5]
        [else "Please insert a valid direction: N, NNE, NE, ENE, E, ESE, SE, SSE, S, SSW, SW, WSW, W, WNW, NW, NNW"]))