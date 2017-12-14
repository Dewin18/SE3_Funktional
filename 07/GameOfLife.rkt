#lang racket
;;;2 Game Of Life
(define N 30)
;;2.1
;this function gives us a list of list with basicly the cordinates of the Game so we have 1-30 places in row 1 1-30 places in row 2 and so on we give out the row number and then the seats
;using this function gives us the advantage that we can find the neighbours wich will be important for the game and we can set N however we want
;for each seat there is a maker that can be either a for alive or d for dead we start with every place beeing alive
(define (gameStart S N)
  (if (<= S N)
      (cons (cons (cons S '() ) (countAliveOrDeadFromTo 0 N)) (gameStart (+ S 1)  N ))
      '()))
;we also get a list of lists wich represent the coordinates but this time we have the rownumber inside the place number so insted of saying row 1: 1 2 3 4... ew say 1 2 3 4... and then next 11 12 ...
;the advantage of this is that we dont have the rownumber seperate to the seats we wanted to have both options available
(define (gameStartBig S N A)
  (if (<= S N)
      (cons  (countAliveOrDeadFromTo  (- (+ A 1)  10)   N) (gameStartBig (+ S 10 1)  (+ 10 N) (+ A 10) ))
      '()))
;this function counts from a start to and End and adds after each number a for alive or d for dead
(define (countAliveOrDeadFromTo S N)
  (if (< S N)
      (flatten (list  (cons S (car (shuffle (list "d" "a")))) (countAliveOrDeadFromTo (+ S 1) N)))
      (cons N "a") ))
;;2.2
(require 2htdp/image)
;the box that represents a living cell
(define alive
  (rectangle 10 10 "solid" "black"))
;the box that represents a dead cell
(define dead
  (rectangle 10 10 "outline" "black"))
;a function wich draws one row of the game
(define (drawRow Rlist S N)
  (if (<= S N)
      (beside (drawBox (car (cdr Rlist))) (drawRow (cddr Rlist) (+ 1 S) N ))
      (rectangle 0 0 "solid" "black") ))
;a function that draws one place of the game
(define (drawBox letter)
  (if (eq? letter "a")
      alive
      dead))
;a function that draws the hole game
(define (startPicture S N)
  (if (<= S N)
      (above (drawRow (cdr (assoc (cons S '()) (gameStart 0 N))) 0 N) (startPicture (+ 1 S) N))
      (rectangle 0 0 "solid" "black") ))
;;2.3
;analyses the Neigbhours of a cell at the position X Y and gives the state of every Neighbour from it back as a list
(define (analyseNeighbours X Y)
  (list
   (analyseNeighbour (+ X 1) Y)
   (analyseNeighbour (- X 1) Y)
   (analyseNeighbour  X  (+ Y 1))
   (analyseNeighbour  X  (-  Y 1))
   (analyseNeighbour (+ X 1) (- Y 1))
   (analyseNeighbour (+ X 1) (+ Y 1))
   (analyseNeighbour (- X 1) (+ Y 1))
   (analyseNeighbour (- X 1) (- Y 1))))
;analyses the state of a cell at Position X Y
(define (analyseNeighbour X Y)
  (let ([Row (cdr (assoc (cons X '()) (gameStart 0 N)))])
    (getXElementFromList (+ 1 (* 2 Y)) Row)))
;gives back the specific element at place X from a list
(define (getXElementFromList X List)
  (if (< 0 X)
      (getXElementFromList  (- X 1)(cdr List))
      (car List)))
;this function analyses what should happen next to a place with 8 Neighbours based on the state of these Neighbours 
(define (nextState NeighbourStates)
  (cond [(= 3 (countLiving NeighbourStates 0))
         "a"]
         [(< 3 (countLiving NeighbourStates 0))
         "d"]
         [(> 2 (countLiving NeighbourStates 0))
         "d"]
         [else "same"]))

(define (nextStatePos X Y)
  (let ([NeighbourStates (analyseNeighbours X Y)])
  (cond [(= 3 (countLiving NeighbourStates 0))
         "a"]
         [(< 3 (countLiving NeighbourStates 0))
         "d"]
         [(> 2 (countLiving NeighbourStates 0))
         "d"]
         [else "same"])))
;this function gives back how many Cells are alive in a list of cells
(define (countLiving NeighbourStates n)
  (if (< 0 (length NeighbourStates))
  (if (eq? (car NeighbourStates) "a")
      (countLiving (cdr NeighbourStates)  (+ n 1))
      (countLiving (cdr NeighbourStates) n))
  n))

;this functions finds the compllete new game state for one game state according to the rules using the function that does this for one state on all states
(define (nextGameState X oldGameState)
  (if (< 0  (length oldGameState) )
      (cons 
   (cons (cons X '()) (nextGameStateRow X (cdr (car oldGameState)))) (nextGameState (+ X 1) (cdr oldGameState)))
  '()))
;this functions find the new game state of one row using the rules and the old row
(define (nextGameStateRow X oldGameStateRow)
  (if (< 1 (length oldGameStateRow))
       (flatten (cons   (flatten (cons
                                  (car oldGameStateRow)
                                  (nextGameStatePlace X    (car oldGameStateRow) (car (cdr oldGameStateRow)))))
                        (nextGameStateRow  X (cddr oldGameStateRow))))
      '()))
;this function finds one new state using the rules and the old states
(define (nextGameStatePlace X Y Current)
  (let ([NeighbourStates (analyseNeighboursWithEdge X Y)])
  (cond [(= 3 (countLiving NeighbourStates 0))
         "a"]
         [(< 3 (countLiving NeighbourStates 0))
         "d"]
         [(> 2 (countLiving NeighbourStates 0))
         "d"]
         [else (cond
                 [(eq? Current "a") "a"]
                 [else "d"])
          ])))
;this function is like the old analysNeighbour function only that we also deal with cells wich have less then 8 neihbours
(define (analyseNeighboursWithEdge X Y)
  (cond [(= 0 X Y) 
         (list
          (analyseNeighbour (+ X 1) (+ Y 1))
          (analyseNeighbour X (+ Y 1))
          (analyseNeighbour (+ X 1) Y))]
        [(= 30 X Y)
         (list
          (analyseNeighbour (- X 1) (- Y 1))
          (analyseNeighbour X (- Y 1))
          (analyseNeighbour (- X 1) Y))]
        [(and (= 30 Y) (= 0 X))
         (list
          (analyseNeighbour (+ X 1) (- Y 1))
          (analyseNeighbour X (- Y 1))
          (analyseNeighbour (+ X 1) Y))]
        [(and (= 30 X) (= 0 Y))
         (list
          (analyseNeighbour (+ Y 1) (- X 1))
          (analyseNeighbour Y (- X 1))
          (analyseNeighbour (+ Y 1) X))]
        [(= 0 X) 
         (oneEdge X Y +)]
        [(= 0 Y)
         (oneEdge Y X +)]
        [ (= 30 X) 
         (oneEdge X Y -)]
        [ (= 30 Y) 
         (oneEdge Y X -)]
        [else (analyseNeighbours X Y)]))
;this function deals with the cases of the analyseNeighboursWithEdges in wich we have cells with 5 neighbours 
(define (oneEdge Edge NotEdge op )
         (list
          (analyseNeighbour (op Edge 1) (+ NotEdge 1))
          (analyseNeighbour Edge (+ NotEdge 1))
          (analyseNeighbour (op Edge 1) NotEdge)
          (analyseNeighbour (op Edge 1) (- NotEdge 1))
          (analyseNeighbour Edge (- NotEdge 1))))
;;2.4
(require 2htdp/universe)
;a start for the animation
(define gamestart
  (gameStart 0 30))
;the function that does the animation
(define (gameOfLife-sim tick)
  (big-bang
   (make-gameOfLife-universe gamestart)
   (on-tick next-frame tick)
   (to-draw show-gameOfLife-world 300 300)
   (name "Game of Life2")))
;our universe it takes a state as input
(define-struct gameOfLife-universe (state))
;the function that determines what happens on each tick
(define (next-frame world)
         (let ([new-world (make-gameOfLife-universe
                           (nextGameState 0 (gameOfLife-universe-state world)))])
           new-world))
;the fucntion that draws each picture
(define (show-gameOfLife-world world)
  (currentState (gameOfLife-universe-state world)))
;used by the show.. function to draw a picture
(define (currentState state)
  (drawCurrentState 0 state))
;used by currentState to draw a picture
(define (drawCurrentState start state)
  (if (<= start  30 )
      (above (drawRow (cdr (assoc (cons start '()) state)) 0 30) (drawCurrentState (+ 1 start) state))
      (rectangle 0 0 "solid" "black")))

;the call for the animation (the calculation is reaaaaaaaaaaaaaaaaaaally slow so it takes a while for each picture to show          
(animate( gameOfLife-sim 1))