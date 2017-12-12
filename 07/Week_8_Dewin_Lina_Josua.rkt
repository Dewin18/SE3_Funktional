#lang racket

(require racket/trace)

;Solution of exercise sheet 7,

;author:

;Dewin Bagci, 6815336
;Josua Spisak, 6944594
;Lina Kaine, 6499396

;;1
;naive-ecursive function
(define (count-naive-recursive number list counter)
  (if (= 0 (length list))
      counter
      (if (= number (car list))
          (+  1 (count-naive-recursive number (cdr list) counter))
          (count-naive-recursive number (cdr list) counter))))

;tail-recursive function
(define (count-tail-recursive number list)
  (count-acc number list 0))

;accumulator for tail-recursive part
(define (count-acc number numberList counter)
  (if (= 0 (length numberList))
      counter
      (if (= number (car numberList))
          (count-acc number (cdr numberList) (+ counter 1))
          (count-acc number (cdr numberList) counter))))

;higher order function
(define (count-high number list function)
  (function number list 0))

;monitor functions
(trace count-naive-recursive)
(trace count-acc)
(trace count-high)

(count-naive-recursive  1 '(1 2 3 1 4 1 1 1 3 6 4 1) 0)
(count-tail-recursive 7 '(1 2 3 4 7 7 7 8 8 9))
(count-high 7 '(1 2 3 4 5 5 6 7 7 3 2 3 7) count-acc)



