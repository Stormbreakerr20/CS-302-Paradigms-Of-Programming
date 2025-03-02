#lang scheme
; Q1
(define (f-rec n)
  (if (< n 3) n (+ (f-rec (- n 1)) (* 2 (f-rec (- n 2))) (* 3 (f-rec (- n 3))))))

(define (f-iter n)
  (if (< n 3)
      n
      (let iter ((a 0) (b 1) (c 2) (i (- n 3)))
        (if (< i 0)
            c
            (iter b c (+ c (* 2 b) (* 3 a)) (- i 1))))))

; Q2
(define (qck-split func pivot my-list)
  (if (null? my-list) '()
      (cond ((func (car my-list) pivot) (cons (car my-list) (qck-split func pivot (cdr my-list))))
            (else (qck-split func pivot (cdr my-list))))))
; Quick sort for only unique values
 (define (qck-sort-all-unique my-list)
   (if (null? my-list) '()
       (append (qck-sort-all-unique (qck-split < (car my-list) my-list)) (list (car my-list)) (qck-sort-all-unique (qck-split > (car my-list) my-list)))))

; main Quick sort
 (define (qck-sort my-list)
   ;helper
   (define (equals pivot my-list)
     (let ((x (qck-split = pivot my-list)))
       (if (null? (cdr x)) '() (cdr x))))
   
   (if (null? my-list) '()
       (append (qck-sort (qck-split < (car my-list) my-list)) (list (car my-list)) (qck-sort (append (qck-split > (car my-list) my-list) (equals (car my-list) my-list))))))

; Q3
(define (my-inc x) (+ x 1))
(define (proc x)
  (display x)
  (newline))

(define (do-while proc start limit my-inc)
  (proc start)
  
  (let ((new-start (my-inc start)))
    (if (<= new-start limit) (do-while proc new-start limit my-inc) 'Done)))

; Q4
(define square (lambda (x) (* x x)))
(define inc (lambda (x) (+ x 1)))
(define (rep-op func x)
  (define (helper proc times inp)
    (if (= times 1) (proc inp) (helper proc (- times 1) (proc inp))))
    
  (lambda (y)
    (helper func x y)))
