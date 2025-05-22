#lang racket
(require racket/mpair)

;; Question 1

(define (eval-infix expr)
  (cond
    [(number? expr) expr]
    [(list? expr)
     (let* ([left (eval-infix (first expr))]
            [op (second expr)]
            [right (eval-infix (third expr))])
       (case op
         [(+) (+ left right)]
         [(-) (- left right)]
         [(*) (* left right)]
         [(/) (quotient left right)]
         [else (error "Unknown operator: " op)]))]
    [else (error "Invalid expression")]))

;; Question 2
(define (check-infi-loop mp-list)
  (define (advance slow fast)
    (cond
      [(or (null? fast) (null? (mcdr fast))) #f]
      [(eq? slow fast) #t]
      [else (advance (mcdr slow) (mcdr (mcdr fast)))]))
  (if (or (null? mp-list) (null? (mcdr mp-list)))
      #f
      (advance mp-list (mcdr mp-list))))

;; Question 3

(define (make-deque)
  (mcons null null))  ;; head and tail pointers

(define (empty-deque? dq)
  (null? (mcar dq)))

(define (front-deque dq)
  (if (empty-deque? dq)
      (error "Deque is empty")
      (mcar (mcar dq))))

(define (rear-deque dq)
  (if (empty-deque? dq)
      (error "Deque is empty")
      (mcar (mcdr dq))))

(define (front-insert-deque! dq val)
  (let ([node (mcons val null)])
    (cond
      [(empty-deque? dq)
       (set-mcar! dq node)
       (set-mcdr! dq node)]
      [else
       (set-mcdr! node (mcar dq))
       (set-mcar! dq node)])))

(define (rear-insert-deque! dq val)
  (let ([node (mcons val null)])
    (cond
      [(empty-deque? dq)
       (set-mcar! dq node)
       (set-mcdr! dq node)]
      [else
       (set-mcdr! (mcdr dq) node)
       (set-mcdr! dq node)])))

(define (front-delete-deque! dq)
  (if (empty-deque? dq)
      (error "Deque is empty")
      (let ([new-front (mcdr (mcar dq))])
        (if (null? new-front)
            (begin (set-mcar! dq null) (set-mcdr! dq null))
            (set-mcar! dq new-front)))))

(define (rear-delete-deque! dq)
  (if (empty-deque? dq)
      (error "Deque is empty")
      (let ([start (mcar dq)]
            [last (mcdr dq)])
        (if (eq? start last)
            (begin (set-mcar! dq null) (set-mcdr! dq null))
            (let loop ([ptr start])
              (if (eq? (mcdr ptr) last)
                  (begin
                    (set-mcdr! ptr null)
                    (set-mcdr! dq ptr))
                  (loop (mcdr ptr))))))))

;; Question 4

;; Helper: creates a serializer (mutex)
(define (make-serializer)
  (let ([lock (mcons #f null)])
    (define (serializer proc)
      (lambda args
        (when (mcar lock) (serializer proc)) ;; wait if locked
        (set-mcar! lock #t)
        (define result (apply proc args))
        (set-mcar! lock #f)
        result))
    serializer))

;; Account constructor with unique ID and balance
(define (make-account id bal)
  (let ([balance bal]
        [acc-id id]
        [ser (make-serializer)])
    (define (dispatch msg)
      (match msg
        ['withdraw (lambda (amt) (if (>= balance amt) (begin (set! balance (- balance amt)) balance) "Insufficient funds"))]
        ['deposit (lambda (amt) (set! balance (+ balance amt)) balance)]
        ['balance (lambda () balance)]
        ['id (lambda () acc-id)]
        ['serializer (lambda () ser)]
        [else (error "Unknown operation")]))
    dispatch))

;; Accessors
(define (acc-id a) ((a 'id)))
(define (acc-balance a) ((a 'balance)))
(define (acc-deposit a) (a 'deposit))
(define (acc-withdraw a) (a 'withdraw))
(define (acc-serializer a) ((a 'serializer)))

;; Serialized exchange avoiding deadlock
(define (serialized-exchange-nu a1 a2)
  (define (exchange from to)
    (let ([diff (- (acc-balance from) (acc-balance to))])
      ((acc-withdraw from) diff)
      ((acc-deposit to) diff)))

  (define first (if (< (acc-id a1) (acc-id a2)) a1 a2))
  (define second (if (< (acc-id a1) (acc-id a2)) a2 a1))

  (((acc-serializer first)
    ((acc-serializer second) exchange))
   a1 a2))
