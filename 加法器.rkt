;;  队列的表示
(define (make-queue) (cons '() '()))

(define (front-ptr queue) (car queue))

(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item)(set-car! queue item))

(define (set-rear-ptr! queue item)(set-cdr! queue item))

(define (empty-queue? queue)(null? (front-ptr queue)))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))
;; 入队
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

;; 出队
(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))

;; 反门
(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                    (lambda ()
                      (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

;; 逻辑取反
(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

;; 合门
(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
          (logical-and (get-signal a1) (get-signal a2))))
    (after-delay and-gate-delay
                 (lambda ()
                   (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

;; 逻辑合
(define (logical-and s1 s2)
  (cond ((and (= s1 1)(= s2 1)) 1)
        ((or (= s1 0)(= s2 0)) 0)
        (else (error "Invalid signal" s1 s2))))

;; 或门
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1)(get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

;; 逻辑或
(define (logical-or s1 s2)
  (cond ((or (= s1 1)(= s2 1)) 1)
        ((and (= s1 0)(= s2 0)) 0)
        (else (error "Invalid signal" s1 s2))))

;; 半加器
(define (half-adder a b s c)
  (let ((d (make-wire))(e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

;; 全加器
(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))


;; 模拟线路的表示
(define (make-wire)
  (let ((signal-value 0)(action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))

;; 局部过程set-my-signal!检查新的信号值是否实际改变了线路上的信号，如果真是这样，他就用call-each运行每个动作
(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(define (make-time-segment time queue)
  (cons time queue))

(define (segment-time s)(car s))

(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))

(define (current-time agenda)(car agenda))

(define (set-current-time! agenda time)
  (set-car! agenda time))

(define (segments agenda) (cdr agenda))

(define (set-segments! agenda segments)
  (set-cdr! agenda segments))

(define (first-segment agenda) (car (segments agenda)))

(define (rest-segments agenda) (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))

;; 为了将一个动作加入待处理表，我们首先要检查这个待处理表是否为空。如果真是这样，那么就创建一个新的时间段，并将这个时间段闯入待处理表里。
(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr!
               segments
               (cons (make-new-time-segment time action)
                     (cdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (cons (make-new-time-segment time action)
               segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))


(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display " New-value = ")
                 (display (get-signal wire)))))

;; 逐位进位加法器
(define (ripple-carry-adder a b c-in sum) 
     (if (not (null? a)) 
         (let ((carry (make-wire))) 
               (full-adder         (car a) (car b) c-in (car sum) carry)
               (ripple-carry-adder (cdr a) (cdr b) carry (cdr sum))))) 


 (define (get-signals wires) 
       (map (lambda (w) (get-signal w)) wires)) 

 (define (build-wires input-signals) 
         (if (null? input-signals) 
             '() 
             (let ((new-wire (make-wire))) 
                  (set-signal! new-wire (car input-signals)) 
                  (cons new-wire (build-wires (cdr input-signals)))))) 
;; 测试进位累加器
;  (define B (make-wire))
;((B 'set-signal!) 1)
;  (define C (make-wire))
;((C 'set-signal!) 0)
;  (define D (make-wire))
;((D 'set-signal!) 0)
;  (define E (make-wire))
; ((E 'set-signal!) 1)
;  (define F (make-wire))
; ((F 'set-signal!) 0)
;  (define G (make-wire))
;((G 'set-signal!) 0)
;(define testA (list B C ))
;(define testB (list D E ))
;(define testS (list F G ))
;(get-signals testA)
;(get-signals testB)
;(get-signals testS)
;(define A (make-wire))
 ;;(probe 'A A)
;(ripple-carry-adder testA testB A testS)
;(propagate)
;(get-signals testS)

 (define a (build-wires '(1 0 0 0 0 1))) ;;4 
 (define b (build-wires '(1 1 0 1 0 1))) ;;11 
 (define s (build-wires '(0 0 0 0 0 0))) 
 (define c-in (make-wire)) 
 (ripple-carry-adder a b c-in s) 
  
 (propagate) 
 (get-signals s) ;;should be 15

;(define input-2 (make-wire)) 
;(define sum (make-wire)) 
;(define carry (make-wire)) 
;(probe 'sum sum)
;(probe 'carry carry)
;(half-adder input-1 input-2 sum carry)
;(set-signal! input-1 1)
;(propagate)

;(set-signal! input-2 1)
;(propagate)