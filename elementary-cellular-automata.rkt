#lang racket
(require pict)

(define (zeros n)
  (if (zero? n)
      '()
      (cons 0 (zeros (- n 1)))))

(define (next-cells previous-cells rule)
  (define (wrap x)
    (let wrap-iter ((rem x)
                    (f (car x))
                    (dlist (lambda (x) x)))
      (if (null? (cdr rem))
          (cons (car rem)
                (dlist (list (car rem) f)))
          (wrap-iter (cdr rem)
                     f
                     (lambda (y)
                       (dlist (cons (car rem) y)))))))
  (define (window cells) (take cells 3))
  (let ((wrapped-cells (wrap previous-cells)))
    (let slide-window ((remaining-cells wrapped-cells))
      (if (null? (cdddr remaining-cells))
          (cons (rule (window remaining-cells)) '())
          (cons (rule (window remaining-cells))
                (slide-window (cdr remaining-cells)))))))

(define (make-rule n)
  (define (dec->bits d)
    (let loop ((n d)
               (bits '()))
      (if (zero? n)
          bits
          (loop (floor (/ n 2))
                (cons (remainder n 2) bits)))))
  (define (make-3-bit-pattern pattern)
    (append (zeros (- 3 (length pattern))) pattern))
  (let* ((rule (dec->bits n))
         (rule-length (length rule))
         (full-rule (append (zeros (- 8 rule-length)) rule)))
    (lambda (window)
      (let loop ((n 7)
                 (pattern (make-3-bit-pattern (dec->bits 7)))
                 (new-state full-rule))
        (if (equal? window pattern)
            (car new-state)
            (loop (- n 1)
                  (make-3-bit-pattern (dec->bits (- n 1)))
                  (cdr new-state)))))))

(define (run rule initial iterations)
  (if (zero? iterations)
      '()
      (cons initial
            (run rule
                 (next-cells initial rule)
                 (- iterations 1)))))

(define (initialise-right n)
  (if (zero? n)
      (list 1)
      (cons 0 (initialise-right (- n 1)))))

(define (initialise-left n)
  (cons 1 (zeros n)))

(define (initialise-centre n)
  (define (list-replace x k e)
    (if (zero? k)
        (cons e (cdr x))
        (cons (car x)
              (list-replace (cdr x) (- k 1) e))))
  (let ((centre (floor (/ n 2)))
        (initial-zeros (zeros n)))
    (list-replace initial-zeros centre 1)))

(define (initialise-random n)
  (if (= n 1)
      (list (random 2))
      (cons (random 2)
            (initialise-random (- n 1)))))

(define (print-run x)
  (define (print-cells cells)
    (if (null? cells)
        (newline)
        (if (zero? (car cells))
            (begin (display " ")
                   (print-cells (cdr cells)))
            (begin (display "#")
                   (print-cells (cdr cells))))))
  (for-each print-cells x))

(define (draw-run x)
  (define cell-x 2)
  (define cell-y 2)
  (define (empty-cell) (blank cell-x cell-y))
  (define (filled-cell) (filled-rectangle cell-x cell-y))
  (define (draw-cells cells)
    (if (null? (cdr cells))
        (if (zero? (car cells))
            (empty-cell)
            (filled-cell))
        (if (zero? (car cells))
            (hc-append (empty-cell)
                       (draw-cells (cdr cells)))
            (hc-append (filled-cell)
                       (draw-cells (cdr cells))))))
  (let recurse ((remaining x))
    (if (null? (cdr remaining))
        (draw-cells (car remaining))
        (vc-append (draw-cells (car remaining))
                   (recurse (cdr remaining))))))

(define (save-pict p name)
  (send (pict->bitmap p) save-file name 'png))

; (define initial-cells (initialise-random 256)) ; first generation of cells
; (define iterations 256)                        ; number of iterations
; (define rule-110 (make-rule 110))              ; generate rule

; (print-run (run rule-110 initial-cells iterations)) ; print with ASCII
; (draw-run (run rule-110 initial-cells iterations))  ; draw with pict

; Save pict to .png
; (save-pict (draw-run (run rule-110 initial-cells iterations)) "rule-110.png")
