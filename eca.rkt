#lang racket
(require pict)

(define (zeros n)
  (if (zero? n)
      '()
      (cons 0 (zeros (- n 1)))))

(define (next-cells prev-cells rule)
  (letrec ((wrap
            (lambda (l)
              (let wrap-loop ((reml l) (f (car l)) (k (lambda (x) x)))
                (if (null? (cdr reml))
                    (cons (car reml)
                          (k (list (car reml) f)))
                    (wrap-loop (cdr reml)
                               f
                               (lambda (x)
                                 (k (cons (car reml) x))))))))
           (window
            (lambda (cells)
              (take cells 3))))
    (let ((wrapped-cells (wrap prev-cells)))
      (let slide-window ((rem-cells wrapped-cells))
        (if (null? (cdddr rem-cells))
            (cons (rule (window rem-cells)) '())
            (cons (rule (window rem-cells))
                  (slide-window (cdr rem-cells))))))))

(define (make-rule n)
  (letrec ((dec->bits
            (lambda (d)
              (let loop ((n d) (bits '()))
                (if (zero? n)
                    bits
                    (loop (floor (/ n 2))
                          (cons (remainder n 2) bits))))))
           (make-3-bit-pattern
            (lambda (pat)
              (append (zeros (- 3 (length pat))) pat))))
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
                    (cdr new-state))))))))

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
  (letrec ((list-replace
            (lambda (l k v)
              (if (zero? k)
                  (cons v (cdr l))
                  (cons (car l)
                        (list-replace (cdr l) (- k 1) v))))))
    (let ((centre (floor (/ n 2))))
      (list-replace (zeros n) centre 1))))

(define (initialise-random n)
  (if (= n 1)
      (list (random 2))
      (cons (random 2)
            (initialise-random (- n 1)))))

(define (print-run x)
  (letrec ((print-cells
            (lambda (cells)
              (if (null? cells)
                  (newline)
                  (if (zero? (car cells))
                      (begin
                        (display " ")
                        (print-cells (cdr cells)))
                      (begin
                        (display "#")
                        (print-cells (cdr cells))))))))
    (for-each print-cells x)))

(define (draw-run x)
  (let* ((cellx 2)
         (celly 2)
         (empty-cell (blank cellx celly))
         (filled-cell (filled-rectangle cellx celly)))
    (letrec ((draw-cells
              (lambda (cells)
                (if (null? (cdr cells))
                    (if (zero? (car cells))
                        empty-cell
                        filled-cell)
                    (if (zero? (car cells))
                        (hc-append empty-cell
                                   (draw-cells (cdr cells)))
                        (hc-append filled-cell
                                   (draw-cells (cdr cells))))))))
      (let loop ((rem x))
        (if (null? (cdr rem))
            (draw-cells (car rem))
            (vc-append (draw-cells (car rem))
                       (loop (cdr rem))))))))

(define (save-pict p name)
  (send (pict->bitmap p) save-file name 'png))

;; (define initial-cells (initialise-random 256)) ; first generation of cells
;; (define iterations 256)                        ; number of iterations
;; (define rule-110 (make-rule 110))              ; generate rule

;; (print-run (run rule-110 initial-cells iterations)) ; print with ASCII
;; (draw-run (run rule-110 initial-cells iterations))  ; draw with pict

;; Save pict to .png
;; (save-pict (draw-run (run rule-110 initial-cells iterations)) "rule-110.png")