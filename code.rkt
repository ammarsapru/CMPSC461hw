#lang racket/base

;;-----------------------------------------------------------------------------
;; Problem 1: Working with Data
;;-----------------------------------------------------------------------------

;; Part 1.1: clamp-bounds
(define (clamp-bounds lst lower upper)
  (if (null? lst)
      '()
      (let ((current (car lst))
            (rest (clamp-bounds (cdr lst) lower upper)))
        (cond
          ((< current lower) (cons lower rest))
          ((> current upper) (cons upper rest))
          (else (cons current rest))))))

;; Part 1.2: cleanup-data
(define (cleanup-data lst)
  (if (null? lst)
      '()
      (let ((item (car lst))
            (rest-cleaned (cleanup-data (cdr lst))))
        (cond
          ((boolean? item) rest-cleaned)
          ((string? item) (cons (string-upcase item) rest-cleaned))
          ((number? item)
           (if (even? item)
               (cons (/ item 2) rest-cleaned)
               (cons (- (* item 3) 1) rest-cleaned)))
          ((list? item) (append (cleanup-data item) rest-cleaned))
          (else rest-cleaned))))) ; Should not happen based on problem description

;; Part 1.3: lesser-descendents
(define (find-lesser-in-list value-to-compare lst)
  (if (null? lst)
      '()
      (let ((current (car lst))
            (rest (find-lesser-in-list value-to-compare (cdr lst))))
        (if (< current value-to-compare)
            (cons current rest)
            rest))))

(define (lesser-descendents lst)
  (if (null? lst)
      '()
      (let ((current-val (car lst))
            (remaining-list (cdr lst)))
        (cons
         (list current-val (find-lesser-in-list current-val remaining-list))
         (lesser-descendents remaining-list)))))

;; Part 1.4: windowed-average
(define (take lst n)
  (if (or (= n 0) (null? lst))
      '()
      (cons (car lst) (take (cdr lst) (- n 1)))))

(define (sum lst)
  (if (null? lst)
      0
      (+ (car lst) (sum (cdr lst)))))

(define (windowed-average lst N)
  (cond
    ((< N 1) '())
    ((> N (length lst)) '())
    (else
     (let ((current-window (take lst N)))
       (cons (/ (sum current-window) N)
             (windowed-average (cdr lst) N))))))

;;-----------------------------------------------------------------------------
;; Problem 2: Dealing with Durations
;;-----------------------------------------------------------------------------

;; Part 2.1: pad-duration
(define (pad-duration dur)
  (let ((len (length dur)))
    (cond
      ((= len 3) dur)
      ((= len 2) (cons 0 dur))
      ((= len 1) (cons 0 (cons 0 dur)))
      (else '())))) ; Assuming valid inputs of length 1, 2, or 3

;; Part 2.2: fmt-duration
(define (pluralize value unit)
  (if (= value 1)
      (string-append (number->string value) " " unit)
      (string-append (number->string value) " " unit "s")))

(define (join-strings lst sep)
  (if (or (null? lst) (null? (cdr lst)))
      (if (null? lst) "" (car lst))
      (string-append (car lst) sep (join-strings (cdr lst) sep))))

(define (fmt-duration dur)
  (let* ((padded (pad-duration dur))
         (h (car padded))
         (m (cadr padded))
         (s (caddr padded))
         (h-str (if (> h 0) (list (pluralize h "Hour")) '()))
         (m-str (if (> m 0) (list (pluralize m "Minute")) '()))
         (s-str (if (> s 0) (list (pluralize s "Second")) '())))
    (let ((parts (append h-str m-str s-str)))
      (if (null? parts)
          "0 Seconds"
          (join-strings parts ", ")))))

;; Part 2.3: add-durations
(define (simplify-duration dur)
    (if (and (= (car dur) 0) (not (null? (cdr dur))))
        (if (and (= (cadr dur) 0) (not (null? (cddr dur))))
            (cddr dur)
            (cdr dur))
        dur))

(define (add-durations dur1 dur2)
  (let* ((pd1 (pad-duration dur1))
         (pd2 (pad-duration dur2))
         (total-s (+ (caddr pd1) (caddr pd2)))
         (total-m (+ (cadr pd1) (cadr pd2)))
         (total-h (+ (car pd1) (car pd2)))
         (s-rem (remainder total-s 60))
         (m-carry (quotient total-s 60))
         (new-m (+ total-m m-carry))
         (m-rem (remainder new-m 60))
         (h-carry (quotient new-m 60))
         (new-h (+ total-h h-carry)))
    (let ((result (list new-h m-rem s-rem)))
        (if (equal? result '(0 0 0))
            '(0)
            (simplify-duration result)))))


;;-----------------------------------------------------------------------------
;; Problem 3: Curried Calculations
;;-----------------------------------------------------------------------------

(define menu
  '(("vegetable samosa" . 4.99)
    ("aloo tikki" . 5.35)
    ("paneer pakora" . 6.50)
    ("fish curry" . 16.99)
    ("tikka masala" . 16.35)
    ("chicken saag" . 15.00)
    ("daal tadka" . 13.10)
    ("malai kofta" . 14.35)
    ("mango lassi" . 5.00)
    ("spiced tea" . 3.50)
    ("coffee" . 3.50)))

(define (get-price item menu-al)
  (let ((pair (assoc item menu-al)))
    (if pair (cdr pair) 0)))

(define (get-discount item discount-list)
  (let ((pair (assoc item discount-list)))
    (if pair (cadr pair) 0)))

(define (calculate-subtotal order discounts)
  (define (calculate-order-cost current-order)
    (if (null? current-order)
        0
        (let ((first-item (car current-order)))
          (if (number? first-item)
              (let* ((quantity first-item)
                     (item-name (cadr current-order))
                     (base-price (get-price item-name menu))
                     (discount-percent (get-discount item-name discounts))
                     (item-cost (* quantity base-price (- 1 (/ discount-percent 100)))))
                (+ item-cost (calculate-order-cost (cddr current-order))))
              (let* ((quantity 1)
                     (item-name first-item)
                     (base-price (get-price item-name menu))
                     (discount-percent (get-discount item-name discounts))
                     (item-cost (* quantity base-price (- 1 (/ discount-percent 100)))))
                (+ item-cost (calculate-order-cost (cdr current-order))))))))
  (let ((total-cost (calculate-order-cost order)))
    (/ (round (* total-cost 100)) 100.0)))
