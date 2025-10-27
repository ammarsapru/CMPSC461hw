#lang racket/base

;;-----------------------------------------------------------------------------
;; Problem 1: Working with Data
;;-----------------------------------------------------------------------------

;; Part 1.1: clamp-bounds
(define (clamp-bounds ub lb data)
  (if (null? data)
      '()
      (let ((current (car data))
            (rest (clamp-bounds ub lb (cdr data))))
        (cond
          ((< current lb) (cons lb rest))
          ((> current ub) (cons ub rest))
          (else (cons current rest))))))

;; Part 1.2: cleanup-data
(define (cleanup-data data)
  (if (null? data)
      '()
      (let ((item (car data))
            (rest-cleaned (cleanup-data (cdr data))))
        (cond
          ((boolean? item) rest-cleaned)
          ((string? item) (cons (string-upcase item) rest-cleaned))
          ((number? item)
           (if (even? item)
               (cons (/ item 2) rest-cleaned)
               (cons (- (* item 3) 1) rest-cleaned)))
          ((list? item) (append (cleanup-data item) rest-cleaned))
          (else rest-cleaned)))))

;; Part 1.3: lesser-decadents
(define (find-lesser-in-list value-to-compare lst)
  (if (null? lst)
      '()
      (let ((current (car lst))
            (rest (find-lesser-in-list value-to-compare (cdr lst))))
        (if (< current value-to-compare)
            (cons current rest)
            rest))))

(define (lesser-decadents data)
  (if (null? data)
      '()
      (let ((current-val (car data))
            (remaining-list (cdr data)))
        (cons
         (list current-val (find-lesser-in-list current-val remaining-list))
         (lesser-decadents remaining-list)))))

;; Part 1.4: windowed-average (CORRECTED)
(define (take lst n)
  (if (or (= n 0) (null? lst))
      '()
      (cons (car lst) (take (cdr lst) (- n 1)))))

(define (sum lst)
  (if (null? lst)
      0
      (+ (car lst) (sum (cdr lst)))))

(define (windowed-average size data)
  (cond ((< size 1) '()) ;<- FIX: Handles size 0 or less
        ((< (length data) size) '())
        (else (cons (/ (sum (take data size)) size)
                    (windowed-average size (cdr data))))))

;;-----------------------------------------------------------------------------
;; Problem 2: Dealing with Duration
;;-----------------------------------------------------------------------------

;; Part 2.1: pad-duration
(define (pad-duration duration)
  (let ((len (length duration)))
    (cond
      ((= len 3) duration)
      ((= len 2) (cons 0 duration))
      ((= len 1) (cons 0 (cons 0 duration)))
      (else '(0 0 0)))))

;; Part 2.2: fmt-duration (CORRECTED)
(define (pluralize value unit)
  (if (= value 1)
      (string-append (number->string value) " " unit)
      (string-append (number->string value) " " unit "s")))

;; Helper to reverse a list (needed for complex join)
(define (my-reverse lst)
  (let loop ((original lst) (rev '()))
    (if (null? original)
        rev
        (loop (cdr original) (cons (car original) rev)))))

;; Helper to join list items with ", "
(define (join-with-comma lst)
  (if (null? (cdr lst))
      (car lst)
      (string-append (car lst) ", " (join-with-comma (cdr lst)))))

;; Helper that joins duration parts with "and" and ", and" correctly
(define (join-duration-parts parts)
  (cond ((= (length parts) 1) (car parts))
        ((= (length parts) 2) (string-append (car parts) " and " (cadr parts)))
        (else
         (let* ((rev-parts (my-reverse parts))
                (last-part (car rev-parts))
                (all-but-last (my-reverse (cdr rev-parts))))
           (string-append (join-with-comma all-but-last) ", and " last-part)))))

(define (fmt-duration duration)
    (let* ((padded (pad-duration duration))
           (h (car padded))
           (m (cadr padded))
           (s (caddr padded))
           (h-str (if (> h 0) (list (pluralize h "Hour")) '()))
           (m-str (if (> m 0) (list (pluralize m "Minute")) '()))
           (s-str (if (> s 0) (list (pluralize s "Second")) '())))
      (let ((parts (append h-str m-str s-str)))
        (if (null? parts)
            "0 Seconds"
            (join-duration-parts parts))))) ;<- FIX: Uses new join logic

;; Part 2.3: add-durations
(define (simplify-duration dur)
    (if (and (= (car dur) 0) (> (length dur) 1))
        (simplify-duration (cdr dur))
        (if (null? dur) '(0) dur)))

(define (add-durations leftDuration rightDuration)
  (let* ((pd1 (pad-duration leftDuration))
         (pd2 (pad-duration rightDuration))
         (total-s (+ (caddr pd1) (caddr pd2)))
         (total-m (+ (cadr pd1) (cadr pd2)))
         (total-h (+ (car pd1) (car pd2)))
         (s-final (remainder total-s 60))
         (m-carry (quotient total-s 60))
         (m-interim (+ total-m m-carry))
         (m-final (remainder m-interim 60))
         (h-carry (quotient m-interim 60))
         (h-final (+ total-h h-carry)))
    (simplify-duration (list h-final m-final s-final))))

;;-----------------------------------------------------------------------------
;; Problem 3: Curried Calculations (Matches Problem 4 in your template comment)
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

(define (get-price item menu-list)
  (let ((pair (assoc item menu-list)))
    (if pair (cdr pair) 0)))

(define (get-discount item discount-list)
  (let ((pair (assoc item discount-list)))
    (if pair (cadr pair) 0)))

(define (calculate-subtotal order discounts)
  (define (calculate-order-cost current-order)
    (if (null? current-order)
        0
        (let ((first (car current-order))
              (rest (cdr current-order)))
          (if (number? first)
              (let* ((quantity first)
                     (item-name (car rest))
                     (price (get-price item-name menu))
                     (discount (get-discount item-name discounts))
                     (item-total (* quantity price (- 1 (/ discount 100)))))
                (+ item-total (calculate-order-cost (cdr rest))))
              (let* ((quantity 1)
                     (item-name first)
                     (price (get-price item-name menu))
                     (discount (get-discount item-name discounts))
                     (item-total (* quantity price (- 1 (/ discount 100)))))
                (+ item-total (calculate-order-cost rest)))))))
  (let ((total-cost (calculate-order-cost order)))
    (/ (round (* total-cost 100)) 100.0)))
