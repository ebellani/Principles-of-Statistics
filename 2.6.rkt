#lang racket
;; In a certain survey of the work of chemical research workers, it was
;; found, on the basis of extensive data, that on average each man
;; required no fume cupboard for 60 per cent of his time, one cupboard
;; for 30 per cent and two cupboards for 10 per cent; three or more were
;; never required. If a group of four chemists worked independently of
;; one another, how many fume cupboards should be availabe in order to
;; provode adequate facilities for at least 95 per cent of the time?
(require "cartesian-product.rkt"
         rackunit
         rackunit/text-ui)

(define probability-of-cupboards
  #hash((0 . 0.6)
        (1 . 0.3)
        (2 . 0.1)))


;; how-many-cupboards-for-% : integer number hash -> number
;; given a minimum % and a table of probabilities, find the number of
;; cupboards that will be adequated for the number of people given.
(define (how-many-cupboards-for-% number-of-people
                                  minimum-%
                                  table-of-probabilities)
  (local [(define possibilities
            (sort (hash-keys table-of-probabilities) <))
          (define (accumulate-trials-probabilities
                   trials
                   accumulated-probabilities)
            (if (empty? trials)
                accumulated-probabilities
                (accumulate-trials-probabilities
                 (rest trials)
                 (update-or-insert-probability
                  accumulated-probabilities
                  (foldl (λ (x y)
                            (+ x y))
                         0 (first trials))
                  (foldl (λ (trial-event probability-of-trial)
                            (* (hash-ref table-of-probabilities
                                         trial-event)
                               probability-of-trial))
                         1.0 (first trials))))))]
         (probability-table->result-with-%-greater-than 
          (accumulate-trials-probabilities
           (cartesian-product (make-list
                               number-of-people
                               possibilities))
           (hash))
          minimum-%)))

;; probability-table->result-with-%-greater-than : hash number -> number or false
;; takes a probability table with the accumulated results, adds up then
;; in sequence until it surpasses the threshold. False if there is no it never
;; surpasses the threshold. 
(define (probability-table->result-with-%-greater-than table minimum-%)
  (define (accumulate-result list-of-possibilities
                             acc-probability
                             (last-probability #f))
    (cond ((empty? list-of-possibilities)
           (if (< acc-probability minimum-%) #f last-probability))
          ((> acc-probability minimum-%) last-probability)
          (else (accumulate-result (rest list-of-possibilities)
                                   (+ (hash-ref table
                                                (first list-of-possibilities))
                                      acc-probability)
                                   (first list-of-possibilities)))))
  (accumulate-result (sort (hash-keys table) <) 0))

;; update-or-insert-probability : hash integer number -> hash
(define (update-or-insert-probability table
                                      cupboard-number
                                      probability)
  (hash-update table
               cupboard-number
               (λ (old-probability)
                  (+ old-probability probability))
               0))

(define-test-suite cupboards
  (check-equal? (how-many-cupboards-for-% 4 0.95 probability-of-cupboards) 4)
  (check-equal? (probability-table->result-with-%-greater-than 
                 #hash((0 . 0.1296)
                       (1 . 0.2592)
                       (2 . 0.2808)
                       (3 . 0.1944)
                       (4 . 0.094)
                       (5 . 0.0324)) 0.94)
                4)
  (check-equal? (probability-table->result-with-%-greater-than 
                 #hash() 0.0)
                #f)
  (check-equal? (probability-table->result-with-%-greater-than 
                 #hash((0 . 0.4)
                       (1 . 0.2)) 0.7)
                #f))

(run-tests cupboards)
