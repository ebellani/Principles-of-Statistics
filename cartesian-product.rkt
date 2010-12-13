#lang racket

(require rackunit
         rackunit/text-ui)

(provide cartesian-product)

;; cartesian-product : (listof (listof X)) -> (listof (listof X))
(define (cartesian-product list-of-lists)
  (foldr (λ (an-element collected-so-far)
           (append (combine-element-with-lists an-element
                                               (rest list-of-lists))
                   collected-so-far))
         empty
         (first list-of-lists)))

;; combine-element-with-list : X (listof (listof X)) -> (listof (listof X))
(define (combine-element-with-lists element lists)
  (define (accumulate-product the-lists acc-product)
    (if (empty? the-lists)
        acc-product
        (accumulate-product (rest the-lists)
                            (for*/list ((i acc-product)
                                        (j (first the-lists)))
                              (append i (list j))))))
  
  (accumulate-product lists
                      (list (list element))))

(define-test-suite combination-tests
  (check-equal? (combine-element-with-lists 'a '((x y) (z)))
                '((a x z) (a y z)))
  
  (check-equal? (combine-element-with-lists '0 '((0 1) (0 1)))
                '((0 0 0)
                  (0 0 1)
                  (0 1 0)
                  (0 1 1))))


(define-test-suite cartesian-product-tests
  (check-equal? (cartesian-product '((A B C D) (x y)))
                '((A x) (A y) (B x) (B y) (C x) (C y) (D x) (D y)))

  (check-equal? (cartesian-product '((0 1) (0 1) (0 1)))
                '((0 0 0)
                  (0 0 1)
                  (0 1 0)
                  (0 1 1)
                  (1 0 0)
                  (1 0 1)
                  (1 1 0)
                  (1 1 1))))

;; (run-tests combination-tests)
;; (run-tests cartesian-product-tests)
