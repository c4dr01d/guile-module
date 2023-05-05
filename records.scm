(define-module (records)
  #:use-module (srfi srfi-9)
  #:export (define-record-type*))
(define-syntax-rule (define-record-type* type
                      constructor keyword-constructor
                      predicate
                      (field getter default) ...)
  (begin
    (define-record-type type
      (constructor field ...)
      predicate
      (field getter) ...)
    (define keyword-constructor
      (let ((default-record (constructor default ...)))
        (lambda* (#:key (inherit default-record)
                        (field (getter inherit)) ...)
          (constructor field ...))))))
