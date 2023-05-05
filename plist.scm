(define-module (plist)
  #:use-module (ice-9 match)
  #:export (plist-fold
            plist-get
            plist-add
            plist-delete
            plist-put
            plist-new))
(define (plist-fold proc init plist)
  (let loop ((result init)
             (current plist))
    (match current
      (()
       result)
      ((prop val rest ...)
       (loop (proc prop val result)
             rest)))))
(define (plist-get plist property)
  (match plist
    ((prop val rest ...)
     (if (eq? prop property)
         val
         (plist-get rest property)))
    (_ #f)))
(define (plist-add plist property value)
  (cons* property value plist))
(define (plist-delete plist property)
  (plist-fold (lambda (prop val res)
                (if (eq? prop property)
                    res
                    (plist-add res prop val)))
              '()
              plist))
(define (plist-put plist property value)
  (plist-add (plist-delete plist property)
             property value))
(define (plist-new old-plist . add-plist)
  (plist-fold (lambda (prop val res)
                (plist-put res prop val))
              old-plist
              add-plist))
