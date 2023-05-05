(define-module (processes)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 popen)
  #:export (environment-excursion
            with-environment-excursion
            system*-no-output
            process-exists?))
(define (environment-excursion env-thunk body-thunk)
  (let ((old-env (environ)))
    (dynamic-wind
      env-thunk
      body-thunk
      (lambda () (environ old-env)))))
(define-syntax-rule (with-environment-excursion env body ...)
  (environment-excursion
    (lambda () (environ env))
    (lambda () body ...)))
(define* (process-exists? regexp #:key uid exact?)
  (let ((args `("pgrep" ,regexp
                ,@(if exact? '("--exact") '())
                ,@(if uid
                      (list "--uid" (number->string uid))
                      '()))))
    (zero? (status:exit-val (apply system* args)))))
(define (system*-no-output . args)
  (let ((port (apply open-pipe* OPEN_READ args)))
    (read-string port)
    (close-pipe port)))
