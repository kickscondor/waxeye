;; Waxeye Parser Generator
;; www.waxeye.org
;; Copyright (C) 2008-2010 Orlando Hill
;; Licensed under the MIT license. See 'LICENSE' for details.

(module
tester
scheme

(require (lib "ast.ss" "waxeye") "gen.scm" "interp.scm" "scheme.scm")
(provide tester)


(define *num-pass* 0)
(define *num-fail* 0)


(define (tester grammar tests)
  (define read-tests
    (lambda (i)
      (let ((test (read i)))
        (unless (eof-object? test)
                (start-nt! (symbol->string (car test)) grammar)
                (run-test-iter (dynamic-parser grammar) (cdr test))
                (read-tests i)))))
  (eof-check! #t)
  (set! *num-pass* 0)
  (set! *num-fail* 0)
  (call-with-input-file tests read-tests)
  (displayln "Waxeye Grammar Tester")
  (displayln "------------------------------------------------------------------------------")
  (let* ((t-count (+ *num-pass* *num-fail*))
         (cl (string->list (number->string (exact->inexact (/ (* *num-pass* 100) t-count)))))
         (cent (list->string (take cl (min (length cl) 5)))))
    (display (format "passed ~a | failed ~a | success ~a%\n" *num-pass* *num-fail* cent)))
  (displayln "------------------------------------------------------------------------------")
  (when (positive? *num-fail*) (exit 1)))


(define (run-test-iter parser pairs)
  (unless (null? pairs)
          (run-test parser (car pairs) (cadr pairs))
          (run-test-iter parser (cddr pairs))))


(define (run-test parser input expect)
  (let ((result (parser input)))
    (if (cond
         ((ast? result)
          (or (equal? expect 'pass) (is-expected? result expect)))
         ((parse-error? result)
          (equal? expect 'fail))
         ((equal? result #t)
          (equal? expect 'pass)))
        (set! *num-pass* (+ *num-pass* 1))
        (begin
          (set! *num-fail* (+ *num-fail* 1)) 
          (report-error input expect result)))))


(define (report-error input expect actual)
  (displayln (format "Error! @ ~s" *start-name*) (current-error-port))
  (displayln (format "input    = ~s" input) (current-error-port))
  (displayln (format "expected = ~s" expect) (current-error-port))
  (display "actual   = " (current-error-port))
  (if (ast? actual)
      (displayln (ast->string-sexpr actual) (current-error-port))
      (displayln (if (parse-error? actual)
                   'fail
                   'pass)
                   (current-error-port))))


(define (is-expected? result expect)
  (cond
   ((and (ast? result) (list? expect))
    (let ((type (car expect)) (child (cdr expect)))
      (or (equal? type '*)
          (and (equal? (ast-t result) type) (children-match? (ast-c result) child)))))
   ((and (char? result) (char? expect) (char=? result expect)))
   (else #f)))


(define (children-match? res expect)
  (if (null? res)
      (or (null? expect) (equal? (car expect) '*))
      (and (not (null? expect))
           (or (equal? (car expect) '*)
               (and (is-expected? (car res) (car expect))
                    (children-match? (cdr res) (cdr expect)))))))

)
