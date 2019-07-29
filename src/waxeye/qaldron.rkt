;; Waxeye Parser Generator
;; www.waxeye.org
;; Copyright (C) 2008-2010 Orlando Hill
;; Licensed under the MIT license. See 'LICENSE' for details.

#lang racket/base
(require         waxeye/ast
         waxeye/fa
         "code.rkt" "dfa.rkt" "gen.rkt")
(provide gen-qaldron)


(define (gen-qaldron grammar path)
  (indent-unit! 2)
  (let ((file-path (string-append path (if *name-prefix*
                                           (string-append (string-downcase *name-prefix*) "-parser.q")
                                           "parser.q"))))
    (dump-string (gen-parser grammar) file-path)
    (list file-path)))


(define (gen-trans a)
  (define (gen-char t)
    (format "\"~a~a\""
            (if (escape-for-java-char? t) "\\" "")
            (cond
             ((equal? t #\") "\\\"")
             ((equal? t #\linefeed) "\\n")
             ((equal? t #\tab) "\\t")
             ((equal? t #\return) "\\r")
             (else t))))
  (define (gen-char-class-item a)
    (if (char? a)
        (gen-char a)
        (format "~a..~a"
                (char->integer (car a))
                (char->integer (cdr a)))))
  (cond
   ((symbol? a) (format "'*~a'" a))
   ((list? a)
    (format "[~a~a]"
            (gen-char-class-item (car a))
            (apply string-append (map (lambda (b)
                                  (string-append ", " (gen-char-class-item b)))
                                (cdr a)))))
   ((char? a) (gen-char a))
   (else a)))


(define (gen-edge a)
  (format "edge(~a, ~a, ~a)"
          (gen-trans (edge-t a))
          (edge-s a)
          (bool->s (edge-v a))))


(define (gen-edges d)
  (gen-array gen-edge (list->vector d)))


(define (gen-state a)
  (format "state(~a, ~a)"
          (gen-edges (state-edges a))
          (bool->s (state-match a))))


(define (gen-states d)
  (gen-array gen-state d))


(define (gen-fa a)
  (format "fa('~a', ~a, '~a')"
          (let ((type (string-downcase (symbol->string (fa-type a)))))
            (cond
             ((equal? type "!") "*not")
             ((equal? type "&") "*and")
             (else type)))
          (gen-states (fa-states a))
          (case (fa-mode a)
            ((voidArrow) "void")
            ((pruneArrow) "prune")
            ((leftArrow) "left"))))


(define (gen-fas d)
  (gen-array gen-fa d))


(define (gen-array fn data)
  (let ((ss (vector->list data)))
    (format "[~a]"
            (indent (if (null? ss)
                        ""
                        (string-append (fn (car ss))
                                       (apply string-append (map (lambda (a)
                                                             (string-append ",\n" (ind) (fn a)))
                                                           (cdr ss)))))))))

(define (gen-use)
  (indent (format "
var (edge, fa, parser, state): use ('waxeye')
")))


(define (qaldron-comment lines)
  (comment-base ";" lines))


(define (gen-parser grammar)
  (let ((parser-name (if *name-prefix*
                         (string-append (string-downcase *name-prefix*) "-parser")
                         "parser")))
    (define (gen-parser-class)
       (format "~aparser (\n~a~a)\n"
               (ind)
               (indent (format
"~astart: ~a
~aeof: ~a
~aautomata: ~a
"
(ind)
*start-index*
(ind)
(bool->s *eof-check*)
(ind)
(gen-fas (make-automata grammar))))
               (ind)
               ))

    (format "~a~a\n~a"
            (if *file-header*
                (qaldron-comment *file-header*)
                (qaldron-comment *default-header*))
            (gen-use)
            (if *module-name*
                (format "module ~a\n~aend\n"
                        *module-name*
                        (indent (gen-parser-class)))
                (gen-parser-class)))))
