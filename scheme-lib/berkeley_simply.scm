(define defmacro
  (macro (params body)
    (let ((name (car params))
          (args (cdr params)))
      (list 'define name
            (list 'macro args body)))))

(define define-macro defmacro)

;;; === berkeley.scm === 4.03.01 Tue Jul 22 17:22:59 2003

(define nil '())
(define true #t)
(define false #f)


;;; SICP stuff:

(define (print x)
  (display x)
  (newline))

;; Define tagged data ADT:

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

;;For Section 3.1.2 -- written as suggested in footnote,
;; though the values of a, b, m may not be very "appropriately chosen"
(define (rand-update x)
  (let ((a 27) (b 26) (m 127))
    (modulo (+ (* a x) b) m)))

;;For Section 3.3.4, used by and-gate
;;Note: logical-and should test for valid signals, as logical-not does
(define (logical-and x y)
  (if (and (= x 1) (= y 1))
      1
      0))

;;For Section 3.5.2, to check power series (exercises 3.59-3.62)
;;Evaluate and accumulate n terms of the series s at the given x
;;Uses horner-eval from ex 2.34
(define (eval-power-series s x n)
  (horner-eval x (first-n-of-series s n)))
(define (first-n-of-series s n)
  (if (= n 0)
      '()
      (cons (stream-car s) (first-n-of-series (stream-cdr s) (- n 1)))))

;; Streams:
;; 4.0: We do not redefine promises as procedures anymore.

(define (cadr x) (car (cdr x)))

;; need define-macro!!
(define cons-stream
  (macro args
    `(cons ,(car args) (delay ,(cadr args)))))

(define (stream-car stream) (car stream))

(define (stream-cdr st)
  (force (cdr st)))

(define the-empty-stream '())

(define (stream-null? stream) (eq? stream the-empty-stream))

(define (stream? obj)
  (or (stream-null? obj)
      (and (pair? obj) (promise? (cdr obj)))))

(define (stream-accumulate combiner initial-value stream)
  (if (stream-null? stream)
      initial-value
      (combiner (stream-car stream)
                (stream-accumulate combiner
                                   initial-value
                                   (stream-cdr stream)))))

(define (accumulate-delayed combiner initial-value stream)
  (if (stream-null? stream)
      initial-value
      (combiner (stream-car stream)
                (delay
                  (accumulate-delayed combiner
                                      initial-value
                                      (stream-cdr stream))))))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (interleave-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream (stream-car s1)
                   (interleave-delayed (force delayed-s2)
                                       (delay (stream-cdr s1))))))

(define (stream-flatten stream)
  (accumulate-delayed interleave-delayed
                      the-empty-stream
                      stream))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc . s)
  (if (stream-null? (car s))
      the-empty-stream
      (cons-stream (apply proc (map stream-car s))
                   (apply stream-map proc (map stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin
        (proc (stream-car s))
        (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each
   (lambda (element) (newline) (display element))
   s))

(define (stream-flatmap f s)
  (stream-flatten (stream-map f s)))

(define (stream-append s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (stream-append (stream-cdr s1) s2))))

(define (list->stream l)
  (if (null? l)
      the-empty-stream
      (cons-stream (car l) (list->stream (cdr l))) ))

(define make-stream
  (lambda elements
    (list->stream elements)))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low (stream-enumerate-interval (+ low 1) high))))

(define range stream-enumerate-interval)

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (show-stream strm . args)
  (if (null? args)
      (ss1 strm 10 10)
      (ss1 strm (car args) (car args))))

(define ss show-stream)

(define (ss1 strm this all)
  (cond ((null? strm) '())
        ((= this 0) '(...))
        ((and (pair? strm) (promise? (cdr strm)))
         (cons (ss1 (stream-car strm) all all)
               (ss1 (stream-cdr strm) (- this 1) all)))
        (else strm)))

(define div quotient)

;; 4.0: STk has the RRRS 1+ and -1+ built in; we removed their definitions
;; from here.

(define (nth n l) (list-ref l n))

;;;  Get and put for section 2.3

(define (get key1 key2)
  (let ((subtable (assoc key1 (cdr the-get/put-table))))
    (if (not subtable)
        #f
        (let ((record (assoc key2 (cdr subtable))))
          (if (not record)
              #f
              (cdr record))))))

(define (put key1 key2 value)
  (let ((subtable (assoc key1 (cdr the-get/put-table))))
    (if (not subtable)
        (set-cdr! the-get/put-table
                  (cons (list key1
                              (cons key2 value))
                        (cdr the-get/put-table)))
        (let ((record (assoc key2 (cdr subtable))))
          (if (not record)
              (set-cdr! subtable
                        (cons (cons key2 value)
                              (cdr subtable)))
              (set-cdr! record value)))))
  'ok)

(define the-get/put-table (list '*table*))

;; 02/02/2005 : Added as requested by Professors
(define (1+ n) (+ n 1))
(define (-1+ n) (- n 1))
(define (1- n) (- n 1))


;;; === simply.scm ===

;; Tells us which file's (simply or explorin) definitions take precedence
;; if they conflict

(define (explorinOrSimply)
  "simply"
  )

;;; This file uses Scheme features we don't talk about in _Simply_Scheme_.
;;; Read at your own risk.

;; Get strings in error messages to print nicely (especially "")

(define whoops
  (let ((string? string?)
        (string-append string-append)
        (error error)
        (cons cons)
        (map map)
        (apply apply))
    (define (error-printform x)
      (string x " "))
    (lambda args
      (error (string->symbol
              (apply string-append
                     (map error-printform args)))))))


;;; Logo-style word/sentence implementation

;; (define word?
;;   (let ((number? number?)
;;         (symbol? symbol?)
;;         (string? string?))
;;     (lambda (x)
;;       (or (symbol? x) (number? x) (string? x)))))

(define sentence?
  (let ((null? null?)
        (pair? pair?)
        (word? word?)
        (car car)
        (cdr cdr))
    (define (list-of-words? l)
      (cond ((null? l) #t)
            ((pair? l)
             (and (word? (car l)) (list-of-words? (cdr l))))
            (else #f)))
    list-of-words?))

(define empty?
  (let ((null? null?)
        (string? string?)
        (string=? string=?))
    (lambda (x)
      (or (null? x)
          (and (string? x) (string=? x ""))))))


(define (char-rank char)
  ;; 0 Letter in good case or special initial
  ;; 1 ., + or -
  ;; 2 Digit
  ;; 3 Letter in bad case or weird character
  (let ((*the-char-ranks* (make-vector 256 3))
        (= =)
        (+ +)
        (string-ref string-ref)
        (string-length string-length)
        (vector-set! vector-set!)
        (char->integer char->integer)
        (symbol->string symbol->string)
        (vector-ref vector-ref))
    (define (rank-string str rank)
      (define (helper i len)
        (if (= i len)
            'done
            (begin (vector-set! *the-char-ranks*
                                (char->integer (string-ref str i))
                                rank)
                   (helper (+ i 1) len))))
      (helper 0 (string-length str)))
    (rank-string (symbol->string 'abcdefghijklmnopqrstuvwxyz) 0)
    (rank-string "!$%&*/:<=>?~_^" 0)
    (rank-string "+-." 1)
    (rank-string "0123456789" 2)
    ;; value of char-rank
    (vector-ref *the-char-ranks* (char->integer char))))

;; (define string->word
;;   (let ((= =) (<= <=) (+ +) (- -)
;;         (char-rank char-rank)
;;         (string-ref string-ref)
;;         (string-length string-length)
;;         (string=? string=?)
;;         (not not)
;;         (char=? char=?)
;;         (string->number string->number)
;;         (string->symbol string->symbol))
;;     (lambda (string)
;;       (define (subsequents? string i length)
;;      (andmap (lambda (c) (<= (char-rank c) 2))
;;              (string->list string)))
;;       (define (special-id? string)
;;         (or (string=? string "+")
;;             (string=? string "-")
;;             (string=? string "...")))
;;       (define (ok-symbol? string)
;;         (if (string=? string "")
;;             #f
;;             (let ((rank1 (char-rank (string-ref string 0))))
;;               (cond ((= rank1 0) (subsequents? string 1 (string-length string)))
;;                     ((= rank1 1) (special-id? string))
;;                     (else #f)))))
;;       (define (nn-helper string i len seen-point?)
;;         (cond ((= i len)
;;                (if seen-point?
;;                    (not (char=? (string-ref string (- len 1)) #\0))
;;                    #t))
;;               ((char=? #\. (string-ref string i))
;;                (cond (seen-point? #f)
;;                      ((= (+ i 2) len) #t)  ; Accepts "23.0"
;;                      (else (nn-helper string (+ i 1) len #t))))
;;               ((= 2 (char-rank (string-ref string i)))
;;                (nn-helper string (+ i 1) len seen-point?))
;;               (else #f)))
;;       (define (narrow-number? string)
;;         (if (string=? string "")
;;             #f
;;             (let* ((c0 (string-ref string 0))
;;                    (start 0)
;;                    (len (string-length string))
;;                    (cn (string-ref string (- len 1))))
;;               (if (and (char=? c0 #\-) (not (= len 1)))
;;                   (begin
;;                     (set! start 1)
;;                     (set! c0 (string-ref string 1)))
;;                   #f)
;;               (cond ((not (= (char-rank cn) 2)) #f)  ; Rejects "-" among others
;;                     ((char=? c0 #\.) #f)
;;                     ((char=? c0 #\0)
;;                      (cond ((= len 1) #t)  ; Accepts "0" but not "-0"
;;                            ((= len 2) #f)  ; Rejects "-0" and "03"
;;                            ((char=? (string-ref string (+ start 1)) #\.)
;;                             (nn-helper string (+ start 2) len #t))
;;                            (else #f)))
;;                     (else (nn-helper string start len #f))))))

;;       ;; The body of string->word:
;;       (cond ((narrow-number? string) (string->number string))
;;             ((ok-symbol? string) (string->symbol string))
;; (else string)
;; ))))

(define char->word
  (let ((= =)
        (char-rank char-rank)
        (make-string make-string)
        (string->symbol string->symbol)
        (string->number string->number)
        (char=? char=?))
    (lambda (char)
      (let ((rank (char-rank char))
            (string (make-string 1 char)))
        (cond ((= rank 0) (string->symbol string))
              ((= rank 2) (string->number string))
              ((char=? char #\+) '+)
              ((char=? char #\-) '-)
              (else string))))))

;; (define word->string
;;   (let ((number? number?)
;;         (string? string?)
;;         (number->string number->string)
;;         (symbol->string symbol->string))
;;     (lambda (wd)
;;       (cond ((string? wd) wd)
;;             ((number? wd) (number->string wd))
;;             (else (symbol->string wd))))))

(define word->string string)

(define count
  (let ((word? word?)
        (string-length string-length)
        (word->string word->string)
        (length length))
    (lambda (stuff)
      (if (word? stuff)
          (string-length (word->string stuff))
          (length stuff)))))

;; (define word
;;   (let ((string->word string->word)
;;         (apply apply)
;;         (string-append string-append)
;;         (map map)
;;         (word? word?)
;;         (word->string word->string)
;;         (whoops whoops))
;;     (lambda x
;;       (string->word
;;        (apply string
;;               (map (lambda (arg)
;;                      (if (word? arg)
;;                          arg
;;                          (whoops "Invalid argument to WORD: " arg)))
;;                    x))))))

(define se
  (let ((pair? pair?)
        (null? null?)
        (word? word?)
        (car car)
        (cons cons)
        (cdr cdr)
        (whoops whoops))
    (define (paranoid-append a original-a b)
      (cond ((null? a) b)
            ((word? (car a))
             (cons (car a) (paranoid-append (cdr a) original-a b)))
            (else (whoops "Argument to SENTENCE not a word or sentence"
                          original-a ))))
    (define (combine-two a b)                ;; Note: b is always a list
      (cond ((pair? a) (paranoid-append a a b))
            ((null? a) b)
            ((word? a) (cons a b))
            (else (whoops "Argument to SENTENCE not a word or sentence:" a))))
    ;; Helper function so recursive calls don't show up in TRACE
    (define (real-se args)
      (if (null? args)
          '()
          (combine-two (car args) (real-se (cdr args)))))
    (lambda args
      (real-se args))))

(define sentence se)

;; (define first
;;   (let ((pair? pair?)
;;         (char->word char->word)
;;         (string-ref string-ref)
;;         (word->string word->string)
;;         (car car)
;;         (empty? empty?)
;;         (whoops whoops)
;;         (word? word?))
;;     (define (word-first wd)
;;       (char->word (string-ref (word->string wd) 0)))
;;     (lambda (x)
;;       (cond ((pair? x) (car x))
;;             ((empty? x) (whoops "Invalid argument to FIRST: " x))
;;             ((word? x) (word-first x))
;;             (else (whoops "Invalid argument to FIRST: " x))))))

;; (define last
;;   (let ((pair? pair?)
;;         (- -)
;;         (word->string word->string)
;;         (char->word char->word)
;;         (string-ref string-ref)
;;         (string-length string-length)
;;         (empty? empty?)
;;         (cdr cdr)
;;         (car car)
;;         (whoops whoops)
;;         (word? word?))
;;     (define (word-last wd)
;;       (let ((s (word->string wd)))
;;         (char->word (string-ref s (- (string-length s) 1)))))
;;     (define (list-last lst)
;;       (if (empty? (cdr lst))
;;           (car lst)
;;           (list-last (cdr lst))))
;;     (lambda (x)
;;       (cond ((pair? x) (list-last x))
;;             ((empty? x) (whoops "Invalid argument to LAST: " x))
;;             ((word? x) (word-last x))
;;             (else (whoops "Invalid argument to LAST: " x))))))

;; (define bf
;;   (let ((pair? pair?)
;;         (substring substring)
;;         (string-length string-length)
;;         (string->word string->word)
;;         (word->string word->string)
;;         (cdr cdr)
;;         (empty? empty?)
;;         (whoops whoops)
;;         (word? word?))
;;     (define string-bf
;;       (lambda (s)
;;         (substring s 1 (string-length s))))
;;     (define (word-bf wd)
;;       (string->word (string-bf (word->string wd))))
;;     (lambda (x)
;;       (cond ((pair? x) (cdr x))
;;             ((empty? x) (whoops "Invalid argument to BUTFIRST: " x))
;;             ((word? x) (word-bf x))
;;             (else (whoops "Invalid argument to BUTFIRST: " x))))))

(define butfirst bf)

;; (define bl
;;   (let ((pair? pair?) (- -)
;;         (cdr cdr)
;;         (cons cons)
;;         (car car)
;;         (substring substring)
;;         (string-length string-length)
;;         (string->word string->word)
;;         (word->string word->string)
;;         (empty? empty?)
;;         (whoops whoops)
;;         (word? word?))
;;     (define (list-bl list)
;;       (if (null? (cdr list))
;;           '()
;;           (cons (car list) (list-bl (cdr list)))))
;;     (define (string-bl s)
;;       (substring s 0 (- (string-length s) 1)))
;;     (define (word-bl wd)
;;       (string->word (string-bl (word->string wd))))
;;     (lambda (x)
;;       (cond ((pair? x) (list-bl x))
;;             ((empty? x) (whoops "Invalid argument to BUTLAST: " x))
;;             ((word? x) (word-bl x))
;;             (else (whoops "Invalid argument to BUTLAST: " x))))))

(define butlast bl)

(define item
  (let ((> >) (- -) (< <) (number? number?) (list-ref list-ref)
        (char->word char->word)
        (string-ref string-ref)
        (word->string word->string)
        (not not)
        (whoops whoops)
        (count count)
        (word? word?)
        (list? list?))
    (define (word-item n wd)
      (char->word (string-ref (word->string wd) (- n 1))))
    (lambda (n stuff)
      (cond ((not (number? n))
             (whoops "Invalid first argument to ITEM (must be a number): "
                     n))
            ((< n 1)
             (whoops "Invalid first argument to ITEM (must be positive): "
                     n))
            ((> n (count stuff))
             (whoops "No such item: " n stuff))
            ((word? stuff) (word-item n stuff))
            ((list? stuff) (list-ref stuff (- n 1)))
            (else (whoops "Invalid second argument to ITEM: " stuff))))))

;; (define equal? <see simply.scm>)

(define member?
  (let ((> >) (- -) (< <)
        (null? null?)
        (symbol? symbol?)
        (eq? eq?)
        (car car)
        (not not)
        (symbol->string symbol->string)
        (string-ci=? string-ci=?)
        (cdr cdr)
        (equal? equal?)
        (word->string word->string)
        (string-length string-length)
        (whoops whoops)
        (string-ref string-ref)
        (char=? char=?)
        (list? list?)
        (number? number?)
        (empty? empty?)
        (word? word?)
        (string? string?))
    (define (symbol-in-list? symbol string lst)
      (cond ((null? lst) #f)
            ((and (symbol? (car lst))
                  (eq? symbol (car lst))))
            ((string? (car lst))
             (cond ((not string)
                    (symbol-in-list? symbol (symbol->string symbol) lst))
                   ((string-ci=? string (car lst)) #t)
                   (else (symbol-in-list? symbol string (cdr lst)))))
            (else (symbol-in-list? symbol string (cdr lst)))))
    (define (word-in-list? wd lst)
      (cond ((null? lst) #f)
            ((equal? wd (car lst)) #t)
            (else (word-in-list? wd (cdr lst)))))
    (define (word-in-word? small big)
      (let ((one-letter-str (word->string small)))
        (if (> (string-length one-letter-str) 1)
            (whoops "Invalid arguments to MEMBER?: " small big)
            (let ((big-str (word->string big)))
              (char-in-string? (string-ref one-letter-str 0)
                               big-str
                               (- (string-length big-str) 1))))))
    (define (char-in-string? char string i)
      (cond ((< i 0) #f)
            ((char=? char (string-ref string i)) #t)
            (else (char-in-string? char string (- i 1)))))
    (lambda (x stuff)
      (cond ((empty? stuff) #f)
            ((word? stuff) (word-in-word? x stuff))
            ((not (list? stuff))
             (whoops "Invalid second argument to MEMBER?: " stuff))
            ((symbol? x) (symbol-in-list? x #f stuff))
            ((or (number? x) (string? x))
             (word-in-list? x stuff))
            (else (whoops "Invalid first argument to MEMBER?: " x))))))

(define assoc
  (let ((car car)
        (cdr cdr)
        (null? null?)
        (equal? equal?)
        (pair? pair?)
        (not not))
    (lambda (key alist)
      (cond ((null? alist) #f)
            ((not (pair? (car alist))) (assoc key (cdr alist)))
            ((equal? key (car (car alist))) (car alist))
            (else (assoc key (cdr alist)))))))

(define member assoc)

(define before?
  (let ((not not)
        (word? word?)
        (whoops whoops)
        (string-ci<? string-ci<?)
        (word->string word->string))
    (lambda (wd1 wd2)
      (cond ((not (word? wd1))
             (whoops "Invalid first argument to BEFORE? (not a word): " wd1))
            ((not (word? wd2))
             (whoops "Invalid second argument to BEFORE? (not a word): " wd2))
            (else (string-ci<? (word->string wd1) (word->string wd2)))))))


;;; Higher Order Functions

(define filter
  (let ((null? null?)
        (car car)
        (cons cons)
        (cdr cdr)
        (not not)
        (procedure? procedure?)
        (whoops whoops)
        (list? list?))
    (lambda (pred l)
      ;; Helper function so recursive calls don't show up in TRACE
      (define (real-filter l)
        (cond ((null? l) '())
              ((pred (car l))
               (cons (car l) (real-filter (cdr l))))
              (else (real-filter (cdr l)))))
      (cond ((not (procedure? pred))
             (whoops "Invalid first argument to FILTER (not a procedure): "
                     pred))
            ((not (list? l))
             (whoops "Invalid second argument to FILTER (not a list): " l))
            (else (real-filter l))))))

(define keep
  (let ((+ +) (= =) (pair? pair?)
        (substring substring)
        (char->word char->word)
        (string-ref string-ref)
        (string->list string->list)
        (list->string list->string)
        (word->string word->string)
        (string-length string-length)
        (string->word string->word)
        (make-string make-string)
        (procedure? procedure?)
        (whoops whoops)
        (word? word?)
        (null? null?))
    (lambda (pred w-or-s)
      (define (keep-string in i out out-len len)
        (cond ((= i len) (substring out 0 out-len))
              ((pred (char->word (string-ref in i)))
               (string-set! out out-len (string-ref in i))
               (keep-string in (+ i 1) out (+ out-len 1) len))
              (else (keep-string in (+ i 1) out out-len len))))
      (define (keep-word wd)
        (string->word
         (list->string
          (filter pred
                  (map char->word
                       (string->list (word->string wd)))))))
      (define (keep-word wd)
        (let* ((string (word->string wd))
               (len (string-length string)))
          (string->word
           (keep-string string 0 (make-string len) 0 len))))
      (cond ((not (procedure? pred))
             (whoops "Invalid first argument to KEEP (not a procedure): "
                     pred))
            ((pair? w-or-s) (filter pred w-or-s))
            ((word? w-or-s) (keep-word w-or-s))
            ((null? w-or-s) '())
            (else
             (whoops "Bad second argument to KEEP (not a word or sentence): "
                     w-or-s))))))

(define appearances
  (let ((count count)
        (keep keep)
        (equal? equal?))
    (lambda (item aggregate)
      (count (keep (lambda (element) (equal? item element)) aggregate)))))

;;(display "good")

(define every
  (lambda (fn stuff)
    (define (string-every string i length)
      (if (= i length)
          '()
          (se (fn (char->word (string-ref string i)))
              (string-every string (+ i 1) length))))
    (define (sent-every sent)
      (if (empty? sent)
          sent
          (se (fn (first sent))
              (sent-every (bf sent)))))
    (cond ((not (procedure? fn))
           (whoops "Invalid first argument to EVERY (not a procedure):"
                   fn))
          ((word? stuff)
           (let ((string (word->string stuff)))
             (string-every string 0 (string-length string))))
          (else (sent-every stuff)))))

;; In _Simply Scheme_, accumulate works on words and sentences, and takes
;; two arguments.  In SICP, accumulate works on lists, and takes three
;; arguments.  This version does both.  Sorry.

(define accumulate
  (let ((not not)
        (empty? empty?)
        (bf bf)
        (first first)
        (procedure? procedure?)
        (whoops whoops)
        (member member)
        (list list))
    (lambda args
      (let ((combiner (car args))
            (stuff (cadr args))
            (extra (cddr args)))
        (define (real-accumulate stuff)
          (if (empty? (bf stuff))
              (first stuff)
              (combiner (first stuff) (real-accumulate (bf stuff)))))
        (define (sicp-accumulate initial stuff)
          (if (null? stuff)
              initial
              (combiner (car stuff) (sicp-accumulate initial (cdr stuff)))))
        (cond ((not (procedure? combiner))
               (whoops "Invalid first argument to ACCUMULATE (not a procedure):"
                       combiner))
              ((null? extra)    ; Simply Scheme version
               (cond ((not (empty? stuff)) (real-accumulate stuff))
                     ((member combiner (list + * word se)) (combiner))
                     (else
                      (whoops "Can't accumulate empty input with that combiner"))))
              ((not (null? (cdr extra)))
               (whoops "Too many arguments to accumulate"))
              (else (sicp-accumulate stuff (car extra))))))))

(define reduce
  (let ((null? null?)
        (cdr cdr)
        (car car)
        (not not)
        (procedure? procedure?)
        (whoops whoops)
        (member member)
        (list list))
    (lambda (combiner stuff)
      (define (real-reduce stuff)
        (if (null? (cdr stuff))
            (car stuff)
            (combiner (car stuff) (real-reduce (cdr stuff)))))
      (cond ((not (procedure? combiner))
             (whoops "Invalid first argument to REDUCE (not a procedure):"
                     combiner))
            ((not (null? stuff)) (real-reduce stuff))
            ((member combiner (list + * word se append)) (combiner))
            (else (whoops "Can't reduce empty input with that combiner"))))))

(define repeated
  (let ((= =) (- -))
    (lambda (fn number)
      (if (= number 0)
          (lambda (x) x)
          (lambda (x)
            ((repeated fn (- number 1)) (fn x)))))))


;; Tree stuff
(define make-node cons)
(define make-tree cons)
(define datum car)
(define children cdr)

;; cs3 test-suite library.

;; Berkeley STK Test Framework Additions
;; by Pierre Karashchuk

;; To use, save code into some file, say "tests-named.scm".
;; then load the file: (load "tests-named.scm")

;; ------------------------

;; What can you do with it?

;; add tests to test database:

;; (add-tests-named <name> <test1> <test2> ...)
;; where <name> is some symbol, string, or number which names this group of tests.
;; and <test1>,<test2>, ... are of the format (<expression> <expected result>)
;;
;; NOTE: If a test group with name <name> has already been added, add-tests-named will neither
;; override nor modify it. Rather, it will do nothing.

;; then run the tests:
;;
;; (run-tests-named <name>)
;; where <name> is a name of some group of tests added earlier
;;
;; (run-tests-named <name1> <name2> ...)
;; will run group of tests with <name1> or <name2> and so on...
;;
;; (run-tests-named)
;; will run ALL groups of tests added

;; if you wish to clear tests
;; (need to clear to re-add tests with same name):
;;
;; (clear-tests-name <name1> <name2> ...)
;; will clear tests groups with names <name1>, <name2>, etc...
;;
;; (clear-test-cases)
;; will clear all test cases.

;; sample usage:
;; > (add-tests-named 'arithmetic-test
;;     ((+ 1 2) 3)
;;     ((* 3 5) 15)
;;     ((/ 6 2) (/ 12 4))
;;     ((+ 1 1) 5)
;;     )
;; => arithmetic-test

;; > (run-tests-named 'arithmetic-test)
;; (TESTS NAMED: arithmetic-test)
;; (----- Running 4 tests... Failure output below. -----)
;; ((+ 1 1)     Expected: 5     Actual: 2)
;; (----- Done.  1 failed test(s).  0 error(s). -----)
;;
;; (good: 3     failed: 0   errors: 1)


;; -----------------------------------------

(define show display)

;; use for Stk scheme
(define to-string string)

(define-macro (add-tests-named . tests)
  `(if (member (to-string ,(car tests)) (map car *test-groups*))
       (show (list "WARNING: duplicate test group" ,(car tests) "detected. Ignoring it."))
       (begin
         (add-tests-named-helper ,(car tests) (quote ,(cdr tests)))
         ,(car tests))))

(define-macro (add-test-cases . tests)
  (cons 'begin
        (map (lambda (a) (cons 'add-test-case a))
             tests)))

;; use for MIT scheme
;; (define to-string string)
;; (define-syntax catch
;;   (syntax-rules ()
;;     ((catch expr)
;;      (condition? (ignore-errors (lambda () expr))))))

;; (define-syntax add-tests-named
;;   (syntax-rules ()
;;     ((add-tests-named name (expr val) ...)
;;      (if (member (to-string name) (map car *test-groups*))
;;          (show (list "WARNING: duplicate test group" name "detected. Ignoring it."))
;;          (begin
;;            (add-tests-named-helper name '((expr val) ...))
;;            name)))))

;; (define-syntax add-test-cases
;;   (syntax-rules ()
;;     ((add-test-cases (name expr val))
;;      (add-test-case name val expr))
;;     ((add-test-cases (name expr val) t ...)
;;      (begin (add-test-case name val expr)
;;             (add-test-cases t ...)))))



(define (good-test-name? name)
  (or (symbol? name) (number? name) (string? name)))

(define *test-groups* '())
(define *xx-test-case-store* '())

(define (clear-test-cases)
  (set! *xx-test-case-store* '())
  (set! *test-groups* '()))

(define (clear-tests test-names)
  (set! *xx-test-case-store*
        (filter (lambda (x) (not (member (car x) test-names)))
                *xx-test-case-store*)))

(define (clear-tests-named . names)
  (let ((namestrs (map to-string names)))
    (define (clear groups)
      (cond ((null? groups) '())
            ((member (caar groups) namestrs)
             (clear-tests (map car (cdar groups)))
             (clear (cdr groups)))
            (else (cons (car groups)
                        (clear (cdr groups))))))
    (set! *test-groups* (clear *test-groups*))
    'ok))

(define (add-test-case-quoted name expected expr)
  (if (not (good-test-name? name))
      (show (list "WARNING: badly named test case:" name
                  "Ignoring it."))
      (let ((namestr (to-string name)))
        (if (assoc namestr *xx-test-case-store*)
            (show (list  "WARNING: duplicate test case" namestr "detected."
                         "Ignoring it."))
            (set! *xx-test-case-store*
                  (append *xx-test-case-store*
                          (list (list namestr expected expr))))))))

(define (add-tests-named-helper name tests)
  (define (format-tests i tests)
    (cond ((null? tests) '())
          (else (let ((t (car tests)))
                  (cons (list (to-string name "-" i) (cadr t) (car t))
                        (format-tests (+ i 1) (cdr tests)))))))
  (let ((formatted-tests (format-tests 0 tests)))
    (for-each (lambda (x) (apply add-test-case-quoted x))
              formatted-tests)
    (set! *test-groups*
          (cons (cons (to-string name) formatted-tests)
                *test-groups*))
    ))


(define (run-single-case case)
  (let* ((get-name car)
         (get-expected cadr)
         (get-expression caddr)

         (expected (eval (get-expected case)))
         (actual (eval (get-expression case))))
    (cond
     ;; ((catch (set! actual (eval (get-expression case))))
     ;;  (show (list (get-expression case)
     ;;              "\t Expected:" expected
     ;;              "\tActual:" "ERROR!!!"))
     ;;  (list 1 0)) ;; 1 error, 0 failures
     ((not (equal? actual expected))
      (show (list (get-expression case)
                  "\tExpected:" expected
                  "\tActual:" actual))
      (list 0 1)) ;; 0 errors, 1 failure
     (else (list 0 0)) ;; 0 errors, 0 failures
     )))


(define (run-test-group-named name)
  (let ((test-cases-to-run '())
        (err-fail (list 0 0)))

    (let ((a (assoc (to-string name) *test-groups*)))
      (if a (set! test-cases-to-run (cdr a))))

    (show (list "TESTS NAMED:" name))
    (show (list "----- Running" (length test-cases-to-run)
                "tests... Failure output below. -----"))
    (set! err-fail
          (accumulate (lambda (a b) (map + a b)) (list 0 0)
                      (map run-single-case test-cases-to-run)))
    (show (list "----- Done. "
                (car err-fail) "failed test(s). "
                (cadr err-fail) "error(s). -----" ))
    (newline)
    (cons (length test-cases-to-run) err-fail)))

(define (run-tests-named . names)
  (cond ((null? *test-groups*) 'done)
        ((null? names)
         (apply run-tests-named
                (reverse (map car *test-groups*))))
        (else
         (let ((count-err-fail
                (accumulate (lambda (a b) (map + a b)) (list 0 0 0)
                            (map run-test-group-named names))))
           (show (list "good:" (- (car count-err-fail)
                                  (cadr count-err-fail)
                                  (caddr count-err-fail))
                       "\tfailed:" (cadr count-err-fail)
                       "\terrors:" (caddr count-err-fail)))))))



(define (run-test-cases2 . input)
  (let ((test-cases-to-run '())
        (get-name car)
        (get-expected cadr)
        (get-function caaddr)
        (get-expression caddr)
        (err-fail (list 0 0))
        )

    (define (string-starts-with? start full)
      (equal? (substring full 0 (string-length start))
              start))

    (cond ((null? input)
           (set! test-cases-to-run *xx-test-case-store*))
          ((procedure? (car input))
           (set! test-cases-to-run
                 (filter (lambda (case)
                           (and (list? (get-expression case))
                                (equal? (eval (get-function case))
                                        (car input))))
                         *xx-test-case-store*) )
           )
          (else  ; check names
           (let ((argstr (if (good-test-name? (car input))
                             (to-string (car input))
                             (error "Bad input to run-test-cases:"
                                    "not a procedure, symbol, number, or string!"))))
             (set! test-cases-to-run
                   (filter (lambda (case)
                             (string-starts-with? argstr (get-name case)))
                           *xx-test-case-store*) ) )) )

    (show (list "----- Running"
                (length test-cases-to-run)
                "tests... Failure output below. -----"))
    (set! err-fail
          (accumulate (lambda (a b) (map + a b)) (list 0 0)
                      (map run-single-case test-cases-to-run)))
    (show (list "----- Done. "
                (car err-fail) "failed test(s). "
                (cadr err-fail) "error(s). -----" ))
    (newline)
    ))

;; some easter eggs =)

(define (destroy-the-world!)
  (define (mwhahaha n)
    (if (<= n 0)
        'done
        (begin (print "MWHAHAHAHAHA!!!")
               (mwhahaha (- n 1)))))
  (print "DESTROYED WORLD!!!!")
  (mwhahaha 100))
