#| Assignment 1 - Racket Query Language  (due Oct 14, 11:50pm)

***Write the names, CDF accounts and student IDs for each of your group members below.***
<Name>, <CDF>, <ID>
<Name>, <CDF>, <ID>
|#
#lang racket

; Function versions for common syntactic forms.
; *Use these in your queries instead of the syntactic forms!!!*
;(define (And x y) (and x y)) 
;(define (Or x y) (or x y))
;(define (If x y z) (if x y z))
;
; Correction Oct 5 2016

(define-syntax If
  (syntax-rules ()
  ((If a b c)
  (if a b c))))

(define-syntax Or
  (syntax-rules ()
  ((Or a b c)
  (or a b c))))

(define-syntax And
  (syntax-rules ()
  ((And a b c)
  (and a b c))))
; Please do define And, Or as syntactic forms
; We have actually done this in class you may use the class code and this week's lab code for this.
  

; TODO: After you have defined your macro(s), make sure you add each one
; to the provide statement.
(provide attributes
         tuples
         size
         SELECT)

; Part 0: Semantic aliases

#|
(attributes table)
  table: a valid table (i.e., a list of lists as specified by assigment)

  Returns a list of the attributes in 'table', in the order they appear.
|#
(define attributes first)

#|
(tuples table)
  table: a valid table

  Returns a list of all tuples in 'table', in the order they appear.
  Note: it is possible for 'table' to contain no tuples.
|#
(define tuples rest)

#|
(size table)
  table: a valid table

  Returns the number of tuples in 'table'.
|#
(define (size x) 
  (length (tuples x)))

#|------------------------------------------------------------------------------------------------------------- |#
#|------------------------------------------------------------------------------------------------------------- |#
#|------------------------------------------------------------------------------------------------------------- |#

; Part I "WHERE" helpers; you may or may not wish to implement these.

#|
A function that takes: 
  - a list of attributes
  - a string (representing an attribute)
  - a tuple

  and returns the value of the tuple corresponding to that attribute.
|#




#|
A function that takes:
  - f: a unary function that takes a tuple and returns a boolean value
  - table: a valid table

  and returns a new table containing only the tuples in 'table'
  that satisfy 'f'.
|#

(define (filter_table f table)
  (cons (attributes table)
        (filter f (tuples table))))

#|
A function 'replace-attr' that takes:
  - x 
  - a list of attributes

  and returns a function 'f' which takes a tuple and does the following:
    - If 'x' is in the list of attributes, return the corrresponding value 
      in the tuple.
    - Otherwise, just ignore the tuple and return 'x'.
|#




(define get_index (lambda(lst item [count 0])
  (if (empty? lst)
      -1
      (if (equal? item (first lst))
          count
          (get_index  (rest lst) item (+ count 1))))))

(define (get_attr lst attr tuple)
  (list-ref tuple (get_index attr lst)))

(define (get_attrs index attr tuples)
  (map (lambda (x)
         (get_attr x index tuples))
       attr))

(define (get_tuples attr table)
  (cons attr
        (map (lambda (x)
               (get_attrs (attributes table) attr x))
             (tuples table))))

(define (help item lst)
  (map (lambda (temp)
         (cond
           [(null? item) temp]
           [(null? temp) item]
           [else (append item temp)]))
       lst))

(define (cartesian-product lst1 lst2)
  (foldl (lambda (a result)
           (append result (help a lst2)))
         '()
         lst1))


(define (duplicate attr lst)
  (> (length (filter (lambda (x) (equal? attr x))
                     (apply append lst)))
     1))




(define (join tables)
  (if (empty? (rest tables))
      (first tables)
      (cartesian-product (first tables)
                         (join (rest tables)))))



(define (make-lattr-partial lattr lattrs name)
  (define (append-name attr)
    (if (and (not (eq? attr -1))
             (duplicate attr lattrs))
        (string-append name "." attr)
        attr))
  (map append-name lattr))

(define (make-lattr lattrs lnames)
  (define (make-lattr-help lattrs lattrs-bk lnames)
    (if (empty? lattrs)
        '()
        (cons (make-lattr-partial (car lattrs)
                                  lattrs-bk
                                  (car lnames))
              (make-lattr-help (cdr lattrs)
                               lattrs-bk
                               (cdr lnames)))))
  (apply append (make-lattr-help lattrs lattrs lnames)))

(define (cross table_list)
  (let* ([tables (map car table_list)]
         [records (join (map tuples tables))]
         [lattr (make-lattr (map attributes tables)
                            (map cdr table_list))])
    (cons lattr records)))







(define-syntax Product
  (syntax-rules ()
    [(Product [<table1> <name1>])
     (list (cons <table1> <name1>))]
    [(Product <table>)
     (list (cons <table> -1))]
    [(Product <entry> ...)
     (append (Product <entry>)
             ...)]
    ))



#|------------------------------------------------------------------------------------------------------------- |#
#|------------------------------------------------------------------------------------------------------------- |#
#|------------------------------------------------------------------------------------------------------------- |#

; Starter for Part 3; feel free to ignore!

; What should this macro do?
(define-syntax replace
  (syntax-rules ()
    ; The recursive step, when given a compound expression
    [(replace (expr ...) table)
     ; Change this!
     (void)]
    ; The base case, when given just an atom. This is easier!
    [(replace atom table)
     ; Change this!
     (void)]))

(define-syntax SELECT
  (syntax-rules (* FROM)
    ;Select all from table
    [(SELECT * FROM <table>)
     <table>]
    ;Select attribute from single table
    [(SELECT <attr> FROM <table>)
     (get_tuples <attr> <table>)]
    ; FROM -> SELECT: multiple table, all attrs
    [(SELECT * FROM <entry1> <entry2> ...)
     (cross (Product <entry1> <entry2> ...))]    
    ; FROM -> SELECT: multiple table, attrs
    [(SELECT <attrs> FROM <entry1> <entry2> ...)
     (SELECT <attrs> FROM 
             (cross (Product <entry1> <entry2> ...)))]
))