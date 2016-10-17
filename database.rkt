#| Assignment 1 - Racket Query Language  (due Oct 14, 11:50pm)
***Write the names, CDF accounts and student IDs for each of your group members below.***
Sikai Li, lijusti5, 1000665149
Tony Wang, wangto22, 1001333354
|#
#lang racket

; Function versions for common syntactic forms.
; *Use these in your queries instead of the syntactic forms!!!*
(define (And x y) (and x y)) 
(define (Or x y) (or x y))
(define (If x y z) (if x y z))

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

;WHWERE helpers
#|
A function that takes: 
  - a list of attributes
  - a string (representing an attribute)
  - a tuple
  and returns the value of the tuple corresponding to that attribute.
|#
(define (filter_tuple attrs attr tuple)
  (list (list-ref tuple (get_index attrs attr))))



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

(define (replace-attr x attrs)
  (list (lambda (tuple)
    (if (member x attrs)
        (filter_tuple attrs x tuple)
        (list x)))))

#|
Replace-all-attr uses replace-attr on all of the atoms inside a "condition" given by the user
|#

(define (replace-all-attr condition attrs)
  (if (empty? condition)
      empty
      (append (if (not (list? (first condition)))
                  (replace-attr (first condition) attrs)
                  (list (replace-all-attr (first condition) attrs)))
               (replace-all-attr (rest condition) attrs))))

#|
Map-tuple does replace-all-attr for all passed in tuples
|#
(define (map-tuple replace-alls tuple)
  (if (empty? replace-alls)
      empty
      (append (if (not (list? (first replace-alls)))
                  ((first replace-alls) tuple)
                  (list (map-tuple (first replace-alls) tuple)))
              (map-tuple (rest replace-alls) tuple))))

(define-namespace-anchor a)			; create a namespace anchor
(define ns (namespace-anchor->namespace a))	; create a namespace, ns

;Main WHERE
;Main function for filtering WHERE, maps all of the tuples to conditions and evaluate to decide filtering
(define (filter_where condition table)
  (filter_table
   (lambda (tuple)
     (let ([x (map-tuple
            (replace-all-attr (cond
                                [(not (pair? condition)) (list condition)]
                                [else condition])
                              (attributes table)) tuple)])
       (if (equal? (length x) 1)
           (eval (first x) ns)
           (eval x ns))))
   table))

#|------------------------------------------------------------------------------------------------------------- |#


;ORDER BY

;Uses the sort function decide order, also uses replace-all-attr helper from WHERE to evaluate conditions
(define (order-by condition table)
  (cons (attributes table)
  (cond
    [(not (pair? condition))
     (sort (tuples table) > #:key (lambda (item) (list-ref item (get_index (attributes table) condition))))]
    [else (sort (tuples table) > #:key (lambda (item) (eval (map-tuple (replace-all-attr condition (attributes table)) item)ns)))]
    )))

#|------------------------------------------------------------------------------------------------------------- |#

;SELECT helpers

;Gets the index of an element in a list
(define get_index (lambda(lst item [count 0])
  (if (empty? lst)
      -1
      (if (equal? item (first lst))
          count
          (get_index  (rest lst) item (+ count 1))))))

;Gets the corresponding tuple relative to the attribte
(define (get_attr lst attr tuple)
  (list-ref tuple (get_index attr lst)))

;Gets the tuples for all attributes
(define (get_attrs index attr tuples)
  (map (lambda (x)
         (get_attr x index tuples))
       attr))

;Creates table for all attributes and tuples
(define (get_tuples attr table)
  (cons attr
        (map (lambda (x)
               (get_attrs (attributes table) attr x))
             (tuples table))))

#|------------------------------------------------------------------------------------------------------------- |#

;Multi-table product

;Helper for cartisean product
(define (help item lst)
  (map (lambda (temp)
         (cond
           [(null? item) temp]
           [(null? temp) item]
           [else (append item temp)]))
       lst))

;Returns the cartesian-product
(define (cartesian-product lst1 lst2)
  (foldl (lambda (a result)
           (append result (help a lst2)))
         '()
         lst1))

;Checks for duplicates
(define (duplicate attr lst)
  (> (length (filter (lambda (x) (equal? attr x))
                     (apply append lst)))
     1))

;Joins tables using the cartesian product
(define (join tables)
  (if (empty? (rest tables))
      (first tables)
      (cartesian-product (first tables)
                         (join (rest tables)))))

;Helper to add prefix if there  is a duplicate
(define (helper attr lst name)
    (if (and (not (eq? attr -1))
             (duplicate attr lst))
        (string-append name "." attr)
        attr))

;Helper for creating the final table
(define (helper2 attributes attributes2 name)
    (if (empty? attributes)
        '()
        (cons (prefix (first attributes) attributes2(first name))
              (helper2 (rest attributes) attributes2(rest name)))))

;Add prefix to all in lst
(define (prefix lst all name)
  (map (lambda (x)
         (helper x all name))
       lst))

;Add prefixs to all attributes
(define (prefix_all attributes name)
  (apply append (helper2 attributes attributes name)))

;Create the full cross product table
(define (cross table_list)
  (let ([tables (map car table_list)])
  (cons  (prefix_all (map attributes tables)(map cdr table_list)) (join (map tuples tables)))))

#|------------------------------------------------------------------------------------------------------------- |#

(define-syntax Product
  (syntax-rules ()
    ;Product of table and naming prefix
    [(Product [<table1> <name1>])
     (list (cons <table1> <name1>))]
    ;Product of table and no naming prefix
    [(Product <table>)
     (list (cons <table> -1))]
    ;Product of multiple tables
    [(Product <entry> ...)
     (append (Product <entry>)
             ...)]
    ))

(define-syntax SELECT
  (syntax-rules (* FROM WHERE ORDER BY)

    ;Select all from table where order by
    [(SELECT * FROM <table> WHERE <condition> ORDER BY <condi>)
     (order-by (quote <condi>) (filter_where (quote <condition>) <table>))]
    ;Select attribute from table where order by
    [(SELECT <attrs> FROM <entry1> <entry2> ... WHERE <condition> ORDER BY <condi>)
     (SELECT <attrs> FROM 
             (order-by (quote <condi>) (filter_where (quote <condition>) (cross (Product <entry1> <entry2> ...)))))]

    ;Select all from table order by
    [(SELECT * FROM <table> ORDER BY <condi>)
     (order-by (quote <condi>) <table>)]
    ;Select attribute from single table where
    [(SELECT <attr> FROM <table> ORDER BY <condi>)
     (get_tuples <attr> (order-by (quote <condi>) <table>))]
    ;Select all from multiple table order by
    [(SELECT * FROM <entry1> <entry2> ... ORDER BY <condi>)
     (order-by (quote <condi>) (cross (Product <entry1> <entry2> ...)))]
    ;Select attribute from multiple table order by
    [(SELECT <attrs> FROM <entry1> <entry2> ... ORDER BY <condi>)
     (SELECT <attrs> FROM 
             (order-by (quote <condi>)(cross (Product <entry1> <entry2> ...))))]
    
    ;Select all from table where
    [(SELECT * FROM <table> WHERE <condition>)
     (filter_where (quote <condition>) <table>)]
    ;Select attribute from single table where
    [(SELECT <attr> FROM <table> WHERE <condition>)
    (get_tuples <attr> (filter_where (quote <condition>) <table>))]
    ; FROM -> SELECT: multiple table, all attrs where
    [(SELECT * FROM <entry1> <entry2> ... WHERE <condition>)
     (filter_where (quote <condition>) (cross (Product <entry1> <entry2> ...)))]
    ; FROM -> SELECT: multiple table, attrs where
    [(SELECT <attrs> FROM <entry1> <entry2> ... WHERE <condition>)
     (SELECT <attrs> FROM 
             (filter_where (quote <condition>) (cross (Product <entry1> <entry2> ...))))]

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