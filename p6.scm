; p6.scm
; Some scheme functions to work on to show use of schemes functionality
; as a programing language through the creation of, the odds, evenrev
; penultimate, and palindrome functions.
; CS 320-01
; 12/11/2019
; @author Kyle McLain Kane (cssc0498)
;
;
; Edit this file to add your documentation and function definitions.
; Leave the rest of this file unchanged.
; To run this file, you would start scheme at edoras command line prompt:
; scheme --load p6.scm, where the file is in the current directory
; and then in scheme type the load command (from the '%' prompt):
;(load "p6.scm")
;
; Defined LISTS for use with testing your functions.
#lang scheme

(define list0 (list 'j 'k 'l 'm 'n 'o 'j) )
(define list1 (list 'a 'b 'c 'd 'e 'f 'g) )
(define list2 (list 's 't 'u 'v 'w 'x 'y 'z) )
(define list3 (list 'j 'k 'l 'm 'l 'k 'j) )
(define list4 (list 'n 'o 'p 'q 'q 'p 'o 'n) )
(define list5 '((a b) c (d e d) c (a b)) )
(define list6 '((h i) (j k) l (m n)) ) 
(define list7 '(f (a b) c (d e d) (b a) f) )

;
; Here is a typical function definition from Sebesta Ch. 15
(define (adder lis)
  (cond
    ((null? lis) 0)
	(else (+ (car lis) (adder (cdr lis))))
))

; The above five lines are the sort of definition you would need to add to
; this file if asked to define an ADDER function.
; Uncomment and complete the following four definitions. At least have ODDS
; so the program can be tested.

(define (odds lst)
  (cond
     ;Checks if it is empty, if < 2 atoms
     ((not(list? lst))(display "USAGE: (odds {list})")(newline))
     
     ((null? lst) '())
     ((null? (cdr lst)) lst)

     ;Finds the odds here
     (else     
      (cons(car lst)(odds(cdr(cdr lst)))))
     ))

(define (evenrev lst)
  (cond
    ;Checks conditions if it is empty or the size is less than 2
    ;Checks if it is empty, if < 2 atoms
    ((not(list? lst))(display "USAGE: (odds {list})")(newline))
         
     ((null? lst) '()); No atoms present = '()
     ((null? (cdr lst)) '());If the list has 1 atom = '()
    
     (else
     ;Calls reverse to reverse the list than calls evens to calc the evens of that
     (reverse(odds (cdr lst)))  
     )))

(define (penultimate lst)
  (cond
    ;Checks a few conditions if < 2, then >= 2
    ((not(list? lst))(display "USAGE: (odds {list})")(newline))

    ;Base cases
    ((null? lst) '())
    ((null? (cdr lst)) '())
    ((null? (cddr lst)) (cdr (reverse lst)))

    ;Recursion
    (else
     (penultimate(cdr lst)) 
     )))

(define (palindrome lst)
  (cond
    ;Does base cases of 0-1 atoms == #t
    ((not(list? lst))(display "USAGE: (odds {list})")(newline))
     
    ((null? lst) #t)
    ((null? (cdr lst)) #t)

    ;Base case
    ((not(equal? (car lst) (car(reverse lst)))) #f)

    ;If it hits here it goes back
    (else
     (palindrome(cdr(reverse(cdr lst))))
     ))
  )

;Reverse function
(define (reverse lst)
  (if (null? (cdr lst))
      lst
      (append (reverse (cdr lst)) (list (car lst)))))

