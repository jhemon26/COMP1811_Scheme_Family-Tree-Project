;;Maternal branch
(define Mb
;Child        ;Parent1  ;Parent2       ;DOB       ;IfDOD
'(((Mary Blake) ((Ana Ali) (Theo Blake)) ((17 9 2022) ())) 
((Ana Ali) ((Ada West) (Md Ali)) ((4 10 1995) ()))
((Theo Blake) ((Mary Jones) (Tom Blake)) ((9 5 1997) ()))
((Greta Blake) ((Mary Jones) (Tom Blake)) ((16 3 1999) ()))
((Mary Jones) (() ())((12 5 1967) (19 5 2024)))
((Tom Blake) (() ()) ((17 1 1964) ()))
((Ada West) (() ()) ((22 8 1973) ()))
((Md Ali) (() ()) ((14 2 1972) (2 5 2023)))
((Ned Bloom) (() ()) ((23 04 2001)()))
((John Bloom) ((Greta Blake) (Ned Bloom)) ((5 12 2023) ()))))

#| Requirements A-01 |#

(define (parents Mb)                       ;Method that gets the members nestes list
(define (remove-duplicates-identity lst)   ;Method to remove duplicate appearence from list
  (if (null? lst) '()                      ;BASEcASE->checks if the list is not null so proceed to Recursion
     (cons (car lst)                       ;Extract the first element(Member), "cons" will construct newly non duplicate list.
       (remove-duplicates-identity         ;Recursive call of RDI method
        (filter (lambda (x) (not (equal? x (car lst)))) (cdr lst))))))  ;Comment Description below
        ;Here the "filter" fucn gets "cdrlst" and apply "(not (equal? x (car lst)"
        ;Here if memberCarlst = memberCdrlst then #t but the "not" func then change it to
        ;#f and we discard the member through "filter"
(remove-duplicates-identity (apply append (map cadr Mb))))
(parents Mb)


#| Requirements A-02 |#

(define (living-members livinglist)
(filter (lambda (name) (not (eq? name #f))) ;filter removes #f and keeps #t (here name = element processed by mapfucn
                                            ;if map return a not empty value element(member) then filter removes that element 
        (map (lambda (member)               ;map applies λ to all element of livinglist
            (cond 
               [(null? (cadr (caddr member))) (car member)] ;cond checks wheather the member(elmntOfLList) empty or not
                                                            ;If empty then returns carmember(FirstElmntOfLList) 
               [else #f])) livinglist)))                    ;Here if death date empty means alive if not then dead and we remove dead

(living-members Mb)