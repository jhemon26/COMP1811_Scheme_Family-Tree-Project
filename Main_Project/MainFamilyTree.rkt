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





#| Requirements A-01 parents |#

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



#| Requirements A-02 living-members |#

(define (living-members livinglist)
(filter (lambda (name) (not (eq? name #f))) ;filter removes #f and keeps #t (here name = element processed by mapfucn
                                            ;if map return a not empty value element(member) then filter removes that element 
        (map (lambda (member)               ;map applies λ to all element of livinglist
            (cond 
               [(null? (cadr (caddr member))) (car member)] ;cond checks wheather the member(elmntOfLList) empty or not
                                                            ;If empty then returns carmember(FirstElmntOfLList) 
               [else #f])) livinglist)))                    ;Here if death date empty means alive if not then dead and we remove dead

(living-members Mb)



#| Requirements A-03 Current-age |#

(define (current-age age-list)                                           ;function that provides age of all member in a list
(map (lambda (member)                                                  ;"map" applies "lambda to every element of the list
       (let* ((DateOfBirth-DeathOFDeath (caddr member))                ;"let" allows us to create all the veriable which will holds the needed data
              (DateOfBirth (car DateOfBirth-DeathOFDeath))             ;DOB holds [car DOB&DOD(1st element of caddr member)] ex= DOB date            
              (DeathOFDeath (cadr DateOfBirth-DeathOFDeath))           ;DOD holds the Death date
              (current-year 2025)                                      ;created current-year veriable to holds current year(if no DOD found)
              (birth-year (if (pair? DateOfBirth) (caddr DateOfBirth) 0)) ;checks if DOB is a pair and if valid this is to avoid '() list problem
              ;if DOB found pair then "caddrDOB" extracts the Birth year, if no then 0
              ;"0" is used as a fallback if anyhow BOD is empty and to avoid errors
              (death-year (if (null? DeathOFDeath) current-year        ;checks if DOD is empty then returns 2025 else, extract the DOD
                              (caddr DeathOFDeath))))  
         (list (car member) (- death-year birth-year))))               ;kepping the calculation of Age in a list
     age-list))                                                        ;"map" applies this to every element of the list


(current-age Mb)


#| Requirement B-01 children |#

(define (children children-list)            ; Define a function 'children' that takes a family tree list as input.
  (map car                                  ; Use 'map' to extract the child's name (first element) from each record.
       (filter (lambda (record)             ; Use 'filter' to select records where the child has at least one parent.
                 (let ((parents (cadr record))) ; Extract the parent list (second element) from the record.
                   (or (not (null? (car parents)))    ; Check if the first parent's list is non-empty.
                       (not (null? (cadr parents)))))) ; Check if the second parent's list is non-empty.
               children-list)))              ; Apply this filtering to each record in the provided children-list.



#| Requirement B-02 oldest-living-member |#

(define (oldest-living-member family-branch)       ; Define function to find the oldest living member
  (let* ((living (living-members family-branch))   ; Get list of all living members
         (ages (current-age family-branch))        ; Get list of all members with their ages
         (living-ages (filter (lambda (member)    ; Filter the age list to keep only living members
                                (assoc (car member) (map (lambda (x) (list x)) living)))
                              ages)))
    (if (null? living-ages)                        ; Check if there are no living members
        '()                                        ; Return empty list if no one is alive
        (foldl (lambda (a b)                       ; Iterate through the list to find the max
                 (if (> (cadr a) (cadr b)) a b))   ; Compare ages and keep the member with the highest age
               (car living-ages)                   ; Start with the first member in the list
               (cdr living-ages)))))               ; Process the rest of the list



#| Requirement B-03  average-age-on-death |#

(define (average-age-on-death family-branch)
  (let* ((deceased (filter (lambda (member) 
                             (not (null? (cadr (caddr member))))) ; Filter members with non-empty DOD
                           family-branch))
         (ages (map (lambda (m)
                      (let* ((dob-dod (caddr m)) ; Extract DOB and DOD
                             (dob (car dob-dod))
                             (dod (cadr dob-dod))
                             (birth-year (caddr dob)) ; Birth year from DOB
                             (death-year (caddr dod))) ; Death year from DOD
                        (- death-year birth-year))) ; Calculate age at death
                    deceased)))
    (if (null? ages)
        0
        (/ (apply + ages) (length ages)))))