;;Maternal branch
(define Mb
'(
    ;:Child         Parent1       Parent2          DOB           DOD
    ((Mary Blake)  ((Ana Ali)    (Theo Blake))   ((17 9 2022)  ()))
    ((Ana Ali)     ((Ada West)   (Md Ali))       ((4 10 1995)  ()))
    ((Theo Blake)  ((Mary Jones) (Tom Blake))    ((9 5 1997)   ()))
    ((Greta Blake) ((Mary Jones) (Tom Blake))    ((16 3 1999)  ()))
    ((Mary Jones)  (()           ())             ((12 5 1967)  (19 5 2024)))
    ((Tom Blake)   (()           ())             ((17 1 1964)  ()))
    ((Ada West)    (()           ())             ((22 8 1973)  ()))
    ((Md Ali)      (()           ())             ((14 2 1972)  (2 5 2023)))
    ((Ned Bloom)   (()           ())             ((23 04 2001) ()))
    ((John Bloom)  ((Greta Blake)(Ned Bloom))    ((5 12 2023)  ()))
))

;;Paternal branch
(define Pb
  '(
    ;;Child          Parent1       Parent2         DOB          DOD
    ((Mary Doe)    ((Alan Doe)    ())            ((14 4 1964)  ()))
    ((John Smith)  ((Fred Smith)  (Jane Doe))    ((1 12 1956)  (3 3 2021)))
    ((Ana Smith)   ((Fred Smith)  (Jane Doe))    ((6 10 1958)  ()))
    ((Alan Doe)    ((John Doe)    (Eve Talis))   ((8 9 1932)   (23 12 2000)))
    ((Jane Doe)    ((John Doe)    (Eve Talis))   ((2 6 1930)   (4 12 1992)))
    ((Fred Smith)  ((Tom Smith)   (Lisa Brown))  ((17 2 1928)  (13 9 2016)))
    ((Eve Talis)   (()            ())            ((15 5 1900)  (19 7 1978)))
    ((John Doe)    (()            ())            ((18 2 1899)  (7 7 1970)))
    ((Lisa Brown)  (()            ())            ((31 6 1904)  (6 3 1980))) ; Note: June has 30 days (invalid date)
    ((Tom Smith)   (()            ())            ((2 8 1897)   (26 11 1987)))
  ))

;;define lst-mb
;;define lst-pb
;;define lst-all

;; C1
(define (lst-mb mb)
  mb)
 
;; C2 
(define (lst-pb pb)
  pb)
  
;; C3
(define (append-lst list1 list2)
        (if (null? list1) list2
            (cons (car list1) (append-lst (cdr list1) list2))))
			
(define (lst-all mb pb)
  (append-lst mb pb))
(lst-all Mb Pb)







                                           #| Features -> A |#




#| Requirements A-01 parents |#

(define (parents Mb)                       ; Method that gets the members nestes list
(define (remove-duplicates-identity lst)   ; Method to remove duplicate appearence from list
  (if (null? lst) '()                      ; BASEcASE->checks if the list is not null so proceed to Recursion
     (cons (car lst)                       ; Extract the first element(Member), "cons" will construct newly non duplicate list.
       (remove-duplicates-identity         ; Recursive call of RDI method
        (filter (lambda (x) (not (equal? x (car lst)))) (cdr lst))))))  ; Comment Description below
        ; Here the "filter" fucn gets "cdrlst" and apply "(not (equal? x (car lst)"
        ; Here if memberCarlst = memberCdrlst then #t but the "not" func then change it to
        ; #f and we discard the member through "filter"
(remove-duplicates-identity (apply append (map cadr Mb))))

;Example usage:
(newline)
(displayln "Example of A-01 'parents':")
(parents Mb)



#| Requirements A-02 living-members |#

(define (living-members lst)
(filter (lambda (name) (not (eq? name #f))) ; filter removes #f and keeps #t (here name = element processed by mapfucn
                                            ; if map return a not empty value element(member) then filter removes that element 
        (map (lambda (member)               ; map applies λ to all element of livinglist
            (cond 
               [(null? (cadr (caddr member))) (car member)] ; cond checks wheather the member(elmntOfLList) empty or not
                                                            ; If empty then returns carmember(FirstElmntOfLList) 
               [else #f])) lst)))                    ; Here if death date empty means alive if not then dead and we remove dead

;Example usage:
(newline)
(displayln "Example of A-02 'living-members':")
(living-members Mb)



#| Requirements A-03 Current-age |#

(define (current-age lst)                                         ; function that provides age of all member in a list
(map (lambda (member)                                                  ; "map" applies "lambda to every element of the list
       (let* ((DateOfBirth-DeathOFDeath (caddr member))                ; "let" allows us to create all the veriable which will holds the needed data
              (DateOfBirth (car DateOfBirth-DeathOFDeath))             ; DOB holds [car DOB&DOD(1st element of caddr member)] ex= DOB date            
              (DeathOFDeath (cadr DateOfBirth-DeathOFDeath))           ; DOD holds the Death date
              (current-year 2025)                                      ; created current-year veriable to holds current year(if no DOD found)
              (birth-year (if (pair? DateOfBirth) (caddr DateOfBirth) 0)) ; checks if DOB is a pair and if valid this is to avoid '() list problem
              ; if DOB found pair then "caddrDOB" extracts the Birth year, if no then 0
              ; "0" is used as a fallback if anyhow BOD is empty and to avoid errors
              (death-year (if (null? DeathOFDeath) current-year        ; checks if DOD is empty then returns 2025 else, extract the DOD
                              (caddr DeathOFDeath))))  
         (list (car member) (- death-year birth-year))))               ; kepping the calculation of Age in a list
     lst))                                                        ; "map" applies this to every element of the list

;Example usage:
(newline)
(displayln "Example of A-03 'current-age':")
(current-age Mb)



#| Requirements A-04 same-birthday-month |#

(define (same-birthday-month lst month)
(let ((matches                                               ; Matches will hold element examined by "filter"
       (filter (lambda (member)                              ;"filter" applies "lambda" to every member
                 (let* ((DateOfBirth (car (caddr member))))  ; Extracts DOB 
                   (if (null? DateOfBirth) #f                ; Ensure DOB is not empty else #f and removed by "filter"
                       (equal? (cadr DateOfBirth) month))))  ; Checks if month parameter = member DOB Months
               lst)))
  (if (null? matches)                                        ; Checks if "matches" is null? else returns (car member and massage) 
      '()  
      (for-each (lambda (member) (display (car member)) (newline)) matches)))
(display "No More Matches Found"))

;Example usage:
(newline)
(displayln "Example of A-04 'same-birthday-month':")
(same-birthday-month Mb 5)(newline)



#| Requirements A-05 sort-by-last |#

(define (get-last-name full-name)              ; Helper fucn to get full name
(cadr full-name))                              ; "cadr" extract full name
(define (sort-by-last lst)                     ; Helper func the sort by Lname
(map car (sort                                 ; map car extract the full name from "sort (sort transform into pairs Fname Lname)"
          
    (map (lambda (member)                      
           (let ((name (car member)))          ; Gets the first name of from the "Mb" elemetns
             (cons name (symbol->string (get-last-name name))))) ; Transform symbol into strings and "cons" create FnameLname pairs
         lst)                                                    ;  -> (Mary Blake . "Blake")
    (lambda (a b) (string<? (cdr a) (cdr b))))))                 ; Lambda compares last names

;Example usage:
(newline)
(displayln "Example of A-05 'sort-by-last':")
(sort-by-last Mb)





                                      #| Features -> B |#



#| Requirement B-01 children |#

(define (children children-list)            ; Define a function 'children' that takes a family tree list as input.
  (map car                                  ; Use 'map' to extract the child's name (first element) from each record.
       (filter (lambda (record)             ; Use 'filter' to select records where the child has at least one parent.
                 (let ((parents (cadr record))) ; Extract the parent list (second element) from the record.
                   (or (not (null? (car parents)))    ; Check if the first parent's list is non-empty.
                       (not (null? (cadr parents)))))) ; Check if the second parent's list is non-empty.
               children-list)))              ; Apply this filtering to each record in the provided children-list.

;Example usage:
(newline)
(displayln "Example of B-01 'children':")
(children Mb)



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

;Example usage:
(newline)
(displayln "Example of B-02 'oldest-living-member':")
(oldest-living-member Mb)



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

;example usage:
(newline)
(displayln "Example of B-03 'average-age-on-death':")
(average-age-on-death Mb)


#| Requirements B-04 same-birthday-month |#

(define (birthday-month-same lst month)
  (let ((matches                                               
         (filter (lambda (member)                              
                   (let* ((dob-entry (caddr member))           ; Extract DOB and DOD pair
                          (DateOfBirth (car dob-entry)))        ; Extract DOB list
                     (and (not (null? DateOfBirth))            ; Ensure DOB is not empty
                          (>= (length DateOfBirth) 2)          ; Ensure DOB has at least two elements
                          (equal? (cadr DateOfBirth) month)))) ; Compare month
                 lst)))
    (if (null? matches)
        (display "No More Matches Found")
        (for-each (lambda (member) 
                    (display (car member)) 
                    (newline)) 
                  matches))
    (newline)))

;example usage:
(newline)
(displayln "Example of B-04 'same-birthday-month':")
(same-birthday-month Mb 5)



#| Requirement B-05 sort-by-first |#

(define (sort-by-first family-branch)
  (sort family-branch                     ; Sort the list of family members
        (lambda (a b)                     ; Custom comparator for two members
          (string<? 
           (symbol->string (car (car a)))   ; Convert the first name of member a to a string
           (symbol->string (car (car b))))))) ; Convert the first name of member b to a string

;Examples usage:
(newline)(newline)
(displayln "Example of B-05 'sort-by-first':")
(sort-by-first Mb)



#| Requirement B-06 change-name-to-Maria |#

(define (change-name-to-Maria family-branch)
  (map (lambda (member)                           ; Process each family member in the branch
         (let* ((name (car member))                ; Extract the name (e.g., (Mary Jones))
                (first-name (car name))            ; Get the first name (e.g., Mary)
                (last-name (cadr name)))           ; Get the last name (e.g., Jones)
           (if (eq? first-name 'Mary)              ; Check if the first name is 'Mary
               (cons (list 'Maria last-name)      ; If so, change it to 'Maria keeping the same last name
                     (cdr member))               ; Preserve the rest of the member's data (parents and DOB/DOD)
               member)))                         ; Otherwise, leave the member unchanged
       family-branch))                           ; Apply this transformation to every member

;Example usage:
(newline)
(displayln "Example of B-06 'change-name-to-Maria':")
(change-name-to-Maria Mb)

