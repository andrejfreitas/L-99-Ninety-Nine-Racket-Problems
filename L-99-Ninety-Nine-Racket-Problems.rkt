;http://www.ic.unicamp.br/~meidanis/courses/mc336/2006s2/funcional/L-99_Ninety-Nine_Lisp_Problems.html
;P01 (*) Find the last box of a list.
;Example:
;* (my-last '(a b c d))
;(D)
(define (my-last l)
  (if (or (null? l) (null? (rest l)))
      l
      (my-last (rest l))))

;P02 (*) Find the last but one box of a list.
;Example:
;* (my-but-last '(a b c d))
;(c d)
(define (my-but-last l)
  (cond
    [(or (empty? l) (empty? (rest l)) (empty? (rest (rest l)))) l]
    [else (my-but-last (rest l))]))

;P03 (*) Find the K'th element of a list.
;The first element in the list is number 1.
;Example:
;* (element-at '(a b c d e) 3)
;C
(define (element-at l n)
  (cond
    [(empty? l) '()]
    [(if (eq? n 1) (first l) (element-at (rest l) (- n 1)) )]
    )
  )

;P04 (*) Find the number of elements of a list.

(define (length l)
  (if (null? l)
      0
      (+ (length (cdr l)) 1)))

;P05 (*) Reverse a list.
(define (reverse-l l [la '()])
  (if (null? l)
      la
      (reverse-l (cdr l) (cons (car l) la))))

;P06 (*) Find out whether a list is a palindrome.
;A palindrome can be read forward or backward; e.g. (x a m a x).


(define (palindrome? l) 
  (equal? (reverse l) l)) 

;P07 (**) Flatten a nested list structure.
;Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).
;Example:
;* (my-flatten '(a (b (c d) e)))
;(a b c d e)
;Hint: Use the predefined functions list and append.

(define (my-flatten ll)
  (cond
    [(null? ll) ll]
    [(list? (car ll)) (append (my-flatten (car ll)) (my-flatten (cdr ll)))]
    [else (cons (car ll) (my-flatten (cdr ll)))]))

;P08 (**) Eliminate consecutive duplicates of list elements.
;If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
;Example:
; caddr  = (first (rest (rest l)))
;cdr = rest l
;* (compress '(a a a a b c c a a d e e e e))
;(a b c a d e)
(define (compress-l l)
  (cond 
    [(null? l) l]
    [(null? (cdr l)) (list (car l))]
    [(eq? (car l) (cadr l)) (compress-l (cdr l))]
    [else (cons (car l) (compress-l (cdr l)))]))

;P09 (**) Pack consecutive duplicates of list elements into sublists.
;If a list contains repeated elements they should be placed in separate sublists.
;Example:
;* (pack '(a a a a b c c a a d e e e e))
;((a a a a) (b) (c c) (a a) (d) (e e e e))
(define (empacota l)
  (cond
  [(null? l) l]
  [(let [(aux (empacota-aux (first l) (rest l)))]
     (cons (first aux) (empacota (rest aux))))]))

(define (empacota-aux e l [la '()])
  (cond
    [(null? l ) (list (cons e la))]
    [(eq? e (first l)) (empacota-aux e (rest l) (cons e la))]
    [else (append (list (cons e la)) l)])
  )

;P10 (*) Run-length encoding of a list.
;Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
;Example:
;* (encode '(a a a a b c c a a d e e e e))
;((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))
(define (encode-l l) (encode-l-aux (pack-l l)))
(define (encode-l-aux l)
  (if (null? l)
      l
      (cons (cons (length (car l)) (caar l)) (encode-l-aux (cdr l)))))

;P11 (*) Modified run-length encoding.
;Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.
;Example:
;* (encode-modified '(a a a a b c c a a d e e e e))
;((4 A) B (2 C) (2 A) D (4 E))
(define (encode-modified l) (encode-modified-aux (pack-l l)))
(define (encode-modified-aux l)
  (cond
    [(null? l) l]
    [(> (length (car l)) 1) (cons (list (length (car l)) (caar l)) (encode-modified-aux (cdr l)))]
    [else (cons (caar l) (encode-modified-aux (cdr l)))]))

;P12 (**) Decode a run-length encoded list.
;Given a run-length code list generated as specified in problem P11. Construct its uncompressed version.
(define (decode-l l)
  (cond
    [(null? l) l]
    [(list? (car l)) (append (decode-l-aux (car l)) (decode-l (cdr l)))]
    [else (cons (car l) (decode-l (cdr l)))]))

(define (decode-l-aux l)
  (if (or (null? l) (= (car l) 0))
      '()
      (cons (cadr l) (decode-l-aux (list (- (car l) 1) (cadr l))))))


;P13 (**) Run-length encoding of a list (direct solution).
;Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create
;the sublists containing the duplicates, as in problem P09, but only count them. As in problem P11,
;simplify the result list by replacing the singleton lists (1 X) by X.
;Example:
;* (encode-direct '(a a a a b c c a a d e e e e))
;((4 A) B (2 C) (2 A) D (4 E))
; O 12 FOI FEITO ASSIM MESMO

;P14 (*) Duplicate the elements of a list.
;Example:
;* (dupli '(a b c c d))
;(A A B B C C C C D D)

(define (dupli l)
    (if (null? l) l (cons (first l) (cons (first l) (dupli (rest l))))
    )
  )

;P15 (**) Replicate the elements of a list a given number of times.
;Example:
;* (repli '(a b c) 3)
;(A A A B B B C C C)

(define (repli l n)
  (if (null? l)
      l
      (append (repli-aux (car l) n) (repli (cdr l) n))))

(define (repli-aux e n)
  (if (= n 0)
      '()
      (cons e (repli-aux e (- n 1)))))

;Solução 2
(define (repli l n [aux n])
  (cond
    [(null? l) l]
    [(if (> aux 0) (cons (first l) (repli l  n (- aux  1) )) (repli (rest l) n n))]))

;P16 (**) Drop every N'th element from a list.
;Example:
;* (drop '(a b c d e f g h i k) 3)
;(A B D E G H K)
(define (drop l n [c 1])
  (cond
    [(null? l) l]
    [(= c n) (drop (cdr l) n 1)]
    [else (cons (car l) (drop (cdr l) n (+ c 1)))]))

;P17 (*) Split a list into two parts; the length of the first part is given.
;Do not use any predefined predicates.
;Example:
;* (split '(a b c d e f g h i k) 3)
;((A B C) (D E F G H I K))
(define (split-l l n [c 1])
  (cond
    [(or (null? l) (= n 0)) (cons '() (list l))]
    [(= c n) (cons (list (car l)) (list (cdr l)))]
    [(< c n) (let [(aux (split-l (cdr l) n (+ c 1)))]
               (cons (cons (car l) (car aux)) (cdr aux)))]
    [else "n não deve ser menor que 0 (zero)"]))

;P18 (**) Extract a slice from a list.
;Given two indices, I and K, the slice is the list containing the elements between the I'th and K'th
;element of the original list (both limits included). Start counting the elements with 1.
;Example:
;* (slice '(a b c d e f g h i k) 3 7)
;(C D E F G)
(define (slice-l l i k [c 1])
  (cond
    [(null? l) l]
    [(and (>= c i) (< c k)) (cons (car l) (slice-l (cdr l) i k (+ c 1)))]
    [(= c k) (list (car l))]
    [else (slice-l (cdr l) i k (+ c 1))]))

;P19 (**) Rotate a list N places to the left.
;Examples:
;* (rotate '(a b c d e f g h) 3)
;(D E F G H A B C)
;* (rotate '(a b c d e f g h) -2)
;(G H A B C D E F)
;Hint: Use the predefined functions length and append, as well as the result of problem P17.
(define (rotate-l l n)
  (cond
    [(null? l) l]
    [(< n 0) (rotate-l l (+ (length l) n))]
    [(> n (length l)) (rotate-l l (modulo n (length l)))]
    [else (let [(aux (split-l l n))]
            (append (cadr aux) (car aux)))]))

;Devolve o resto da divisão de x por y
(define (modulo x y) (if (< x y) x (modulo (- x y) y)))

;Mais eficiente que o anterior
(define (modulo2 x y [aux y])
  (cond
    [(< x aux) x]
    [(< x y) (modulo2 (- x aux) (+ aux aux) aux)]
    [else (modulo2 (- x y) (+ y y) aux)]))

;P20 (*) Remove the K'th element from a list.
;Example:
;* (remove-at '(a b c d) 2)
;(A C D)

(define (remove-at l e)
  (cond 
    [(null? l) l]
    [(eq? e 1) (rest l)]
    [(cons (first l) (remove-at (rest l) (- e 1)))]
    )
  )

;P21 (*) Insert an element at a given position into a list.
; Example:
;* (insert-at 'alfa '(a b c d) 2)
; (a alfa b c d) 

(define (insert-at e l p) 
   (cond
     [(or (null? l) (eq? p 1) ) (cons e l)]
     [(cons (first l) (insert-at e (rest l) (- p 1)))]
     )
  )

;Considerando o tamanho da lista 
(define (insert-at e l p) 
   (cond
     [(or (< p 1) (> p (tamanho l) )) l ]
     [(or (null? l) (eq? p 1)) (cons e l)]
     [else (cons (first l) (insert-at e (rest l) (- p 1)))]
     )
  )

;P22 (*) Create a list containing all integers within a given range.
;If first argument is smaller than second, produce a list in decreasing order.
;Example:
;* (range 4 9)
;(4 5 6 7 8 9)
(define (range-l i j)
  (cond
    [(< i j) (cons i (range-l (+ i 1) j))]
    [(> i j) (cons i (range-l (- i 1) j))]
    [else (list i)]))

;P23 (**) Extract a given number of randomly selected elements from a list.
;The selected items shall be returned in a list.
;Example:
;* (rnd-select '(a b c d e f g h) 3)
;(E D A)
;Hint: Use the built-in random number generator and the result of problem P20.
(define (rnd-select l n)
  (if (or (null? l) (= n 0))
      '()
      (let [(aux (rnd-aux (+ (random (length l)) 1) l))]
        (cons (car aux) (rnd-select (cdr aux) (- n 1))))))

(define (rnd-aux n l [la '()])
  (cond
    [(null? l) #f]
    [(eq? n 1) (cons (car l) (append la (cdr l)))]
    [else (rnd-aux (- n 1) (cdr l) (append la (list (car l))))]))

;P24 (*) Lotto: Draw N different random numbers from the set 1..M.
;The selected numbers shall be returned in a list.
;Example:
;* (lotto-select 6 49)
;(23 1 17 33 21 37)
;Hint: Combine the solutions of problems P22 and P23.
(define (lotto-select n m) (rnd-select (range-l 1 m) n))

;P25 (*) Generate a random permutation of the elements of a list.
;Example:
;* (rnd-permu '(a b c d e f))
;(B A D C E F)
;Hint: Use the solution of problem P23.
(define (rnd-permu l) (rnd-select l (length l)))

;;
;;P26 Generate the combinations of K distinct objects chosen from the N elements of a list
;; In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that
;; there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficients).
;; For pure mathematicians, this result may be great. But we want to really generate all the
;; possibilities in a list.
;;
;; Example:
;; (combination 3 '(a b c d e f))
;;  => '((a b c) (a b d) (a b e) ...etc. )

(define (combination k lst)
  (define (combination-helper k lst)
    (cond
      [(or (empty? lst) (> k (length lst))) (list lst)]
      [else (append (map (λ (x) (cons (first lst) x)) (combination-helper (sub1 k) (rest lst))) (combination-helper k (rest lst)))]))
  (filter (λ (x) (= (length x) k)) (combination-helper k lst)))




;P28
;; Sorting a list of lists according to length of sublists:
;;
;; a) We suppose that a list contains elements that are lists themselves. The
;;    objective is to sort the elements of this list according to their length.
;;    i.e. short lists first, longer lists later, or vice versa.
;;
;;    Example:
;;    (lsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
;;     => '((o) (d e) (d e) (m n) (a b c) (f g h) (i j k l))
;;
;; b) Again, we suppose that a list contains elements that are lists themselves. But
;;    this time the objective is to sort the elements of this list according to their
;;    length frequency. i.e. in the default, where sorting is done ascendingly, lists
;;    with rare lengths are placed first, others with a more frequent length come later.
;;
;;    Example:
;;    (lfsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
;;     => '((i j k l) (o) (a b c) (f g h) (d e) (d e) (m n))
;;
;;    Note that in the above example, the first two lists in the result have length 4 and 1,
;;    both lengths appear just once. The third and forth list have length 3 which appears twice
;;    (there are two list of this length). And finally, the last three lists have length 2.
;;    This is the most frequent length.
;;

(define (lsort lst)
  (sort lst < #:key length #:cache-keys? #t))

(define (lfsort lst)
  (define (lfrequency item lst)
    (define item-length (length item))
    (foldl (λ (x y) (if (= (length x) item-length) (add1 y) y)) 0 lst))
  
  (define (lfsort-helper lst)
    (cond
      [(empty? lst) empty]
      [else (define first-freq (lfrequency (first lst) lst))
            (append (lfsort (filter (λ (x) (< (lfrequency x lst) first-freq)) (rest lst)))
                    (list (first lst))
                    (lfsort (filter (λ (x) (>= (lfrequency x lst) first-freq)) (rest lst))))]))
  
  (lfsort-helper lst))


;P31 (**) Determine whether a given integer number is prime.
;Example:
;* (is-prime 7)
;T
(define (is-prime n [c 2] [l n])
  ;(write (list l))
  (cond
    [(< n 2) #f]
    [(or (= n 2) (> c l)) #t]
    [else (let [(aux (qr n c))]
            (if (= (cdr aux) 0) #f (is-prime n (+ c 1) (car aux))))]))

;Devolve o quociente e o resto da divisão inteira como um par
(define (qr x y [q 0])
  (if (< x y)
      (cons q x)
      (qr (- x y) y (+ q 1))))


;P32 (**) Determine the greatest common divisor of two positive integer numbers.
;Use Euclid's algorithm.
;Example:
;* (gcd 36 63)
;9
(define (gcd x y)
  (cond
    [(= x y) x]
    [(> x y) (gcd (- x y) y)]
    [else (gcd x (- y x))]))

;P33 (*) Determine whether two positive integer numbers are coprime.
;Two numbers are coprime if their greatest common divisor equals 1.
;Example:
;* (coprime 35 64)
;T
(define (coprime x y) (= (gcd x y) 1))

;P34 (**) Calculate Euler's totient function phi(m).
;Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r < m) that are coprime to m.
;Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1) = 1.
;* (totient-phi 10)
;4
;Find out what the value of phi(m) is if m is a prime number. Euler's totient function plays an important role in one of the most widely
;used public key cryptography methods (RSA). In this exercise you should use the most primitive method to calculate this function
;(there are smarter ways that we shall discuss later).
(define (totient-phi m [c 1] [ma m])
  (cond
    [(= ma 1) c]
    [(coprime m ma) (totient-phi m (+ c 1) (- ma 1))]
    [else (totient-phi m c (- ma 1))]))

;P35 (**) Determine the prime factors of a given positive integer.
;Construct a flat list containing the prime factors in ascending order.
;Example:
;* (prime-factors 315)
;(3 3 5 7)
(define (prime-factors x [d 2] [l '()])
  (if (= x 1)
      l
      (let [(aux (qr x d))]
        (if (= (cdr aux) 0)
            (prime-factors (car aux) d (append l (list d)))
            (prime-factors x (+ d 1) l)))))


;P36 (**) Determine the prime factors of a given positive integer (2).
;Construct a list containing the prime factors and their multiplicity.
;Example:
;* (prime-factors-mult 315)
;((3 2) (5 1) (7 1))
;Hint: The problem is similar to problem P13.
(define (prime-factors-mult x) (encode (prime-factors x)))

;P39 (*) A list of prime numbers.
;Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.
(define (primes-in x y)
  (cond
    [(> x y) '()]
    [(is-prime x) (cons x (primes-in (+ x 1) y))]
    [else (primes-in (+ x 1) y)]))

;P40 (**) Goldbach's conjecture.
;Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers. Example: 28 = 5 + 23. It is one of the most famous facts in number theory that has not been proved to be correct in the general case. It has been numerically confirmed up to very large numbers (much larger than we can go with our Prolog system). Write a predicate to find the two prime numbers that sum up to a given even integer.
;Example:
;* (goldbach 28)
;(5 23)
(define (goldbach x [m 2] [n (- x 2)])
  (cond
    [(> m n) #f]
    [(and (is-prime m) (is-prime n)) (list m n)]
    [else (goldbach x (+ m 1) (- n 1))]))

;P49 (**) Gray code.
;An n-bit Gray code is a sequence of n-bit strings constructed according to certain rules. For example,
;n = 1: C(1) = ['0','1'].
;n = 2: C(2) = ['00','01','11','10'].
;n = 3: C(3) = ['000','001','011','010',´110´,´111´,´101´,´100´].;
;Find out the construction rules and write a predicate with the following specification:
;% gray(N,C) :- C is the N-bit Gray code
;Can you apply the method of "result caching" in order to make the predicate more efficient, when it is to be used repeatedly?
(define (gray c)
  (cond
    [(eq? c 1) (list (list 0) (list 1))]
    [else (let [(aux (gray (- c 1)))]
            (append (gray-aux 0 aux) (gray-aux 1 aux)))]))

(define (gray-aux e ll)
  (if (null? ll)
      ll
      (cons (cons e (car ll)) (gray-aux e (cdr ll)))))

;P54A (*) Check whether a given term represents a binary tree
;Write a predicate istree which returns true if and only if its argument is a list representing a binary tree.
;Example:
;* (istree (a (b nil nil) nil))
;T
;* (istree (a (b nil nil)))
;NIL
(define (istree t)
  (cond
    [(null? t) #t]
    [(not (= (length (cdr t)) 2)) #f]
    [else (and (istree (cadr t)) (istree (caddr t)))]))


;P61 (*) Count the leaves of a binary tree
;A leaf is a node with no successors. Write a predicate count-leaves/2 to count them. 
;% count-leaves(T,N) :- the binary tree T has N leaves
(define (count-leaves t)
  (cond
    [(null? t) 0]
    [(and (null? (cadr t)) (null? (caddr t))) 1]
    [else (+ (count-leaves (cadr t)) (count-leaves (caddr t)))]))