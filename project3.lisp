;; Returns true (non-nil) if the element X is an element of the list
;; L (test elements with equalp), otherwise false (nil)
 (defun .element-of (X L)
     (cond
         ((null L) nil)
         ((eql X (car L)) t)
         (t (.element-of X (cdr L)))))



;; Returns a list that contains the same elements as the list L, but in the
;; reverse (opposite) order
(defun .reverseHelper(L B)
    (if (null L)
        B
        (.reverseHelper (cdr L) (cons (car L) B))))
       
(defun .reverse (L)
    (.reverseHelper L nil))




;; Assuming that list L contains numbers, returns their sum
(defun .sum (L)
    (if (null L)                   
      0                             
      (+ (car L)                    
         (.sum (cdr L)))))   



;; Returns a list containing only those elements of L for which Boolean
;; function F returns true (non-nil)

(defun .filter (F L)
    (cond
        ((null L) nil)
        ((funcall F (car L))
        (cons (car L) (.filter F (cdr L))))
        (t (.filter F (cdr L)))))
        
        ;; This is test cases given by the nstructor
        ;; (defun .lessthan3 (x) (< x 3))
        ;; (defun .lessthany (y) (lambda (x) (< x y)))


;; Returns the cardinality of (number of elements in) set S
(defun .cardinality (S)
    (if (null S)
      0
      (+ 1
      (.cardinality(cdr S)))))


;; Returns true (non-nil) if the set S contains the element X (test
;; elements with equalp), otherwise false (nil)

(defun .contains (S X)
    (cond
        ((null S) nil)
        ((equalp X (car S)) t)
        (t (.contains (cdr S) X))))


;; Returns the set that is the intersection of sets S1 and S2
;; (that is: L1 ∩ L2 )
(defun .intersection (S1 S2)
    (cond
        ((null S1) nil)
        ((.contains S2 (car S1))
          (cons (car S1)
            (.intersection (cdr S1) S2)))
        (t (.intersection (cdr S1) S2))))


;; Returns true (non-nil) if S1 is a subset of or equal to set S2
;; (that is, if S1 ⊆ S2), otherwise return false (nil)

(defun .subseteq (S1 S2)
    (cond
        ((null S1) t)
        ((.contains S2(car S1))
            (.subseteq (cdr S1) S2))
        (t nil)))



;; Returns true (non-nil) if the complex numbers C1 and C2 are
;; equal. Two complex numbers ⟨r1, i1⟩ and ⟨r2, i2⟩ are equal iff r1 = r2 and i1 = i2.
;; You may assume that complex numbers are represented using lists containing two
;; elements where the first element is the real component of the number and the
;; second element is the imaginary part of the number. Test numbers using =.
(defun .complex= (C1 C2)
    (and (= (car C1) (car C2))
        (= (car (cdr C1)) (car (cdr C2)))))


;; Returns the remainder when positive integer X is divided by positive integer Y

(defun .mod (X Y)
    (if (< X Y)
      X
      (.mod (- X Y) Y)))



;; Returns the dot product of vectors V1 and V2. The dot product of two vectors v1 = ⟨v1,1, v1,2, . . . , v1,n⟩ and v2 = ⟨v2,1, v2,2, . . . , v2,n⟩ 
;; You may assume that vectors are represented as lists of their components. You
;; may also assume that V1 and V2 have the same non-zero number of components

(defun .dot-product (V1 V2)
    (if (or (null V1) (null V2))
      0
      (+ (* (car V1) (car V2))
        (.dot-product(cdr V1) (cdr V2)))))


;; Returns the greatest common divisor (GCD) of the two given positive
;; integers

(defun .gcd(X Y)
    (cond
        ((= X 0) Y)
        ((= Y 0) X)
        (t (.gcd Y (.mod X Y)))))

