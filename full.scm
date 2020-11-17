; list of x and y cordinate elements
(define(make-point x-cor y-cor)
    (cons x-cor y-cor)
)

; helper: retrieves x-value of a point
(define(get-x point)
    (car point)
)

; helper: retrieves y-value of a point
(define(get-y point)
    (cdr point)
)

; helper: rounds input to 3 decimals
(define (thousandth z)
    (/ (round(* 1000 z)) 1000)
)

; helper: rounds input to five decimals
(define (hun-thousandth z)
    (/ (round(* 100000 z)) 100000)
)

; helper: defines pi
(define pi (* 4 (atan 1.0)))

; helper: side lengths -> radians
; function is a general application of cosine law and
; cos(a|b|c) is possible but dependent on input order
(define (cosine-law lhs-1 lhs-2 rhs-1)
    (let    
        (   (a (+ (expt lhs-1 2) (expt lhs-2 2)))
            (b (expt rhs-1 2))
        )
        (acos(/ (- a b) (* 2 lhs-1 lhs-2)))
    )
)

; helper: radians -> degrees
(define (degree rad)
    (/ (* rad 180.0) pi)
)

; -- Divider
; Core Functions: (is-line), (distance),
; (perimeter), (area), (calculate-triangle)

; true  (#t) if the three points form a line 
; false (#f) if they form a triangle.
(define (is-line point1 point2 point3)
    (let* (
        (x1 (get-x point1)) (y1 (get-y point1))
        (x2 (get-x point2)) (y2 (get-y point2))
        (x3 (get-x point3)) (y3 (get-y point3))

        (a (* (- y1 y2) (- x1 x3)))
        (b (* (- y1 y3) (- x1 x2)))
    )
        (= a b)
    )
)

; calculates the distance between two 
; points using the distance formula as
; adapted from the Pythagorean theorem
(define (distance point1 point2)
    (let* (
        (x1 (get-x point1)) (y1 (get-y point1))
        (x2 (get-x point2)) (y2 (get-y point2))
    )
        (sqrt 
            (
                + 
                (expt (- x1 x2) 2)
                (expt (- y1 y2) 2)
            )
        )
    )
)

; calculates perimeter of a triangle by finding 
; sum of (distance)-s between the three points
(define (perimeter point1 point2 point3)
    (if 
        (equal? 
            ; if three points == line then (#f)
            (is-line point1 point2 point3) 
            #f
        )
        (
            + 
            (distance point1 point2) 
            (distance point2 point3) 
            (distance point1 point3)
        )   (display "Something went wrong here...")
    )
)

; calculates area of triangle using formula:
; ... Area = sqrt(p(p-a)(p-b)(p-c))
; ... where p = perimeter / 2
; ... and a, b, and c equal the 3 side lengths
(define (area point1 point2 point3)
    (let* 
        (
            ; gets triangle side lengths
            (sideA (distance point1 point2))
            (sideB (distance point2 point3))
            (sideC (distance point1 point3))

            ; gets 1/2 of perimeter length
            (peri (/ (perimeter point1 point2 point3) 2))
        )
        (sqrt 
            ( 
                *
                peri
                    (- peri sideA)
                    (- peri sideB)
                    (- peri sideC)                
            )           
        ) 
    )
)

; calculates perimeter, area, interior angles
(define (calculate-triangle point1 point2 point3)
    (let* (
        ; gets triangle side lengths
        (sideA (distance point1 point2))
        (sideB (distance point2 point3))
        (sideC (distance point1 point3))
        
        ; gets triangle perimeter 
        (peri (perimeter point1 point2 point3))
        
        ; gets triangle area
        (ari (area point1 point2 point3))

        ; gets the radian value of each triangle
        ; side using Law of Cosines
        (sideA-rad (cosine-law sideB sideC sideA))
        (sideB-rad (cosine-law sideC sideA sideB))
        (sideC-rad (cosine-law sideB sideA sideC))
    )
        ; displays triangle side lengths 
        (display "Side 1 = ") (display (thousandth sideA)) (newline)
        (display "Side 2 = ") (display (thousandth sideB)) (newline)
        (display "Side 3 = ") (display (thousandth sideC)) (newline)

        ; displays triangle perimeter
        (display "Perimeter = ") (display (thousandth peri)) (newline)
        
        ; displays triangle area
        (display "Area = ") (display (thousandth ari)) (newline)
        
        ; displays triangle angles
        (display "Angle 1 = ") (display (hun-thousandth sideA-rad)) (display "   ") 
            (display (hun-thousandth (degree sideA-rad))) (newline)
        (display "Angle 2 = ") (display (hun-thousandth sideB-rad)) (display "   ") 
            (display (hun-thousandth (degree sideB-rad))) (newline)
        (display "Angle 3 = ") (display (hun-thousandth sideC-rad)) (display "   ") 
            (display (hun-thousandth (degree sideC-rad))) (newline)
        
        ; addl. newline to match output.txt
        (newline)
    )
)
