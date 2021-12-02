(defun deg-to-rad (degrees) (* pi (/ degrees 180.0)))
(defun rad-to-deg (radians) (* (/ 180.0 pi) radians))
;; Apothem is distance from center of hex to middle of one side.
(defun area-of-hex (apothem)
  (let* ((hypotenuse (/ apothem (cos (deg-to-rad 30))))
	 (perimeter (* 6.0 hypotenuse))
	 (area (/ (* perimeter apothem) 2.0)))
    (format "apothem: %f\nhypotenuse: %f\nperimeter: %f\narea: %f"
	    apothem hypotenuse perimeter area)))
;; Remember the argument is apothem, not width, so apothem of 5 means
;; 10 miles wide!

(area-of-hex 5) 
"apothem: 5.000000
     hypotenuse: 5.773503
     perimeter: 34.641016
     area: 86.602540"
