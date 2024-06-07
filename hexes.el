(progn
  (defun deg-to-rad (degrees) (* pi (/ degrees 180.0)))
  (defun rad-to-deg (radians) (* (/ 180.0 pi) radians))
  ;; Apothem is distance from center of hex to middle of one side.
  (defun area-of-hex (apothem)
    (let* ((hypotenuse (/ apothem (cos (deg-to-rad 30))))
	   (perimeter (* 6 hypotenuse))
	   (area (/ (* perimeter apothem) 2)))
      (format "apothem: %f\nhypotenuse: %f\nperimeter: %f\narea: %f"
	      apothem hypotenuse perimeter area))))

(area-of-hex 2.5)"apothem: 2.500000
hypotenuse: 2.886751
perimeter: 17.320508
area: 21.650635"

(* (* 20 20) 21.650635)8660.254
(* 30 20)600
