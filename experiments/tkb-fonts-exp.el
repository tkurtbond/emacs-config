(x-list-fonts "-*-DejaVu Sans Mono-normal-r-normal-normal-*-*-*-*-c-*-iso10646-1")
(let* ((djv (car (x-list-fonts "-*-DejaVu Sans Mono-medium-r-normal-*-*-*-*-*-*-*-iso10646-1")))
       (split (split-string djv "-"))
       (_ (setf (nth  7 split) "14"))
       (djv (mapconcat #'identity split "-")))
  (set-frame-font djv))


(let* ((djv (car (x-list-fonts "-*-freemono-medium-r-normal-*-*-*-*-*-*-*-iso10646-1")))
       (split (split-string djv "-"))
       (_ (setf (nth  7 split) "14"))
       (djv (mapconcat #'identity split "-")))
  (set-frame-font djv))
