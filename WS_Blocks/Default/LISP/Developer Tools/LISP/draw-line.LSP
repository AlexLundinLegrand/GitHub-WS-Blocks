(defun 	c:driver ( / p1 p2 layer linetype)

  	(setq p1 (getpoint (princ "Select startpoint: ")))
  	(setq p2 (getpoint (princ "Select endpoint: ")))
  	(setq layer "0")
  	(setq linetype "BYLAYER")
	(draw-line p1 p2 layer linetype)

)



; --- draw-line ---
; Notes:
; Alex Lundin 06-14-2017
(defun draw-line (start-point end-point line-layer line-linetype)
;;;	Arguments:
;;;	startpoint
;;;	endpoint
;;;	line-layer
;;;	line-linetype
  	(entmakex
	  		(append
			(list
			(cons 0 "LINE")	      
			(cons 6 line-linetype)
			(cons 8 line-layer)
                       	(cons 10 start-point)
			(cons 11 end-point)
			)
			)
	)
)