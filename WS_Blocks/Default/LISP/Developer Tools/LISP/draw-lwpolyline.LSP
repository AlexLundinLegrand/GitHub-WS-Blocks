;;; --- driver---
;;; Notes:
;;; Loop to create list of points
;;; Ends loop when user types Close or any substring of close, not case sensitive
;;; Alex Lundin 06-14-2017
(defun 	c:driver ( / CLASS P1 P2 PLLAYER PLLINETYPE PLWIDTH PTLIST)

	(if												;-if
		(setq p1 (getpoint "\nSpecify start point: "))						;-user chooses point for p1
		(progn											;--progn wrap on if statements
		(setq ptList (cons p1 ptList))								;set ptList to all the elements of ptList, with p1 added to the beginning
			(while										;---while
			  	(listp p2)								;continue loop while p2 is a list, use listp function to check
				(initget "Close")							;initget is a tool to allow or disallow certain user responses, this particular one only allows the string "Close"
				(setq p2 (getpoint p1 "\nSpecify next point [Close]: "))		;use set p2 to point received from user through getpoint, display p1 during the user selection and a message "Specify next point [Close]:"
					(if								;----if
						(not (eq p2 "Close"))					;----p2 is not equal to "Close"
						(progn							;-----progn wrap to package if statements
						(setq ptList (cons p2 ptList ))				;set ptList to all the elements of ptList, with p2 added to the beginning
						(setq p1 p2)
						)							;-----end progn wrap on if statements
					)								;----end if
			)										;---end while
		)											;--end progn wrap on if statements
	)												;-end if

	(setq ptList (reverse ptList))									;reverse the entire ptList, because using cons builds list backwards
	(setq class 0)											;set other arguments for draw-lwpolyline function call
	(setq plLayer "0")
	(setq plWidth 0.05)
	(setq plLinetype "BYLAYER")
	(draw-lwpolyline ptList class plLayer plWidth plLinetype)

)





;;; --- draw-lwpolyline---
;;; Notes:
;;; class-holder must be 0 for lwpolyline dxfcodes
;;; Alex Lundin 06-14-2017
(defun draw-lwpolyline (point-list class-holder polyline-layer polyline-width polyline-linetype)
;;; arguments
;;;	point-list			
;;;	class-holder	must be 0	
;;; 	polyline-layer			
;;;	polyline-width			
;;;	polyline-linetype		
  	(entmakex
	  		(append
			(list
			(cons 0 "LWPOLYLINE")	      
                       	(cons 100 "AcDbEntity")
                       	(cons 100 "AcDbPolyline")
			(cons 6 polyline-linetype)
			(cons 8 polyline-layer)
			(cons 40 polyline-width)
			(cons 41 polyline-width)
			(cons 43 polyline-width)
                       	(cons 90 (length point-list))
                      	(cons 70 class-holder))
       			(mapcar (function (lambda (p) (cons 10 p))) point-list))
	)
)