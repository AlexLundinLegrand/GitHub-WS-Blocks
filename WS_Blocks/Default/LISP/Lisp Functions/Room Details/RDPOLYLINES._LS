;;;RDPOLYLINES functions
;;;these functions are designed to help engineer format polylines drawing inside a Room Details template


; --- P1 ---
; Notes:
; sets necessary arguments for subfunctions and calls them
; fillets the entity returned from subfunctions
; Arguments: None
; Alex Lundin 06-21-2017
(defun c:P1
       		(
		 
	     	/
		CLASS CREATEDENAME CREATEDPOINTLIST FILLETRADIUS PDRETURN PLLAYER PLLINETYPE PLWIDTH
		)

	(setq class 0)												;set other arguments for draw-lwpolyline function call
	(setq plLayer "0")
	(setq plWidth 0.6)
	(setq plLinetype "BYLAYER")
  
  	(setq pdReturn (plinedriver class plLayer plWidth plLinetype))						;call plinedriver and set pdreturn to returned result
  	(setq createdPointList (nth 0 pdReturn))								;seperate return list
  	(setq createdEname (nth 1 pdReturn))
  
	(setq filletRadius "6.0")										;fillet item created retunred by subfunctions
	(command "_.fillet" "p" "r" filletRadius createdEname)
)





; --- P2 ---
; Notes:
; sets necessary arguments for subfunctions and calls them
; fillets the entity returned from subfunctions
; Creates list of points from created polyline
; Removes first and last point from list
; Creates selection set from new point list
; Deletes anything in the selection set
; Arguments: None
; Alex Lundin 06-21-2017
(defun c:P2
       		(
		
	     	/
		CLASS CMAX CNTR CREATEDENAME CREATEDPOINTLIST CREATEDPOINTLISTFIRST CREATEDPOINTLISTLAST CREATEDPOINTLISTSECOND CREATEDPOINTLISTSECONDFROMLAST DXF5 EHANDLE ENAME ENT ENTDXF FILLETRADIUS ORGANIZEDCREATEDPOINTLIST PDRETURN PLLAYER PLLINETYPE PLWIDTH POINT SSCREATEDPOINTLIST
		organizedPointList pointList
		)


	(setq class 0)												;set other arguments for draw-lwpolyline function call
	(setq plLayer "0")
	(setq plWidth 0.6)
	(setq plLinetype "BYLAYER")
  
  	(setq pdReturn (plinedriver class plLayer plWidth plLinetype))						;call plinedriver and set pdreturn to returned result
  	(setq createdPointList (nth 0 pdReturn))								;seperate return list
  	(setq createdEname (nth 1 pdReturn))
  
	(setq filletRadius "6.0")										;fillet item created retunred by subfunctions
	(command "_.fillet" "p" "r" filletRadius createdEname)
  



	(setq entDXF(entget createdEname))									;set the varaible entDxf to the list of dxfcodes from the ent varaible
	(setq eName (cdr (assoc -1 entDXF )))

  	(setq dxf5 (assoc 5 entdxf))										;set dxf5 to the 5 dxf code of entdxf
  	(setq createdEhandle (cdr dxf5))									;set ehandle to the second element for dxf5

  
  	;;; if createdPointList exists
  	;;; organize the points based on the Room Details connections
  	;;; only add items that are not the first or last item
  	;;; this drops them past the largest controllers so they don't get caught in the fence selection and deleted
  	;;; create fence selections set
	(if
	  	(/= createdPointList nil)	
	  	(progn

		(setq createdPointListFirst 0)
		(setq createdPointListSecond 1)
		(setq createdPointListSecondFromLast (- (length createdPointList) 2))
		(setq createdPointListLast (- (length createdPointList) 1))
		
		(setq cntr 0)											;initialize counter to 0 for while loop
	  	(setq cmax (length createdPointList))								;set cmax to length of blocks selection set
		(while 	(< cntr cmax)										;set loop to continue while the counter varaible cntr is less that the number of objects in the selection set blocks
			(setq point(nth cntr createdPointList))							;set variable ent to the name of the entity that matches the current counter value

		  	(if
			  	(AND
				(/= cntr createdPointListFirst)(/= cntr createdPointListSecond)
				(/= cntr createdPointListSecondFromLast)(/= cntr createdPointListLast)
				)
			  	(progn
				(setq organizedcreatedPointList (cons point organizedcreatedPointList))		;add point to organizedcreatedPointList when not first or last item
				)
			)
		  	
			(setq cntr(+ cntr 1))									;once while loop reaches this point, increment counter varaible by one
		)




		
		(if
		  	(/= organizedcreatedPointList nil)
		  	(progn
			(setq sscreatedPointList (ssget "_F" organizedcreatedPointList))			;create fence selection set from the organizedcreatedPointList
			)
		)
		)
	)



	(setq cntr 0)												;initialize counter to 0 for while loop
  	(if
	  	(/= sscreatedPointList nil)
	  	(progn
	  	(setq cmax (sslength sscreatedPointList))							;set cmax to length of blocks selection set
		(while 	(< cntr cmax)										;set loop to continue while the counter varaible cntr is less that the number of objects in the selection set blocks
			(setq ent(ssname sscreatedPointList cntr))						;set variable ent to the name of the entity that matches the current counter value

			(setq entDXF(entget ent))								;set the varaible entDxf to the list of dxfcodes from the ent varaible
			(setq eName (cdr (assoc -1 entDXF )))

		  	(setq dxf5 (assoc 5 entdxf))								;set dxf5 to the 5 dxf code of entdxf
		  	(setq eHandle (cdr dxf5))								;set ehandle to the second element for dxf5

		  
		  	(if
			  	(/= eHandle createdEhandle)							;compare eHandle to plineHandle
			  	(command "erase" ent "")							;erase any objects that are not the original polyline from initial user selection, pline
			)
			(setq cntr(+ cntr 1))									;once while loop reaches this point, increment counter varaible by one
		)
		)
	)
  	
)



;;; --- plinedriver---
;;; Notes:
;;; Accepts arguments from P1 or P2
;;; Loop to create list of points
;;; Ends loop when user types Close or any substring of close, not case sensitive
;;; Calls draw-lwpolyline
;;; returns list of points created by user
;;; Alex Lundin 06-21-2017
(defun 	plinedriver (pd-class pd-plLayer pd-plWidth pd-plLinetype  / DPRETURN PDRETURNLIST CLASS P1 P2 PLLAYER PLLINETYPE PLWIDTH PTLIST)
  
;;; arguments
;;;	pd-class
;;;	pd-plLayer
;;;	pd-plWidth
;;;	pd-plLinetype

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

	(setq dpReturn (draw-lwpolyline ptList pd-class pd-plLayer pd-plWidth pd-plLinetype))		;call draw polyine subfunction and set return value to a variable dpreturn
	(setq pdReturnList (list ptList dpReturn))							;create return list from the pointlist and the dpreturn from subfunction
)





;;; --- draw-lwpolyline---
;;; returns name of entity, if created
;;;
;;; Notes:
;;; class-holder must be 0 for lwpolyline dxfcodes
;;; Alex Lundin 06-14-2017
(defun draw-lwpolyline (point-list class-holder polyline-layer polyline-width polyline-linetype / ENAME)
;;; arguments
;;;	point-list			
;;;	class-holder	must be 0	
;;; 	polyline-layer			
;;;	polyline-width			
;;;	polyline-linetype
	(setq ename
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
  
  	(setq dpReturnEname ename)
)







































