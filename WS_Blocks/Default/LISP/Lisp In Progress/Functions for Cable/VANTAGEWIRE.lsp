; --- VANTAGECABLEFUNCTIONS ---
; These functions are designed to assist the Engineer draw wire between Vantage parts
; Alex Lundin 12-19-2016


; --- VANTAGEWIRE ---
; Greedy algorithm
; This function is designed to wire parts for the Engineer
; The first loops use a greedy algorithm to sort objects by distance
; The user will designate the first object and from there, the code finds the object that is closest to the first
; Now the object that was found is the current object and the code looks for the next closest
; Each time a item is found it is removed from the list so there are not duplicates
; Alex Lundin 12-19-2016
(defun C:VANTAGEWIRE
       		(
		
		/
		CMAX CNTR DXF10 EN ENTDXF MAINLIST SS STARTBLOCK SUBLIST TRUENAME XCOORD YCOORD DXF5 HANDLE STARTHANDLE innerlooplist
		MAX2 CNTR2 DIST FINALLIST HANDLE2 HANDLE3 ITEM ITEM2 ITEM3 ITEMPOSITION NUMBER NUMBER2 NUMBER3 OUTERLOOPLIST POINT1 POINT2 TRUENAME2 TRUENAME3 XCOORD2 XCOORD3 YCOORD2 YCOORD3
		)

	(vl-load-com)
	(alert "Select all the blocks on the wire run:")
  	(command "_select" pause)
	(setq ss (ssget "p"))												;set variable ss, to the previous selction set

  	(setq startblock(car(entsel "\n Select the first block on the wire run: ")))						;set variable startblock to the first block on the run
	;;; place startblock on the list
  	;;; save the handle of this block as starthandle
  	;;; this will be used later to protect the main list from a duplicate entry of the start block
  	;;; handles are the only naming convention that autocad keeps consistent when operating on blocks
  	;;; entity names and regular block names are dynamic
  	;;; handles are static
  	(setq en startblock)
	(setq truename (blocktruename en))										;call blocktruename function
  	(setq cntr -1)													;set counter variable to -1 to ensure that this first block has the lowest value
	(setq entdxf (entget en))											;set entdxf to the dxf codes of the en entity
	(setq dxf10 (assoc 10 entdxf))											;set dxf10 to the 10th dxf code of entdxf
	(setq xcoord (nth 1 dxf10))											;set xcoord to the nth 1 of dxf10
	(setq ycoord (nth 2 dxf10))											;set ycoord to the nth 2 of dxf10
  	(setq dxf5 (assoc 5 entdxf))											;set dxf5 to the 5 dxf code of entdxf
  	(setq starthandle (cdr dxf5))											;set starthandle to the second element for dxf5
  	(setq dxf8 (assoc 8 entdxf))
  	(setq startlayer (cdr dxf8))
	(setq sublist (list truename starthandle xcoord ycoord cntr))							;create sublist for current item  
	(setq mainlist (cons sublist mainlist))										;add sublist to mainlist

  	;;; this loop creates the list of all parts on the station bus run
  	;;; order of each item follows:
  	;;; truename of block, handle of block, xcoord, ycoord, # in the main list
  	;;; the number value, stored at the end of each item, is respective to the counter variable at
  	;;; each point in the loop. This value is used for organizing and removing items later
  	(setq cntr 0)
  	(setq cmax (sslength ss))											;set cmax to length of blocks selection set
		(while 	(< cntr cmax)											;- while, set loop to continue while the counter varaible cntr is less that the number of objects in the selection set blocks
			(setq en(ssname ss cntr))									;set variable en to the entity name of the block that matches the current counter value
			(setq truename (blocktruename en))								;call blocktruename function
		  	(setq entdxf (entget en))									;set entdxf to the dxf codes of the en entity
			(setq dxf5 (assoc 5 entdxf))									;set dxf5 to the 5th dxf code of entdxf
		  	(setq handle (cdr dxf5))									;set handle to the second element of dxf5
		  	(setq dxf10 (assoc 10 entdxf))									;set dxf10 to the 10th dxf code of entdxf
		  	(setq xcoord (nth 1 dxf10))									;set xcoord to the nth 1 of dxf10
		  	(setq ycoord (nth 2 dxf10))									;set ycoord to the nth 2 of dxf10
			(setq sublist (list truename handle xcoord ycoord cntr))					;create sublist for current item


		  
		  	(if												;--if block
			  	;;; protects list from duplicating the startblock item, which is already added
			  	(/= starthandle handle)									;--if statement, when the en variable doesn't equal startblock
			  	(progn											;---progn wrap if
				(setq mainlist (cons sublist mainlist))							;add sublist to mainlist, which accumulates during the loop
				)											;---end progn wrap
			)												;--end if
		  	
			(setq cntr(+ cntr 1))										;once while loop reaches this point, increment counter varaible by one			
  		)													;-end while loop


  	(setq mainlist (reverse mainlist))										;reverse the list because cons adds them in reverse order


	;;; this outer loop starts at the startblock, then loops through the remaining blocks
	;;; this inner loop finds the block that is closest to the current item from the entire list
	;;; the resulting list is the items sorted by shortest distance between objects
	;;; outer loop and inner loop both use the nth operator to pull the seperate pieces apart from each item in the list
  	;;; the counter variable is stored as a number at the end of each item so we can parse the list and remove items later
  	
  	(setq outerlooplist mainlist)


  	
    	(setq cntr 0)
  	(setq item(nth cntr outerlooplist))										;set item to the nth member of outerlooplist that corresponds to the cntr variable
  	(setq finallist (cons item finallist))
  	(setq outerlooplist (removenth cntr outerlooplist))								;remove item from outerlooplist
  	(setq cmax (length outerlooplist))										;set cmax to length of blocks selection set
		(while 	(< cntr cmax)											;- while, set loop to continue while the counter varaible cntr is less that the number of objects in the selection set blocks
			
		  	
			(setq truename (nth 0 item))									;set truename to 1st element of item
			(setq handle (nth 1 item))									;set handle to 2nd element of item
		  	(setq xcoord (nth 2 item))									;set xcoord to 3rd element of item
		  	(setq ycoord (nth 3 item))									;set ycoord to 4th element of item
		  	(setq number (nth 4 item))									;set number to 5th element of item
		  
		    	(setq cntr2 0)
		  	(setq cmax2 (length outerlooplist))								;set cmax2 to length of blocks selection set

		  	;;; this loop keeps outerlooplist in the same order while it's looping through
		  	;;; it also adds the current counter value to each item
		  	;;; this is the cntr2 value and it serves as the index for the list
		  	;;; after the list is completed, the next line sorts the list by distance
		  	;;; thats okay though because the orignal indexs are still stored on each item
		  	;;; these indexs on innerloop can be used to remove the same item from outerloop
				(while 	(< cntr2 cmax2)									;--while, set loop to continue while the counter varaible cntr2 is less that the number of objects in the innerlooplist
					(setq item2(nth cntr2 outerlooplist))						;set item to the nth member of innerlooplist that corresponds to the cntr2 variable
					(setq truename2 (nth 0 item2))							;set truename to 1st element of item
					(setq handle2 (nth 1 item2))							;set handle to 2nd element of item
		  			(setq xcoord2 (nth 2 item2))							;set xcoord to 3rd element of item
		  			(setq ycoord2 (nth 3 item2))							;set ycoord to 4th element of item
		  			(setq number2 (nth 4 item2))							;set number to 5th element of item




				  	;;; the section calculates the distances
				  	;;; there are two options:
				  	;;; greedy decision algorithm
				  	;;; compare the object from the inner list to the previously wired point
				  	;;; thrifty decision algorithm
				  	;;; compare the object from the inner list to the starting object chosen by user
				  	;;; to convert between the two just swap how point 1 is set below by uncommenting the one you want and commenting out the other

				  	;;; greedy decision algorithm
				  	;;; use last block as reference, changes each time
				  	;;; set the first point to the xcoord and ycoord, they correspond to whatever the previous object was
;;;					(setq point1 (list xcoord ycoord))
				  
				  	;;; thrifty decision algorithm
				  	;;; use beginning object as reference every time
				  	;;; set the first point to the starting object selected by user
					(setq point1 startpoint)
				  
				  	(setq point1 (list xcoord ycoord))						;make points
					(setq point2 (list xcoord2 ycoord2))
				  	(setq dist (distance point1 point2))						;calculate distance
				  	(setq sublist (list truename2 handle2 xcoord2 ycoord2 dist cntr2))		;create sublist for current item, I did not put the number2 back on because it is not useful at this point anymore. instead I put cntr2, which gives the index in the list, this will allow me to remove that item from the list later
				  	(setq innerlooplist (cons sublist innerlooplist))				;add sublist to innerlooplist
					(setq cntr2(+ cntr2 1))								;once while loop reaches this point, increment counter varaible by one			
		  		)											;--end while
			(setq innerlooplist (vl-sort innerlooplist (function (lambda (x y)(< (nth 4 x)(nth 4 y))))))	;sort according to nth 4th element of the item, which is distance. Low to high
		  	
			(setq item3(nth 0 innerlooplist))								;set item3 to the nth member of innerlooplist at 0, which is the first item, which is the shortest distance
			(setq truename3 (nth 0 item3))									;set truename3 to 1st element of item
			(setq handle3 (nth 1 item3))									;set handle3 to 2nd element of item
		  	(setq xcoord3 (nth 2 item3))									;set xcoord3 to 3rd element of item
		  	(setq ycoord3 (nth 3 item3))									;set ycoord3 to 4th element of item
		  	(setq dist (nth 4 item3))									;set dist to 5th element of item
		  	(setq number3 (nth 5 item3))									;set number3 to 6th element of item
		  	(setq finallist (cons item3 finallist))								;add item3 to the finallist
		  
		  	;;; now at this point, we use the counter value from innerloop, cntr2
		  	;;; this value was stored in the inner loop as the 5th element
		  	;;; at this point it is stored in number3 when we pulled the item apart
		  	;;; use this number as the index in outerlooplist, the original list
			(setq itemposition number3) 									;set itemposition to number3, which is the cntr2 variable, which is the index on the outerlooplist
		  	(setq outerlooplist (removenth itemposition outerlooplist))					;remove item from outerlooplist using the removenth function which expects a n value and a list
			(setq innerlooplist nil)									;nil finallist
		  	(setq item item3)										;set item to the value of item3, which we just found was the closest item
			(setq cntr(+ cntr 1))										;once while loop reaches this point, increment counter varaible by one
  		)	
  	(setq finallist (reverse finallist))

  	(setq cntr 0)
  	(setq cmax (length finallist))											;set cmax to length of finallist

  	(setq item(nth cntr finallist))											;set item to the nth member of innerlooplist that corresponds to the cntr variable
	(setq truename (nth 0 item))											;set truename to 1st element of item
	(setq handle (nth 1 item))											;set handle to 2nd element of item
	(setq xcoord (nth 2 item))											;set xcoord to 3rd element of item
	(setq ycoord (nth 3 item))											;set ycoord to 4th element of item
	(setq dist (nth 4 item))											;set distance to 5th element of item
	(setq number (nth 5 item))											;set number to 6th element of item
  	(setq cntr(+ cntr 1))												;once while loop reaches this point, increment counter varaible by one
  	(setq xcoordprevious xcoord)											;store coordinates
  	(setq ycoordprevious ycoord)
  
		(while 	(< cntr cmax)											;- while, set loop to continue while the counter varaible cntr is less that the number of objects in the selection set blocks
		  	
			(setq item(nth cntr finallist))									;set item to the nth member of innerlooplist that corresponds to the cntr variable

			(setq truename (nth 0 item))									;set truename to 1st element of item
			(setq handle (nth 1 item))									;set handle to 2nd element of item
		  	(setq xcoord (nth 2 item))									;set xcoord to 3rd element of item
		  	(setq ycoord (nth 3 item))									;set ycoord to 4th element of item
		  	(setq dist (nth 4 item))									;set distance to 5th element of item
		  	(setq number (nth 5 item))									;set number to 6th element of item

		  	(setq point1 (list xcoordprevious ycoordprevious))						;set point1 to previous coords
		  	(setq point2 (list xcoord ycoordprevious))							;set point2 to new x and previous y
		  	(setq point3 (list xcoord ycoord))								;set point3 to new coords

		  	(setq point-list (list point1 point2 point3))							;set point-list to all points on line
		  	(setq cls 0)											;set cls to the type of class of polyline, 0
		  	(setq layer startlayer)									;set layer for polyline
		  	(ent-poly point-list cls layer)

		  
			(setq cntr(+ cntr 1))										;once while loop reaches this point, increment counter varaible by one
	  		(setq xcoordprevious xcoord)											;store coordinates
  			(setq ycoordprevious ycoord)
  		)													;-end while loop
  	
	(princ)  
)









;;; send entity name of block in through en variable from calling function
;;; the en argument from the caller gets passed into the en variable in this function
;;; the truename is returned to the caller
(defun blocktruename (en / vlaobject truename)
  
 	(setq vlaobject (vlax-ename->vla-object en))						;helper function to handle Anonymous names -- set vlaobject to the converted vla name from the entity name of block
	(setq truename(vlax-get-property vlaobject						;set the truename variable  name of block
	(if (vlax-property-available-p vlaobject 'effectivename)				;only if the property called effective name exisits inside block (this entire block fails when if statement is false)
		'effectivename									;to the effective name property
		'name
		)
	)
	) 

)




(defun removenth
       		( n l / i )
    	(setq i -1)
    	(vl-remove-if '(lambda ( x ) (= (setq i (1+ i)) n)) l)
)




;;; expects 1 as the class value
(defun ent-spline-fitpoint
       			(point-list cls layer)
  
  			(entmakex
			(append
			  	(list
				(cons 0 "SPLINE")
                          	(cons 100 "AcDbEntity")
                          	(cons 100 "AcDbSpline")
				(cons 8 layer)
                          	(cons 74 (length point-list))
                          	(cons 71 cls)
				)
                    		(mapcar (function (lambda (p) (cons 10 p))) point-list)
		    		(mapcar (function (lambda (p) (cons 11 p))) point-list)
		    	)
			)

)	


;;;expects 0 as class value
(defun ent-poly (point-list cls layer)
	(entmakex (append (list (cons 0 "LWPOLYLINE")
                          (cons 100 "AcDbEntity")
                          (cons 100 "AcDbPolyline")
			  (cons 8 layer)
                          (cons 90 (length point-list))
                          (cons 70 cls))
                    (mapcar (function (lambda (p) (cons 10 p))) point-list)))
)


