; --- WSCPRID ---
; This function is designed to fill out blocks ROOMID's placed into a ROOM already labeled by the Engineer
; saftey factor is coded in, to allow code to not pull attributes from blocks far away.
; Alex Lundin 05-08-2017
(defun C:WSCPRID
       		(
		
		/
		ATTTAG ATTVAL BLOCKS CMAX CMAX2 CNTR CNTR2 COPYIEDROOMDID 
		DIST DISTANCELIST DXF10 DXF5 EN EN2 EN3 ENDXF ENDXF2 ENLIST 
		ENLIST2 ENSUB ENT ENTDXF GROUP0 GROUP66 HANDLE HANDLE2 
		HANDLE3 INNERLOOPLIST ITEM ITEM2 ITEM3 MAINLIST NUMBER 
		NUMBER2 NUMBER3 OUTERLOOPLIST POINT1 POINT2 safteyfactor SS SS1 SUBLIST 
		TRUENAME TRUENAME2 TRUENAME3 XCOORD XCOORD2 XCOORD3 YCOORD YCOORD2 YCOORD3
		)

	(vl-load-com)

	(setq safteyfactor 36)
  
	(command "_.Layer" "_Make" "NOID" "_Color" "30" "" "LType" "Continuous" "" "")					;create NOID LAYER
  	(setq ss (ssadd))
  	(setq ss1 (ssadd))
	(setq blocks(ssget "x" '((0 . "INSERT")(8 . "WATTSTOPPER"))))							;create selection set from all objects that are inserted blocks on the WATTSTOPPER layer 
	(setq cntr 0)													;initialize counter to 0 for while loop
  	;;; this loop seperates all blocks into two selection sets
  	;;; ss is emtpy ROOMID blocks
  	;;; ss1 is full ROOMID blocks
  		(cond													;-conditional block
		  	((/= blocks nil)										;-when blocks are not nil
			(while 	(< cntr (sslength blocks))								;set loop to continue while the counter varaible cntr is less that the number of objects in the selection set blocks
				(setq en(ssname blocks cntr))								;set variable en to the name of the block that matches the current counter value
				(setq enlist(entget en))								;set the varaible enlist to the list of entities from the en varaible
				(setq group66(cdr(assoc 66 enlist)))							;set the varaible group66 to the second value of the 66th entitiy of the enlist variable
		  
					(cond	((= group66 1)								;start the conditional statement 1 loop when group66 varaible = 1
						(setq en2(entnext en))							;set en2 to the next sub entity of en
						(setq enlist2(entget en2))						;set enlist2 to the list of entities of en2
						(setq attTag(cdr(assoc 2 enlist2)))					;set attTag varaible to the second, plus all remaining values of the second member of enlist2
						(setq attVal(cdr(assoc 1 enlist2)))					;set attVal varaible to the second, plus all remaining values of the first memeber of enlist2
						 
							(if
							  	(AND							;-start nested and inside if
								(= attTag "ROOMID")(= attVal "")			;if the attTag is ROOMID and the attVal is nothing
								)							;-end nested and inside if
						   		(setq ss (ssadd en ss))
							)

							(if
							  	(AND							;-start nested and inside if
								(= attTag "ROOMID")(/= attVal "")			;if the attTag is ROOMID and the attVal is nothing
								)							;-end nested and inside if
						   		(setq ss1 (ssadd en ss1))
							)
						 
						 
  															;end if
						) 	 								;end conditional statement 1
	 
					)										;end conditional statement block											
			(setq cntr(+ cntr 1))										;once while loop reaches this point, increment counter varaible by one			
	  		)												;end while loop
			)												;-end when blocks not nil conditional
		)


  	;;; this loop creates the information list of all parts with a empty ROOMID
  	;;; order of each item follows:
  	;;; truename of block, handle of block, xcoord, ycoord, # in the main list, entityname
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
			(setq sublist (list truename handle xcoord ycoord cntr en))					;create sublist for current item

			(setq mainlist (cons sublist mainlist))								;add sublist to mainlist, which accumulates during the loop

			(setq cntr(+ cntr 1))										;once while loop reaches this point, increment counter varaible by one			
  		)													;-end while loop


  	(setq mainlist (reverse mainlist))										;reverse the list because cons adds them in reverse order
  	(setq outerlooplist mainlist)
  	(setq mainlist nil)
  
  	;;; this loop creates the information list of all parts with a full ROOMID
  	;;; order of each item follows:
  	;;; truename of block, handle of block, xcoord, ycoord, # in the main list
  	;;; the number value, stored at the end of each item, is respective to the counter variable at
  	;;; each point in the loop. This value is used for organizing and removing items later
  	(setq cntr 0)
  	(setq cmax (sslength ss1))											;set cmax to length of blocks selection set
		(while 	(< cntr cmax)											;- while, set loop to continue while the counter varaible cntr is less that the number of objects in the selection set blocks
			(setq en(ssname ss1 cntr))									;set variable en to the entity name of the block that matches the current counter value
			(setq truename (blocktruename en))								;call blocktruename function
		  	(setq entdxf (entget en))									;set entdxf to the dxf codes of the en entity
			(setq dxf5 (assoc 5 entdxf))									;set dxf5 to the 5th dxf code of entdxf
		  	(setq handle (cdr dxf5))									;set handle to the second element of dxf5
		  	(setq dxf10 (assoc 10 entdxf))									;set dxf10 to the 10th dxf code of entdxf
		  	(setq xcoord (nth 1 dxf10))									;set xcoord to the nth 1 of dxf10
		  	(setq ycoord (nth 2 dxf10))									;set ycoord to the nth 2 of dxf10
			(setq sublist (list truename handle xcoord ycoord cntr en))					;create sublist for current item

			(setq mainlist (cons sublist mainlist))								;add sublist to mainlist, which accumulates during the loop

			(setq cntr(+ cntr 1))										;once while loop reaches this point, increment counter varaible by one			
  		)													;-end while loop


  	(setq mainlist (reverse mainlist))										;reverse the list because cons adds them in reverse order
	(setq innerlooplist mainlist)
	(setq mainlist nil)

  
	;;; this outer loop starts at the startblock, then loops through the remaining blocks
	;;; this inner loop finds the block that is closest to the current item from the entire list
	;;; the resulting list is the items sorted by shortest distance between objects
	;;; outer loop and inner loop both use the nth operator to pull the seperate pieces apart from each item in the list
  	;;; the counter variable is stored as a number at the end of each item so we can parse the list and remove items later
  	
  	
    	(setq cntr 0)
  	(setq item(nth cntr outerlooplist))										;set item to the nth member of outerlooplist that corresponds to the cntr variable
  	(setq cmax (length outerlooplist))										;set cmax to length of blocks selection set
		(while 	(< cntr cmax)											;- while, set loop to continue while the counter varaible cntr is less that the number of objects in the selection set blocks
			
		  	
			(setq truename (nth 0 item))									;set truename to 1st element of item
			(setq handle (nth 1 item))									;set handle to 2nd element of item
		  	(setq xcoord (nth 2 item))									;set xcoord to 3rd element of item
		  	(setq ycoord (nth 3 item))									;set ycoord to 4th element of item
		  	(setq number (nth 4 item))									;set number to 5th element of item
		  	(setq en (nth 5 item))
		  
		    	(setq cntr2 0)
		  	(setq cmax2 (length innerlooplist))								;set cmax2 to length of blocks selection set

		  	;;; this loop keeps outerlooplist in the same order while it's looping through
		  	;;; it also adds the current counter value to each item
		  	;;; this is the cntr2 value and it serves as the index for the list
		  	;;; after the list is completed, the next line sorts the list by distance
		  	;;; thats okay though because the orignal indexs are still stored on each item
		  	;;; these indexs on innerloop can be used to remove the same item from outerloop
				(while 	(< cntr2 cmax2)									;--while, set loop to continue while the counter varaible cntr2 is less that the number of objects in the innerlooplist
					(setq item2(nth cntr2 innerlooplist))						;set item to the nth member of innerlooplist that corresponds to the cntr2 variable
					(setq truename2 (nth 0 item2))							;set truename to 1st element of item
					(setq handle2 (nth 1 item2))							;set handle to 2nd element of item
		  			(setq xcoord2 (nth 2 item2))							;set xcoord to 3rd element of item
		  			(setq ycoord2 (nth 3 item2))							;set ycoord to 4th element of item
		  			(setq number2 (nth 4 item2))							;set number to 5th element of item
					(setq en2 (nth 5 item2))
				  
				  	;;; the section calculates the distances
				  	;;; greedy decision algorithm
				  	;;; use last block as reference, changes each time
				  	;;; set the first point to the xcoord and ycoord, they correspond to whatever the previous object was
					(setq point1 (list xcoord ycoord))
				  

				  
					(setq point2 (list xcoord2 ycoord2))						;set the second point to the current object in the inner loop x and y coord
				  	(setq dist (distance point1 point2))						;calculate distance
				  	(setq sublist (list truename2 handle2 xcoord2 ycoord2 dist cntr2 en2))		;create sublist for current item, I did not put the number2 back on because it is not useful at this point anymore. instead I put cntr2, which gives the index in the list, this will allow me to remove that item from the list later
				  	(setq distancelist (cons sublist distancelist))					;add sublist to innerlooplist
					(setq cntr2(+ cntr2 1))								;once while loop reaches this point, increment counter varaible by one			
		  		)											;--end while
			(setq distancelist (vl-sort distancelist (function (lambda (x y)(< (nth 4 x)(nth 4 y))))))	;sort according to nth 4th element of the item, which is distance. Low to high
		  	
			(setq item3(nth 0 distancelist))								;set item3 to the nth member of innerlooplist at 0, which is the first item, which is the shortest distance
			(setq truename3 (nth 0 item3))									;set truename3 to 1st element of item
			(setq handle3 (nth 1 item3))									;set handle3 to 2nd element of item
		  	(setq xcoord3 (nth 2 item3))									;set xcoord3 to 3rd element of item
		  	(setq ycoord3 (nth 3 item3))									;set ycoord3 to 4th element of item
		  	(setq dist (nth 4 item3))									;set dist to 5th element of item
		  	(setq number3 (nth 5 item3))									;set number3 to 6th element of item
			(setq en3 (nth 6 item3))
		  

		  	(if
			  	(<= dist safteyfactor)
			  	(progn
				;;; section to get ROOMID from full block
				(setq enDXF (entget en3))
	  			(setq enSub (entnext en3))
				(setq enDXF2 (entget enSub))
	  			(setq group0(cdr(assoc 0 enDXF2)))
				(setq ent en3)
				(while	(= group0 "ATTRIB")									;start the conditional predicate 1 loop when group66 varaible = 1
					(setq enSub(entnext ent))								;set en2 to the next sub entity of ent
					(setq enDXF2(entget enSub))								;set enlist2 to the list of entities of en2
					(setq attTag(cdr(assoc 2 enDXF2)))							;set attTag varaible to the second, plus all remaining values of the second member of enlist2
					(setq attVal(cdr(assoc 1 enDXF2)))							;set attVal varaible to the second, plus all remaining values of the first memeber of enlist2
		 
					(cond											;conditional block
		  				((= attTag "ROOMID")								;first conditional statement--when the attVal contains nothing 
						(setq copyiedroomdid (cdr (assoc 1 enDXF2)))						;change the value stored in the 1st dxf code of enDXF2 to the segment variable
						)  										;end first conditional statement
					)											;end conditional block
		  
					(setq ent enSub)
					(setq enDXF2(entget enSub))
					(setq group0(cdr(assoc 0 enDXF2)))
				) 												;end conditional statement block


				;;; section to fill out empty block
				(setq enDXF (entget en))
	  			(setq enSub (entnext en))
				(setq enDXF2 (entget enSub))
	  			(setq group0(cdr(assoc 0 enDXF2)))
				(setq ent en)
				(while	(= group0 "ATTRIB")									;start the conditional predicate 1 loop when group66 varaible = 1
					(setq enSub(entnext ent))								;set en2 to the next sub entity of ent
					(setq enDXF2(entget enSub))								;set enlist2 to the list of entities of en2
					(setq attTag(cdr(assoc 2 enDXF2)))							;set attTag varaible to the second, plus all remaining values of the second member of enlist2
					(setq attVal(cdr(assoc 1 enDXF2)))							;set attVal varaible to the second, plus all remaining values of the first memeber of enlist2
		 
					(cond											;conditional block
		  				((= attTag "ROOMID")								;first conditional statement--when the attVal contains nothing 
						(entmod (subst (cons 1 copyiedroomdid) (assoc 1 enDXF2) enDXF2))		;change the value stored in the 1st dxf code of enDXF2 to the segment variable
						)  										;end first conditional statement
					)											;end conditional block
		  
					(setq ent enSub)
					(setq enDXF2(entget enSub))
					(setq group0(cdr(assoc 0 enDXF2)))
				)
				)
			)

 

			(setq distancelist nil)										;nil distancelist

			(setq cntr(+ cntr 1))										;once while loop reaches this point, increment counter varaible by one
		  	(setq item(nth cntr outerlooplist))								;set item to the nth member of outerlooplist that corresponds to the cntr variable
  		)	

  	
	(princ)  
)