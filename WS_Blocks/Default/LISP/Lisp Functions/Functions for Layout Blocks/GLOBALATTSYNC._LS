(defun c:globalattsync (/ vlaobject )
  	(vl-load-com)
	(setq ss(ssget "x" '((0 . "INSERT")(8 . "WATTSTOPPER"))))							;create selection set from all objects that are inserted blocks
  	(setq cntr 0)
  	(setq cmax (sslength ss))											;set cmax to length of blocks selection set

		(while (< cntr cmax)											;set loop to continue while the counter varaible cntr is less that the number of objects in the selection set blocks
			(setq en(ssname ss cntr))									;set variable en to the name of the block that matches the current counter value
			  	(setq vlaobject (vlax-ename->vla-object en))						;helper function to handle Anonymous names -- set vlaobject to the converted vla name from the entity name of block
			    	(setq truename(vlax-get-property vlaobject						;set the truename variable  name of block
			        	(if (vlax-property-available-p vlaobject 'effectivename)			;only if the property called effective name exisits inside block (this entire block fails when if statement is false)
			            	'effectivename									;to the effective name property
			            	'name
			        	)
			    	)
				)
		 
		(setq EntDxf (entget en))

		(setq truenamelist (cons truename truenamelist)
		(princ cntr)
		(setq cntr(+ cntr 1))											;once while loop reaches this point, increment counter varaible by one			
  		)													;end while loop
		
	(setq truenamelist (reverse truenamelist))
	
	
      	(while  truenamelist																;create a list of unique room names from all the room names in the text file
        	(setq x (car  truenamelist)														;set x to first item from all the rooms
              		 truenamelist (vl-remove x (cdr  truenamelist))										;remove any other instances of x from the entire list
              		uniquetruenamelist (cons x uniquetruenamelist)													;add x to the new list
        		)
    		)																		;end while
	(setq uniquetruenamelist (reverse uniquetruenamelist))														;reverse the unique list to account for cons property

	
	(setq truenamelist uniquetruenamelist)
	
	
	
	
	
  	(setq cntr 0)
  	(setq cmax (length truenamelist))											;set cmax to length of blocks selection set
		
		(while (< cntr cmax)											;set loop to continue while the counter varaible cntr is less that the number of objects in the selection set blocks
			(setq en(ssname ss cntr))									;set variable en to the name of the block that matches the current counter value
			  	(setq vlaobject (vlax-ename->vla-object en))						;helper function to handle Anonymous names -- set vlaobject to the converted vla name from the entity name of block
			    	(setq truename(vlax-get-property vlaobject						;set the truename variable  name of block
			        	(if (vlax-property-available-p vlaobject 'effectivename)			;only if the property called effective name exisits inside block (this entire block fails when if statement is false)
			            	'effectivename									;to the effective name property
			            	'name
			        	)
			    	)
				)
		 
		(setq EntDxf (entget en))
		(if
		  	(/= (cdr (assoc 0 EntDxf )) "SEQEND" )
		  	(progn
			(vl-cmdf "_.attsync" "_N" truename)
			)
		)
		(princ cntr)
		(setq cntr(+ cntr 1))											;once while loop reaches this point, increment counter varaible by one			
  		)		
		
)


