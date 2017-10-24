; --- WSHLT Function ---
; Move selected Wattstopper block to "WATTSTOPPERHIGHLIGHT"
; Alex Lundin 01-21-2016
(defun C:WSHLT ( / blocks)
	(setvar "CMDECHO" 0)
	(PRINC(tblsearch "LAYER" "WATTSTOPPERHIGHLIGHT"))
	
	(command "_.Layer" "_Make" "WATTSTOPPERHIGHLIGHT" "_Color" "magenta" "" "LType" "Continuous" "" "")		;make layer called WATTSTOPPERHIGHLIGHT
	(setq block (car(entsel "\n Select the block to highlight: ")))							;set block variable to the entity name of the block to highlight
  	(setq en(entget block))												;set en variable to the entity name of the block variable
  	(setq wsBlockName (cdr(assoc 2 en)))										;set the wsBlockName variable to the second element (cdr function) of the 2 associative property (dxf code 2) of en (this is the block name)

  	(setq vlaobject (vlax-ename->vla-object block))									;helper function to handle Anonymous names -- set vlaobject to the converted vla name from the entity name of block
    	(setq truename(vlax-get-property vlaobject									;set the truename variable to the vlax getproperty of vlaobject
        	(if (vlax-property-available-p vlaobject 'effectivename)						;if there is a vlax property available on vlaobject, set the effectivename property to truename  variable
            	'effectivename
            	'name
        	)
    	)
	)
  
	(setq blocks(ssget "x" '((0 . "INSERT")(8 . "WATTSTOPPER"))))							;create selection set of all blocks on WATTSTOPPER layer
  
  	(cond														;conditional block
	  	((/= blocks nil)											;first conditional
			(setq cntr 0)
		 	(setq cmax (sslength blocks))									;set cmax to number of items in blocks
			(while 	(< cntr cmax)										;while loop when counter variable cntr less than cmax
				(setq block1(ssname blocks cntr))							;set blocks1 to the entity name in blocks that corresponds to the current value of cntr (ssname function)
	  			(setq en2(entget block1))								;set en2 to the entity name of block1
	  			(setq wsBlockName1(cdr(assoc 2 en2)))							;set the wsBlockName1 variable to the second element (cdr function) of the 2 associative property (dxf code 2) of en (this is the block name)
			  
  				(setq vlaobject1 (vlax-ename->vla-object block1))					;helper function to handle Anonymous names -- same as above
    				(setq truename1(vlax-get-property vlaobject1
        				(if (vlax-property-available-p vlaobject1 'effectivename)
            				'effectivename
            				'name
        				)
	    			)
				)
			  
	  			(cond											;nested conditional block
				  	((= truename1 truename)								;first conditional when truename1 equals truename
					(command "_chprop" block1 "" "la" "WATTSTOPPERHIGHLIGHT" "")  			;move block1 to WATTSTOPPERHIGHLIGHT
			
					)										;end first conditional
				)											;end nested conditional block
	  			(setq cntr(+ cntr 1))									;increment counter

			)												;end while loop
		 


		)													;end first conditional
  	)														;end conditional block

	  		
	(setvar "CMDECHO" 1)
)




; --- WSHLT2 Function ---
; Copy all items from certain layers
; Alex Lundin 01-21-2016
(defun C:WSHLT2 ( / blocks)
	(setvar "CMDECHO" 0)											;set system variable cmdecho to 0
	(setq block (car(entsel "\n Select the block to unhighlight: ")))					;set block variable to the entity name of the block to un highlight
  	(setq en(entget block))											;set en variable to the entity name of the block variable
  	(setq wsBlockName (cdr(assoc 2 en)))									;set the wsBlockName variable to the second element (cdr function) of the 2 associative property (dxf code 2) of en (this is the block name)
  
	(setq vlaobject (vlax-ename->vla-object block))								;helper function to handle Anonymous names -- set vlaobject to the converted vla name from the entity name of block
    	(setq truename(vlax-get-property vlaobject								;set the truename variable 
        	(if (vlax-property-available-p vlaobject 'effectivename)					;only if the property called effective name exisits inside block (this entire block fails when if statement is false)
            	'effectivename											;to the effective name property
            	'name
        	)
    	)
	)
  
	(setq blocks(ssget "x" '((0 . "INSERT")(8 . "WATTSTOPPERHIGHLIGHT"))))					;create selelection set of all blocks on WATTSTOPPERHIGHLIGHT layer
  	(cond													;conditional block
	  	((/= blocks nil)										;first conditional when blocks selection set is not nil
			(setq cntr 0)										;set counter to 0
  			(setq cmax (sslength blocks))								;set counter max to length of selection set blocks
			(while 	(< cntr cmax)									;while loop, while counter less than cmax
				(setq block1(ssname blocks cntr))						;set block1 to the entity name (ssname function) of the entity in the blocks selection set that corresponds to the current value of the counter varialbe cntr
	  			(setq en2(entget block1))							;set en2 to the entity name (entget) of block1
	  			(setq wsBlockName1(cdr(assoc 2 en2)))						;set wsBlockName1 variable to the second element (cdr function) of the 2 associative property (dxf code 2) of en2 (this is the block name)
			  
  				(setq vlaobject1 (vlax-ename->vla-object block1))				;helper function to handle Anonymous names -- same as above
    				(setq truename1(vlax-get-property vlaobject1
        				(if (vlax-property-available-p vlaobject1 'effectivename)
            				'effectivename
            				'name
        				)
	    			)
				)
			
			  
	  				(cond									;nested conditional block
					  	((= truename1 truename)						;first conditional when truename1 equals truename
						(command "_chprop" block1 "" "la" "WATTSTOPPER" "")  		;move the block to WATTSTOPPER layer
			
						)
					)
			  
	  			(setq cntr(+ cntr 1))								;increment counter cntr each loop (moves block1 variable to next item in blocks selection set with the ssname function)
			  
			)
		 
	)
  	)

	  		
	(setvar "CMDECHO" 1)											;set system variable cmdecho to 1

  	(command "clayer" "WATTSTOPPER")									;sets WATTSTOPPER current layer
	(command "_.purge" "LA" "WATTSTOPPERHIGHLIGHT" "N")							;purge command
	(prompt "\nPurge WATTSTOPPERHIGHLIGHT layer complete.")
)