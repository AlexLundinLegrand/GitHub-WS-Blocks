; --- WSFRONT Function ---
; Alex Lundin 5-30-2017
(defun C:WSFRONT( / 	BLOCKS
		 )													;GLOBAL VARIABLES -- None
  															;START WSFRONT
  
	(setq blocks(ssget "x" '((0 . "INSERT")(8 . "WATTSTOPPER"))))							;select all blocks on WATTSTOPPER layer, store to blocks varaible
		(cond
		  	((/= blocks nil)
			(command "draworder" "p" "" "front")
			)
		)



  	;;;this part sets any block on top of all others by sending it to the front of the draw order last
  	;;;such at the RACCESS modem and attenea, which needs to be infront of the network enclosures wipeout
	(setq blocks(ssget "x" '((0 . "INSERT")(8 . "WATTSTOPPER"))))							;select all blocks on WATTSTOPPER layer, store to blocks varaible
  
	(cond
	  	((/= blocks nil)											;-conditional statement
	 	(setq cntr 0)												;set counter variable cntr to 0 before each loop
 		(setq cmax (sslength blocks))
		(while 	(< cntr cmax)											;--while loop, loop continues will counter cntr is less than length of blocks selection set
			(setq block(ssname blocks cntr))								;set bridge varaible to the member in blocks that corresponds to the current counter cntr variable

			(setq ent block)										;set ent to the entity stored in block
			(setq truename (blocktruename ent))								;call blocktruename function with the ent parameter set in the line above, set the truename variable to the final result

			(if												;-inner if
			  	(OR											;-- or inside the if
				(= truename "RACCESS")
				)											;-- end or
			  	(progn											;--progn wrap the if

				(command "draworder" block "front")
				  
				)											;--end progn wrap
			)												;-end inner if

			(setq cntr(+ cntr 1))  										;increment counter cntr    
		)													;--end while
		)													;-end conditional statement
	)



  
  	(command "regenall")
  	(princ)
)															;END WSFRONT
															






; --- WSBACK Function ---
; Alex Lundin 5-30-2017
(defun C:WSBACK( / 	BLOCKS
		 )													;GLOBAL VARIABLES -- None
  															;START WSFRONT
  	;;;this part sets any block on top of all others, by sending it to the back of the draw order before the others
  	;;;such at the RACCESS modem and attenea, which needs to be infront of the network enclosures wipeout
	(setq blocks(ssget "x" '((0 . "INSERT")(8 . "WATTSTOPPER"))))							;select all blocks on WATTSTOPPER layer, store to blocks varaible
  
	(cond
	  	((/= blocks nil)											;-conditional statement
	 	(setq cntr 0)												;set counter variable cntr to 0 before each loop
 		(setq cmax (sslength blocks))
		(while 	(< cntr cmax)											;--while loop, loop continues will counter cntr is less than length of blocks selection set
			(setq block(ssname blocks cntr))								;set bridge varaible to the member in blocks that corresponds to the current counter cntr variable

			(setq ent block)										;set ent to the entity stored in block
			(setq truename (blocktruename ent))								;call blocktruename function with the ent parameter set in the line above, set the truename variable to the final result

			(if												;-inner if
			  	(OR											;-- or inside the if
				(= truename "RACCESS")
				)											;-- end or
			  	(progn											;--progn wrap the if

				(command "draworder" block "" "front")
				  
				)											;--end progn wrap
			)												;-end inner if

			(setq cntr(+ cntr 1))  										;increment counter cntr    
		)													;--end while
		)													;-end conditional statement
	)

  
	(setq blocks(ssget "x" '((0 . "INSERT")(8 . "WATTSTOPPER"))))							;select all blocks on WATTSTOPPER layer, store to blocks varaible
		(cond
		  	((/= blocks nil)
			(command "draworder" "p" "" "BACK")
			)
		)
  	(command "regenall")
  	(princ)
)
															;END WSFRONT