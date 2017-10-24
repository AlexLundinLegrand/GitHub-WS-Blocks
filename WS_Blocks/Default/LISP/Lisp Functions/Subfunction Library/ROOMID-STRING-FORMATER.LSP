;;; --- ROOMID-STRING-FORMATER  Sub Function ---											
;;; send stringin through string variable from calling function										
;;; formatted string is returned to caller												
;;;																	
;;; Alex Lundin 09-01-2017														
;;; example call															
;;; (setq formattedString (ROOMID-STRING-FORMATER string))										

(defun ROOMID-STRING-FORMATER

       	(
	;arguments
	string
	/
	;local variables
	CHARACTER COUNTER COUNTERMAX *ERROR* FORMATCHECK FORMATTEDSTRING RETURNVALUE STRING1 STRING2 STRING2COUNTER STRINGCOUNTER 
	)
  
;;; Arguments:																
;;;	string 		- any valid selection set											
;;; Return:																
;;;	returnValue	- formatted string												
	
  
  
  	(setq formatcheck 0)

	(setq counter 0)
  	(setq counterMax 0)
  	(if
	  	(/= string nil)
	  	(progn
		(setq string(strcase string))             									;sets string to uppercase
  		(setq counterMax (strlen string))										;sets counterMax to length of string passed into function
		)
	)
  
  	(while 	(< counter counterMax)
	  	(setq stringCounter (+ 1 counter))										;strings start at 1, so the stringCounter is 1 more than the loop counter
	  	(setq string2Counter (+ 2 counter))										;this string2Counter is used for the second half of the word
  		(setq character (substr string stringCounter 1))								;store character to variable
	  

		;;; if and structure to allow for multiple conditions
		;;; progn loop must follow the and portion to wrap all statements together
	  	(if
			(AND	
				(= character " ")(= stringCounter counterMax)							;if we are at the end of the string and there is a blank space
			)
		  	(progn
			(setq character "")											;set character to nothing, which removes the space
			(setq formatcheck 1)
			)
		)
	  
		(if
			(AND	(/= character "A")(/= character "B")(/= character "C")(/= character "D")(/= character "E")(/= character "F")
				(/= character "G")(/= character "H")(/= character "I")(/= character "J")(/= character "K")(/= character "L")
				(/= character "M")(/= character "N")(/= character "O")(/= character "P")(/= character "Q")(/= character "R")
				(/= character "S")(/= character "T")(/= character "U")(/= character "V")(/= character "W")(/= character "X")
				(/= character "Y")(/= character "Z")(/= character "1")(/= character "2")(/= character "3")(/= character "4")
				(/= character "5")(/= character "6")(/= character "7")(/= character "8")(/= character "9")(/= character "0")
				(/= character "_")(/= character "")
			)
		  	(progn
			(setq character "_")											;if string is not any of the declared values, then set character to _
			(setq formatcheck 1)
			)
		)
	  

		;;; string1 and string2 are not used for anything but might be useful for other functions
	  	(cond

		  	((= counter 0)												;when counter is 0
	  		(setq string1 nil)											;there is no string1
			(setq string2 (substr string string2Counter counterMax))						;string2 is the rest of the string after the character
			(setq formattedstring (strcat character))								;the formated string only has the character value
			)
			
		  	((/= counter 0)												;when counter is not 0
	  		(setq string1 (substr string 1 stringCounter))								;string1 is the portion of the string before the character
			(setq string2 (substr string string2Counter counterMax))						;string2 is the portion of the string after the character
			(setq formattedstring (strcat  formattedstring character))						;formattedstring is the previous formatted string plus the new character
			)
		)	  
	  	
	  	(setq counter (+ 1 counter))
	)

  	(setq returnValue formattedstring)


  

  
)