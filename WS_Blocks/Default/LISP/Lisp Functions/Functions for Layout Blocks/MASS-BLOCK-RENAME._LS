;Progrm to add a prefix to all blocks in a drawing including nested blocks.
;Can be used where ther is a conflict when inseting large drawing that have blocks with same name

(defun c:mbr (/ blk bname temp1 star newname newstr)
 (setq NewStr (getstring "\nPlease enter a Prefix for all blocks  ")) 
 (setq     n   1
     blk (tblnext "BLOCK" 1)
  )
  
  (while (/= blk nil)
    (setq bname (cdr (assoc 2 blk)))
		(setq returnedList (mass-block-rename-string-format bname))
		(setq formatCheckBit (nth 0 returnedList))
		(setq formatedString (nth 1 returnedList))
		
		(if	
			(= formatCheckBit 1)
			(progn
			
			(setq newname formatedString)     
			(command "-rename" "b" bname newname)
			)
		)
		(Setq blk (tblnext "BLOCK"))
	)
(princ)
)




(defun mass-block-rename-string-format (string / CHARACTER CMAX CNTR FORMATCHECK STRING1 STRING2 STRING2CNTR STRINGCNTR formattedstring)
;;;	arguments:
;;;  	string, sent in from call

  	(setq formatcheck 0)
  	(setq string(strcase string))             														;sets string to uppercase
	(setq cntr 0)
  	(setq cmax (strlen string))																;sets cmax to length of string passed into function
  
  	(while 	(< cntr cmax)
	  	(setq stringcntr (+ 1 cntr))															;strings start at 1, so the stringcntr is 1 more than the loop cntr
	  	(setq string2cntr (+ 2 cntr))															;this string2cntr is used for the second half of the word
  		(setq character (substr string stringcntr 1))													;store character to variable
	  	  

;;;string1 and string2 are not used for anything but might be useful for other functions
	  	(cond

		  	((= cntr 0)																;when cntr is 0
	  		(setq string1 nil)															;there is no string1
			(setq string2 (substr string string2cntr cmax))												;string2 is the rest of the string after the character
			)
			
		  	((/= cntr 0)																;when cntr is not 0
	  		(setq string1 (substr string 1 stringcntr))												;string1 is the portion of the string before the character
			(setq string2 (substr string string2cntr cmax))												;string2 is the portion of the string after the character
			)
		)	  
		
		
;;;	  	if and structure to allow for multiple conditions
;;;	  	progn loop must follow the and portion to wrap all statements together
	  	(if
			(OR	
				(= string1 "V-")(= string1 "PS-")											;if we are at the end of the string and there is a blank space
			)
		  	(progn
			(setq formattedstring string2)										;formattedstring is the previous formatted string plus the new character
			(setq formatcheck 1)
			)
		)
	  
	  	
	  	(setq cntr (+ 1 cntr))
	)

  	(setq returnString (list formatcheck formattedstring ))

  )