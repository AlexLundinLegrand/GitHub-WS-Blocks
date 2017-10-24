;;; --- wsquotetoolbom ---
;;; Notes:
;;; Alex Lundin 06-18-2017
(defun c:wsquotetoolbom (
			/
			BLLIST BLSET BNA INSTPT INSTPT2 KEYDATA LEGENDX LEGENDX2 LEGENDY LISTNEWLEGNTH LISTTOTALLENGTH NAMELIST NUM OLDECHO OLDOSC OUTLIST QTY X BLOCKNAME DRAWINGLOCATION F1 LISTNEWLENGTH QUANTITY X
			)


 ;save current system settings and edit for routine
  (setq oldecho (getvar "cmdecho"))
  (setq oldosc (getvar "osnapcoord"))
  (setvar "cmdecho" 0)
  (setvar "osnapcoord" 1)


  
 ;create selection set and create list of blocks on WattStopper layer in Model Space
  (if (setq blSet (ssget "x" (list (cons 8 "WATTSTOPPER") (cons 410 "Model") (cons 0 "Insert"))))
    (progn
      (setq blList   (mapcar 'vlax-ename->vla-object								;use mapcar on each item from the list described below, the mapcar function is using vla-ename to object on each item
			     (vl-remove-if									;remove element if it fails the test condition below
			       'listp										;check if the follwing element is a list with listp
			       (mapcar 'cadr (ssnamex blSet))							;use ssnamex to return the entire list of all entity names in blSet, which will be the second item, so cadr is used to retrieve the second item
			     ) ;_ end of vl-remove-if
		     ) ;_ end of mapcar
	    nameList (vl-sort (mapcar '(lambda (X) (vla-get-effectivename x)) blList) '<)			;blList evaluates first, then create nameList from the truenames of each item in blList, use mapcar to define a in-line function to store each item from blList in x and loop through
	    listTotalLength  (length nameList)									;finally store the total length of nameList in listTotalLength
      ) ;_ end of setq
 ;End of Residential Symbol Handling

      (while nameList												;while nameList still exists
	(setq outList (cons (cons (car nameList)								;create the outList from the first item of nameList and the number of items that match it, accumlate each of these values for the entire list
				  (- listTotalLength								;find the number of items that match the first item, by subtract the listTotalLength, minus the listNewLength
				     (setq listNewLength (length (setq nameList					;set nameList to new list with all of the items that matched the first element removed
								  (vl-remove (car nameList) nameList)		;start evaluation of this section here, remove any items that match the first element of nameList, car returns the first element
							   ) ;_ end of setq
						   ) ;_ end of length
				     ) ;_ end of setq
				  ) ;_ end of -
			    ) ;_ end of cons
			    outList
		      ) ;_ end of cons
	) ;_ end of setq
      ) ;_ end of while

      (setq acadobject (vlax-get-Acad-Object))
      (setq drawingName (vla-get-fullname (vla-get-activedocument acadobject)))

      (setq drawingName (vl-filename-base drawingName))
      (setq drawingLocation (getvar 'DWGPREFIX))
      (setq f1 (open (strcat drawingLocation drawingName "_Quote_Tool_BOM.xml")  "w"))				;set f1 to to result of open on the string created from drawingLocation variable plus "Riser_Extraction.txt" open file for writing

      
      (sub-function-text-output-header f1 drawingName)								;call subfunction
      
      (foreach item (reverse outList)										;foreach loop, reverse entire outList list, set item to the current element in the loop
		  	(setq quantity (strcat (itoa (cdr item))))						;set quanitity to the string of the integer value of the second element of item
	          	(setq blockName (strcat (car item)))							;set block name to the string of the first element of item
			(setq returnList (sub-function-switch-formater blockName))
			(setq formatedBlockName (nth 0 returnList))
			(sub-function-text-output-formater formatedBlockName quantity f1)
      ) ;_ end of foreach

      (sub-function-text-output-footer f1)
      (close f1)
    ) ;_ end of progn
  ) ;_ end of if
	
  
 ;restore system settings
  (setvar "cmdecho" oldecho)
  (setvar "osnapcoord" oldosc)
  
  (princ)
) ;_ end of defun





;;; --- sub-function-text-output-header ---
;;; Notes:
;;; Alex Lundin 06-18-2017
(defun	sub-function-text-output-header
	(
	argument-f1 argument-drawingName
	/
	string
	)

  		;write lines
;;;	  	(setq string "<?xml version=\"1.0\" encoding=\"utf-8\"?>")
;;;  		(write-line string toh-f1)
		      
		(setq string (strcat "<MarkupSummary Version=\"1.0\" Document=\"" argument-drawingName "Bill of Material.pdf\">"))
		(write-line string argument-f1)
		
)




;;; --- sub-function-text-output-formater ---
;;; Notes:
;;; Alex Lundin 06-18-2017
(defun	sub-function-text-output-formater
	(
	argument-blockName argument-quantity argument-f1
	/
	string 
	)
  		;write lines
	  	(setq string "  <Markup>")
  		(write-line string argument-f1)	
		(setq string (strcat "    <product_name>"argument-blockName"</product_name>"))
		(write-line string argument-f1)
		(setq string (strcat "    <quantity>"argument-quantity"</quantity>"))
		(write-line string argument-f1)
		(setq string (strcat "  </Markup>"))
		(write-line string argument-f1)
)


	

  
;;; --- sub-function-text-output-header ---
;;; Notes:
;;; Alex Lundin 06-18-2017
(defun	sub-function-text-output-footer
	(
	argument-f1
	/
	string
	)

  		;write lines
	  	(setq string "</MarkupSummary>")
  		(write-line string argument-f1)
		
)





; --- sub-function-switch-formater ---
; accepts a switch
; adds -W to end of string
; Alex Lundin 07-07-2017
(defun sub-function-switch-formater
       			(
			argument-product
			/
			returnlist
			)
;;;  		arguments					
;;;		switch into argument-product			
;;;                                                       	
;;;  		return						
;;;		formated-switch-string				
;;;                                                       	

  	;;; set return result to argument sent in
  	;;; this ensures a non formated return for anything thats not a switch
	(setq formatedProductString argument-product)

  	;;; if product is a switch, add -W
	(if
		(OR
		(= argument-product "LMDM-101")(= argument-product "LMDW-101")(= argument-product "LMDW-102")(= argument-product "LMPW-101")(= argument-product "LMPW-102")(= argument-product "LMPS-104")
		(= argument-product "LMPW-101")(= argument-product "LMPW-102")(= argument-product "LMSW-101")(= argument-product "LMSW-102")(= argument-product "LMSW-103")(= argument-product "LMSW-104")
		(= argument-product "LMSW-105")(= argument-product "LMSW-108")(= argument-product "LMTS-101-CCT")(= argument-product "LMSW-105-CCT")
		)
	  	(progn
		(setq formatedProductString (strcat argument-product "-W"))
		)
	)
  
  	(setq returnlist (list formatedProductString))
)
