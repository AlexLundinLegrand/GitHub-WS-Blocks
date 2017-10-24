; --- rdsetup Function ---
; Sets up Room Detail modelspace with ROOMNAME and PRODUCTS
; Alex Lundin 05-31-2017
(defun c:rdsetup (
		 /
	 
		 )
	(vl-load-com)

  
(vlax-for LAYOUT (vla-get-layouts(vla-get-activedocument(vlax-get-acad-object)))
  (cond
    ((>(vla-get-taborder LAYOUT)1)
      (vl-catch-all-apply 'vla-delete (list LAYOUT))
    ) 
    ((=(vla-get-taborder LAYOUT)1)
      (not(vl-catch-all-error-p (vl-catch-all-apply 'vla-put-name(list LAYOUT "PAPER"))))
    )
  )  
)
	(setq ss (ssget "x" '((0 . "MTEXT")(8 . "RD_Rooms_Text"))))
  	(cond														;-conditional block
		((/= ss nil)												;-conditional statement
		(command "erase" "p" "")	
		)													;-end conditional statement
	)
  
	(setq ss (ssget "x" '((0 . "MTEXT")(8 . "RD_Products_Text"))))
  	(cond														;-conditional block
		((/= ss nil)												;-conditional statement
		(command "erase" "p" "")	
		)													;-end conditional statement
	)

  	(command "-layer" "make" "RD_Products_Text" "color" "7" "" "LType" "Continuous" "" "")					;create bridges layer
	(command "-layer" "make" "RD_Products_Text" "color" "7" "" "LType" "Continuous" "" "")					;create mstp layer


  
	(setq startxcoord 22004.5000)
        (setq startycoord 11220.0000)
  	(setq startpoint (list startxcoord startycoord))
  	(setq xspacer 1188.8885)
  	(setq yspacer 660.0000)
  	(setq xdirection 1)
  	(setq ydirection -1)
  	(setq infromationyoffset 600.0000)
  	(setq informationspacer 100.0000)
  	(setq informationdirection -1)
  	(setq informationdirectionreset 1)

;;;  	open text file for reading
  	(setq file1 (open (strcat (getvar 'DWGPREFIX) "Room_Details_Extraction.txt")  "r"))

;;;  	let user know if text file cannot be found
  	(cond																			;-cond block
	  	((= file1 nil)																	;-- cond statement if error handler for no text file
		(princ "Room_Details_Extraction.txt not found in folder with RD.dwg")
		(quit)
		)																		;-- end cond statement
	)																			;- end cond block

;;;  	read the first line of the text file in, which is the location of the drawing that was extracted from
;;;  	this is the priming read for the following while loop
	(setq xcoord startxcoord)
        (setq ycoord startycoord)
  	(setq ycoord (+ ycoord infromationyoffset))

  	(setq insertionpoint (list xcoord ycoord))
  	(setq drawingLocation (read-line file1))														;read in first line of Room_Details_Extraction.txt which will be drawing location
  	(setq rooms (read-line file1))																
  	(setq numberOfProducts (read-line file1))
	(setq numberOfProducts (atoi numberOfProducts))
  	(repeat numberOfProducts
	  	(progn
	  	(setq product (read-line file1))
	  	(if	(/= product nil)														;-- if statment to protect list from nil
			(setq products (cons product products))												;build products list
		)																	;read the next line into product
		)
	)
	
  	(while 	(/= rooms nil)
		
          
          	(if
                  	(> xcoord 35000)
                  	(progn
			(setq xcoord startxcoord)
                        (setq ycoord (+ ycoord (* yspacer ydirection)))
                        (setq insertionpoint (list xcoord ycoord))
                       	)
               	)
          	(if
                  	(/= rooms nil)
                  	(progn
			(setq textlayer "RD_Rooms_Text")
			(setq attachmentpoint 1)
			(entmake-mtext insertionpoint rooms attachmentpoint textlayer)
                        )
              	)

          	(if
                  	(/= products nil)
                  	(progn
			(setq ycoord (+ ycoord (* informationspacer informationdirection)))
			(setq insertionpoint (list xcoord ycoord))
			(setq textlayer "RD_Products_Text")
			(setq attachmentpoint 1)
			(entmake-mtext insertionpoint products attachmentpoint textlayer)
			(setq ycoord (+ ycoord (* informationspacer informationdirectionreset)))
                        )
              	)
	  
          	(setq xcoord (+ xcoord (* xspacer xdirection)))
          	(setq insertionpoint (list xcoord ycoord))
	  	(setq rooms (read-line file1))																
	  	(setq numberOfProducts (read-line file1))
		(setq numberOfProducts (atoi numberOfProducts))
	  	(repeat numberOfProducts
		  	(progn
		  	(setq product (read-line file1))
		  	(if	(/= product nil)														;-- if statment to protect list from nil
				(setq products (cons product products))												;build products list
			)																	;read the next line into product
			)
		)

	)

  	(close file1 )
)





(defun entmake-mtext (
			insertionPoint text attachmentpoint layer
                     /
                     )

		;;;http://forums.augi.com/showthread.php?28641-Create-MTEXT-with-Lisp
	    	(entmake
	    	(list
	      	(cons 0 "MTEXT")         		;; Entity Name
	      	(cons 100 "AcDbEntity")  		;; Subclass Marker
	      	(cons 410 "Model")       		;; Space
	      	(cons 8 layer)         			;; Layer
	      	(cons 100 "AcDbMText")   		;; Subclass Marker
	      	(cons 10 insertionPoint) 		;; Insertion Point
	      	(cons 40 4.5)            		;; Text Height
	      	(cons 71 attachmentpoint)              	;; Attachment Point 1 = Top left; 2 = Top center; 3 = Top right; 4 = Middle left; 5 = Middle center; 6 = Middle right; 7 = Bottom left; 8 = Bottom center; 9 = Bottom right
	      	(cons 1 text)    			;; Text Content
	      	(cons 7 "Arial")			;; text style
	      	)
	   	)

  )