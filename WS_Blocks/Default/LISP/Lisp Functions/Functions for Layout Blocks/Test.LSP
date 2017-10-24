;; main program
(defun c:test (/ adoc acsp address data_list fname mtext_obj pname pt pt_str sheetnum LINETOEXECUTE NEWBLOCKNAME ORIGINALBLOCKNAME VLAOBJECT-BLOC)
  (vl-load-com)
  (setq adoc (vla-get-activedocument
        (vlax-get-acad-object)
      )
  )
  (if (= 1 (vlax-get-property adoc 'activespace))
    (setq acsp (vla-get-modelspace adoc))
    (setq acsp (vla-get-paperspace adoc))
  )
  ;; points range address
  (setq address "A2:C43")
  ;; sheet number
  (setq sheetnum 1)
 
 
 (if 
 
 ;; hard coded path
 (setq fname "C:\\WS_Blocks\\Default\\LISP\\Lisp Functions\\Functions for Layout Blocks\\Part Names For Replacement.xlsx")   
  (progn
(setq data_list (read_excel_by_cell_range fname sheetnum address))

(foreach line data_list

  	(setq originalBlockName (nth 0 line))
	(setq newBlockName (nth 1 line))
  	(setq lineToExecute (nth 2 line))

  	(setq blockFlag (tblsearch "BLOCK" originalBlockName))
  
  	(if
	  	(/= blockFlag nil)
	  	(progn
		(setq ss(ssget "x" '((0 . "INSERT")(8 . "WATTSTOPPER"))))							;create selection set from all objects that are inserted blocks
	  	(setq cntr 0)
	  	(setq cmax (sslength ss))											;set cmax to length of blocks selection set

		(while (< cntr cmax)												;set loop to continue while the counter varaible cntr is less that the number of objects in the selection set blocks
			(setq ent(ssname ss cntr))										;set variable ent to the name of the block that matches the current counter value
			(setq EntDxf (entget ent))
		  	(setq Dxf0 (cdr (assoc 0 EntDxf)))
		  
		  	(if
			  	(=  Dxf0 "INSERT")
		  		(progn
		  		(setq blockTrueName (ENTITY-TO-BLOCK-TRUENAME ent))

				(if
				  	(= blockTrueName originalBlockName)
				  	(progn
					(setq newDxf2 (cons 2 newBlockName))
					(setq originalDxf2 (assoc 2 EntDxf))
					  
					(entmod (subst newDxf2 originalDxf2 EntDxf))
					)
				)
		 		)
		  	)
			

			(setq cntr(+ cntr 1))											;once while loop reaches this point, increment counter varaible by one
		)
		
		  
		
		
		)
	)
 )
)
   )
  (princ)
  )

(princ)
;;===============================================================;;