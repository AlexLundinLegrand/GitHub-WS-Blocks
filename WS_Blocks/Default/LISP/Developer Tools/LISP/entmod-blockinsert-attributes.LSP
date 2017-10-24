; --- entmod-blockinsert-attributes Sub Function ---
; inserts block
; inserts any existing attributes from the block definition table
; file out attributes from list sent in from calling function
; Arguments:
; attributelist		list of attribute values passed in from block-builder
; Alex Lundin 03-31-2017
(defun entmod-blockinsert-attributes
       				(
				attributelist blkinsertionpoint layername blockname 
				/
				Attdefs ATTBLK ATTBLKNAME ATTRIBUTEDXF11 ATTRIBUTEINSERTIONPOINT ATTRIBUTEXCOORD ATTRIBUTEYCOORD BLOCKXCOORD
				BLOCKYCOORD DATA ENAME NEWINSERTIONPOINT NEWXCOORD NEWYCOORD NEXTENT entmod-blockinsert-attributes-cntr ATTRIBUTEDXF10 ATTRIBUTEVALUE NEWINSERTIONPOINTDXF10 NEWINSERTIONPOINTDXF11
				)
;;;arguments
;;;  	attributevalue - value to place into block
;;;  	blkinsertionpoint - insertion point for filled out block
;;;	layername - name of layer to insert block on
;;;	riserblockname - name of cabinet passed in from block-builder


	;;BLOCKTABLE search, call placeholder later
  
	(setq attblkname blockname)
	(setq entmod-blockinsert-attributes-cntr 0)
  	(setq blockxcoord (car blkinsertionpoint))
	(setq blockycoord (cadr blkinsertionpoint))
  
	(cond															;-cond block
		((setq Ename (tblobjname "block" attblkname))  									;--cond statement, get Parent entity name, when true continue conditional
		(setq NextEnt (entnext Ename))											;first sub entity

			(while NextEnt												;get ATTDEF subentities
			(setq Data (entget NextEnt))
				(if (= "ATTDEF" (cdr (assoc 0 Data)))
				(setq Attdefs (cons Data Attdefs))
				)
			(setq NextEnt (entnext NextEnt))
			)
		(setq attblk (if (= nil Attdefs) 0 1))										;set attblk variable to determine if block definition has attributes
		(and														;---and for outside items
				(entmake											;---first and, entmake insert for the block
				(list
				'(0 . "INSERT")
				'(100 . "AcDbBlockReference")
				(cons  8 layername)										;layer name
				(cons 66 attblk)										;0 or 1 value, if block has attributes it will be 1
				(cons  2 attblkname)
				(cons 10 blkinsertionpoint  )									;insert point
				(cons 41 1.0)
				(cons 42 1.0)
				(cons 43 1.0)
				)
				)												;---end first and


				(foreach x (reverse Attdefs)									;---second and, foreach loop for every item in attdefs

					(if
				  	(/= attributelist nil)
					  	(progn
						(setq attributevalue (nth entmod-blockinsert-attributes-cntr attributelist))	;get first attribute value from the list of them
						(setq entmod-blockinsert-attributes-cntr (+ entmod-blockinsert-attributes-cntr 1));increase counter for next loop
						)
					)
					(if
				  	(= attributelist nil)
					  	(progn
						(setq attributevalue "")							;get first attribute value from the list of them
						)
					)						
					(setq attributedxf11 (assoc 11 x))							;get 11th dxf code from item x, these next few lines calculate where the attribute is placed based on the block insertion point
					(setq attributeinsertionpoint (cdr attributedxf11))
					(setq attributexcoord (car attributeinsertionpoint))
					(setq attributeycoord (cadr attributeinsertionpoint))
					(setq newxcoord (+ blockxcoord attributexcoord))
					(setq newycoord (+ blockycoord attributeycoord))
					(setq newinsertionpointdxf11 (list newxcoord newycoord))




					(setq attributedxf10 (assoc 10 x))							;get 10th dxf code from item x, these next few lines calculate where the attribute is placed based on the block insertion point
					(setq attributeinsertionpoint (cdr attributedxf10))
					(setq attributexcoord (car attributeinsertionpoint))
					(setq attributeycoord (cadr attributeinsertionpoint))
					(setq newxcoord (+ blockxcoord attributexcoord))
					(setq newycoord (+ blockycoord attributeycoord))
					(setq newinsertionpointdxf10 (list newxcoord newycoord))


					  
					(entmake										;entmake the attribute with all the information associated with x
					(list
					'(0 . "ATTRIB")
					(assoc  8 x)
					(assoc 40 x)
					(cons  1 attributevalue)								;use the attributevalue passed in as a arguement
					(assoc 50 x)
					(assoc 41 x)
					(assoc 51 x)
					(assoc  7 x)
					(assoc 71 x)
					(assoc 72 x)
					(cons 10 newinsertionpointdxf10)							;use the new insertion points to locate the attribute based on where the block is inserted
					(cons 11 newinsertionpointdxf11)
					(assoc  2 x)
					(assoc 70 x)
					(assoc 73 x)
					(assoc 74 x)
					)
					)
				)												;---end second add
			(entmake '((0 . "SEQEND")(8 . "0")))    								;---third add, entmake SEQEND to signify final attribute at end of block
		)
		)														;--cond statement
		(T nil)
	)															;-end cond


)