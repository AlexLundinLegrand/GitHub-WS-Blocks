; --- WSTBEXPORT Function ---
; Alex Lundin 07-04-2017
(defun C:WSTBEXPORT ( / ATTTAG ATTVAL DRAWINGLOCATION EN2 ENLIST ENTTYPE LAYERS TAGS TB TEXTFILE VALUES OUTPUTTEXTFILELOCATION )
  	(vl-load-com)
	(setq tb (car(entsel "\n Select the block with desired atrribute values: ")))					;prompt user to select a title block, store the entity name to tb	
  	
  	
	(setq en2(entnext tb))												;set en2 to ename of the group codes for tb
  	(setq enlist(entget en2))											;set enlist to the group codes of en2
  	(setq enttype 0)												;initialize entttype to 0 (makes while loop false by default)
  	(setq attTag 1)													;initialize attTag to 0 (makes condition false by default)
  	(setq enttype(cdr(assoc 0 enlist)))										;set enttype to the second element of the 0 group code in enlist
  		(while (= enttype "ATTRIB")										;loop while enttype is "ATTRIB"
			(setq attTag(cdr(assoc 2 enlist)))								;set attTag to the second element of the second associative property (a blocks attribute name) of enlist
  			(setq attVal(cdr(assoc 1 enlist)))								;set attVal to the second element of the first associative property (a value associated with the above attribute) of enlist
				(cond 	((AND(/= attTag nil)(/= attTag "SHEET"))					;conditional statement only when value is stored in attTag
					(setq Tags(append Tags(list attTag)))						;add the attTag memeber to the list Tags
					(setq Values(append Values(list attVal)))					;add the attVal member to the list Values
					)
				)
  			(setq en2(entnext en2))										;get the codes of the next sub entity of the titleblock (this will drill down farther with each loop)
			(setq enlist(entget en2))									;set enlist to the group codes of this new sub entity
			(setq enttype(cdr(assoc 0 enlist)))								;set the enttype to the second member of the first associative value of the group codes (entity type)
															;loop will repeat until group code is not equal to "ATTRIB"	  			
		)
  
  	;;; for loop, iteratres through each layer in document
  	;;; stores each to list called table
	(vlax-for lyr
	(vla-get-layers
	(vla-get-activedocument
	(vlax-get-acad-object)
	)
	)
	(setq layers (cons (vla-get-name lyr) layers))
	)
  	(setq layers (reverse layers))
  
  	(if
	  	(/= Values nil)												;if attribute values exist
	  	(progn
		(setq drawingLocation (getvar "DWGPREFIX"))
		(setq textFile (open (strcat (getvar 'DWGPREFIX) "AutoCAD_Title_Blocks.txt") "w" ) )
		(textFileWriteAttributes textFile Tags Values)
		(textFileWriteLayers textFile layers)
		(close textFile)
		(setq outPutTextFileLocation (strcat (getvar 'DWGPREFIX) "AutoCAD_Title_Blocks.txt"))
		(alert
			(strcat
			"Export from this drawing sucessful.
			\n
			\nVerify this location is where you want the text file:
			\n"
			outPutTextFileLocation
			"\n
			\nNow opening the text file.
			\nVerify the data:
			\n"
			)	
		)
		(startapp "notepad" outPutTextFileLocation)
		)
	)
)															;END WSTBEXPORT





(defun textFileWriteAttributes (textFile Tags Values / string CMAX CNTR TAG VALUE)

  		;write lines
	  	(setq string "Title_Block_Attributes:")
  		(write-line string textFile)

  		(setq cntr 0)
  		(setq cmax (length Tags))
  		(while 	(< cntr cmax)
		  	(setq Tag (nth cntr Tags))
		  	(setq string Tag)
		  	(write-line string textFile)
			(setq Value (nth cntr Values))
		  	(setq string Value)
		  	(write-line string textFile)
		  	(setq cntr (+ cntr 1))
		)

  
)





(defun textFileWriteLayers (textFile layers / string CMAX CNTR ISREVLAYER LAYERNAME ONOROFF ACADOBJECT ACTIVEDOCUMENT LAYEROBJECT LAYERTABLE REVPREFIX)

  		;write lines
	  	(setq string "Title_Block_Layers:")
  		(write-line string textFile)

  		(setq cntr 0)
  		(setq cmax (length layers))
  		(while 	(< cntr cmax)
		  	(setq layerName (nth cntr layers))
		  	(setq revPrefix "_WS_REV_")

		  	(setq isRevLayer (vl-string-search revPrefix layerName))

		  	;;; only write if Revision Layer is true
			(if
			  	(/= isRevLayer nil)
			  	(progn
			  	(setq string layerName)
			  	(write-line string textFile)
				(setq acadobject (vlax-get-Acad-Object))
				(setq activedocument (vla-get-activedocument acadobject))
				
			 	(setq LayerTable (vla-get-layers activedocument))
				(setq layerObject (vla-item LayerTable layerName))
				(setq onOrOff (vla-get-layeron layerObject))
				(setq freezeOrThaw (vla-get-freeze layerObject))
				(vlax-release-object acadobject)
				;;; determine if writing ON or OFF
			  	(if
				  	(=  onOrOff :vlax-true)
				  	(progn
					(setq string "ON")
					)
				  	(progn
					(setq string "OFF")
					)
				)
			  	(write-line string textFile)
				
				;;; determine if writing ON or OFF
			  	(if
				  	(=  freezeOrThaw :vlax-true)
				  	(progn
					(setq string "FREEZE")
					)
				  	(progn
					(setq string "THAW")
					)
				)
			  	(write-line string textFile)
			  	)
			)
		  	(setq cntr (+ cntr 1))
		)

  		
)




; --- WSTBIMPORT  Function ---
; Alex Lundin 09-05-2017
(defun C:WSTBIMPORT ( / ACADOBJECT ACTIVEDOCUMENT FREEZEORTHAW FREEZEORTHAWLIST LAYERNAME LAYEROBJECT LAYERTABLE ONOROFF ONOROFFLIST ATTTAG ATTVAL DRAWINGLOCATION EN2 ENLIST ENTTYPE LAYERS TAGS TB TEXTFILE VALUES BLOCK BLOCKS CMAX CMAX2 CNTR CNTR2 DXF67 EN ENTDXF RETURNDATALIST STATES TAG VALUE)
  	(vl-load-com)

	(setq textFile (open (strcat (getvar 'DWGPREFIX) "AutoCAD_Title_Blocks.txt") "r" ) )
  	(if
	  	(/= textFile nil)
	  	(progn
		(setq returnDataList (textFileReadData textFile))
		(setq Tags (nth 0 returnDataList))
	  	(setq Values (nth 1 returnDataList))
		(setq Layers (nth 2 returnDataList))
		(setq onOrOffList (nth 3 returnDataList))
		(setq freezeOrThawList (nth 4 returnDataList))
		(close textFile)
		)
	  	(progn
		(princ "No AutoCAD_Title_Blocks.txt file found to import")
		(princ)
		(exit)
		)
	)
	


	(setq blocks (ssget "x" (list (cons 0 "INSERT"))))
		       
	(setq cntr 0)
  	(setq cmax (sslength blocks))
  	(while	(< cntr cmax)
		(setq block (ssname blocks cntr))
	  	(setq entDxf (entget block))
	  	(setq dxf67 (cdr (assoc 67 entDxf)))

	  	(if
		  	;;; if we are not in modelspace
		  	(= dxf67 1)
		  	(progn
			(setq en (entnext block))
			(if
			  	(/= en nil)
			  	(progn
			  	(setq enlist(entget en))										;set enlist to the group codes of en2
			  	(setq enttype 0)											;initialize entttype to 0 (makes while loop false by default)
			  	(setq attTag 1)												;initialize attTag to 0 (makes condition false by default)
			  	(setq enttype(cdr(assoc 0 enlist)))									;set enttype to the second element of the 0 group code in enlist
		  		(while (= enttype "ATTRIB")										;loop while enttype is "ATTRIB"
					(setq attTag(cdr(assoc 2 enlist)))								;set attTag to the second element of the second associative property (a blocks attribute name) of enlist
		  			(setq attVal(cdr(assoc 1 enlist)))								;set attVal to the second element of the first associative property (a value associated with the above attribute) of enlist
						(cond 	((/= attTag nil)								;conditional statement only when value is stored in attTag


					  		(setq cntr2 0)
					  		(setq cmax2 (length Tags))
					  		(while 	(< cntr2 cmax2)
							  	(setq Tag (nth cntr2 Tags))
								(setq Value (nth cntr2 Values))

							  	(if
								  	(= Tag attTag)
								  	(progn
									(setq enlist (subst (cons 1 Value)(assoc 1 enlist) enlist))
									(entmod enlist)
									)
								)
							  	(setq cntr2 (+ cntr2 1))
							)
							)
						)
		  			(setq en(entnext en))										;get the codes of the next sub entity of the titleblock (this will drill down farther with each loop)
					(setq enlist(entget en))									;set enlist to the group codes of this new sub entity
					(setq enttype(cdr(assoc 0 enlist)))								;set the enttype to the second member of the first associative value of the group codes (entity type)
																	;loop will repeat until group code is not equal to "ATTRIB"	  			
				)
				)
			  )
			  )
		)
	  	(setq cntr (+ cntr 1))
	)



	(setq cntr 0)
	(setq cmax (length layers))
	(while 	(< cntr cmax)
	  	(setq layerName (nth cntr layers))
	  	(setq onOrOff (nth cntr onOrOffList))
		(setq freezeOrThaw (nth cntr freezeOrThawList))


		(setq acadobject (vlax-get-Acad-Object))
		(setq activedocument (vla-get-activedocument acadobject))
		
	 	(setq LayerTable (vla-get-layers activedocument))
		(setq layerObject (vla-item LayerTable layerName))
		

		;;; determine if writing ON or OFF
	  	(if
		  	(=  onOrOff "ON")
		  	(progn
			(vla-put-layeron layerObject :vlax-true)
			)
		  	(progn
			(vla-put-layeron layerObject :vlax-false)
			)
		)
		;;; determine if writing ON or OFF
	  	(if
		  	(=  freezeOrThaw "FREEZE")
		  	(progn
			(vla-put-freeze layerObject :vlax-true)
			)
		)
	  	(if
		 	(=  freezeOrThaw "THAW")
		  	(progn
			(vla-put-freeze layerObject :vlax-false)
			)
		)	  
		(vlax-release-object acadobject)
	  	(vlax-release-object activedocument)
	  	(setq cntr (+ cntr 1))
	)




	(princ)

  


  
)																;END WSTBIMPORT





(defun textFileReadData (textFile / string CMAX CNTR TAG VALUE LAYERS RETURNLIST STATES TAGS TEXTAREA TEXTLINE VALUES FREEZEORTHAWLIST ONOROFFLIST)

  		;read data

  		(setq textLine 1)

  		(while 	(/= textLine nil)
			
			(setq textLine (read-line textFile))
		  
		  	(if
			  	(= textLine "Title_Block_Attributes:")
			  	(progn
			  	(setq textArea "Attributes")
				(setq textLine (read-line textFile))
				)
			)
		  	(if
			  	(= textLine "Title_Block_Layers:")
			  	(progn
			  	(setq textArea "Layers")
				(setq textLine (read-line textFile))
				)
			)			  
		  	(if
			  	(= textArea "Attributes")
			  	(progn
				(setq Tags(append Tags(list textLine)))
				(setq textLine (read-line textFile))
				(setq Values(append Values(list textLine)))
				
				)
			)
		  	(if
			  	(AND (= textArea "Layers")(/= textLine nil))
			  	(progn
				(setq Layers(append Layers(list textLine)))
				(setq textLine (read-line textFile))
				(setq onOrOffList(append onOrOffList(list textLine)))
				(setq textLine (read-line textFile))
				(setq freezeOrThawList(append freezeOrThawList(list textLine)))				
				)
			)

		  	
		)

  		(setq returnlist (list Tags Values Layers onOrOffList freezeOrThawList))
)





