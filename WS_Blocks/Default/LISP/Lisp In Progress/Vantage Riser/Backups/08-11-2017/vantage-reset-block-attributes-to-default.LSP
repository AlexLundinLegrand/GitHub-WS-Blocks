;;; --- reset-block-attributes-to-default Function ---
;;; use block table from active drawing
;;; get default values from table
;;; loop through all blocks on wattstopper layer
;;; replace any existing attribute values with default ones
;;; Arguments
;;; entity's name passed into ent variable
;;; Alex Lundin 12-21-2016
(defun c:reset-block-attributes-to-default
	(
	/ ACADOBJ ADOC BLOCKS LAYER VLAOBJECT
	ATTS ATTTAG-FROMBLOCK ATTTAG-FROMTABLE ATTTAGOBJ-FROMBLOCK ATTTAGOBJ-FROMTABLE ATTVALUE-FROMTABLE BLOCK CMAX CMAX2 CNTR CNTR2 E-TYPE EN ENAME-BLOCK ITEM LST SUBLIST TAG TRUENAME VALUE VLAOBJECT-BLOCK VLAOBJECT-BLOCK2 Y Z
	)
  
	(Setq lst '())                                                                             	; empty list
  
  	(setq layer "WATTSTOPPER")
  	(setq blocks (ssget "x" (list (cons 0 "INSERT") (cons 8 layer))))
  
	(setq cntr 0)
  	(setq cmax (sslength blocks))


	(setq acadObj (vlax-get-acad-object))
	(setq adoc (vla-get-ActiveDocument acadObj))
  
 
  	(while
	  	(< cntr cmax)
		(setq item (ssname blocks cntr))
	  	(setq block (entget item))
		(setq ename-block (cdr (assoc -1 block)))
	  
		(setq e-type (cdr (assoc 0 block)))

	  
	  	;;; block verification before continueing
		(if
			(= e-type "INSERT")
			(progn
			  
		 	(setq vlaobject (vlax-ename->vla-object ename-block))								;helper function to handle Anonymous names -- set vlaobject to the converted vla name from the entity name stored in ent
			(setq truename(vlax-get-property vlaobject									;set the truename variable to the effective name property
			(if (vlax-property-available-p vlaobject 'effectivename)							;only if the property called effective name exisits inside block 
				'effectivename												;this entire operation of setting truename fails when if statement is false
				'name
				)
			)
			)
			

		  
			(setq vlaobject-block (vla-item (vla-get-blocks adoc) truename))
		  
			(vlax-for AttEnt vlaobject-block                                                   
				(if (=  (vla-get-objectname AttEnt) "AcDbAttributeDefinition")         	; go through the entities pull out attributes
				(progn
				(setq attTagObj-FromTable AttEnt)
				(setq attTag-FromTable (vla-get-tagstring attTagObj-FromTable))
				(setq en ename-block)

				(setq attValue-FromTable (vla-get-textstring AttEnt ))
				(setq sublist (cons  attTagObj-FromTable attValue-FromTable))
				(Setq lst (cons sublist LST))       	; make a list of the defaults


		


				) ; progn
				) ; if
			) ; for
		  
			(setq cntr (+ cntr 1))

			(if
				(/= lst nil)
				(progn
				(setq lst (reverse lst)) ; need reversed

				(setq vlaobject-block2 (vlax-ename->vla-object ename-block))
			  
				; now update object
				(setq atts (vlax-invoke vlaobject-block2  'getattributes)) 	 				; get all the attributes
				(setq z 0)
				(repeat (setq y (length atts ))                     						; repeat for how many attributes

					(setq attTagObj-FromBlock (nth z atts))
					(setq attTag-FromBlock (vla-get-tagstring attTagObj-FromBlock))
				  
					(setq cntr2 0)
					(setq cmax2 (length lst))
					(while
						(< cntr2 cmax2)
						(setq item(nth cntr2 lst))
						(setq attTagObj-FromTable(car item))
						(setq attTag-FromTable (vla-get-tagstring attTagObj-FromTable))
					  
						(setq value(cdr item))
						(if
							(= attTag-FromBlock attTag-FromTable)
							(progn
							(vla-put-textstring attTagObj-FromBlock value)
							(setq cntr2 cmax2)
							)
						)
						(setq cntr2 (+ cntr2 1))
					)
						; go through attributes and update
					(setq z (+ z 1))
				) ; repeat
				)
			)			
			
			
			)
		)
		(Setq lst '())
	  
	)

  


) ;defun