;;; --- driver---
;;; Notes:
;;; Demonstrates multiple ways of creating selection sets
;;; Alex Lundin 08-25-2017
(defun 	c:driver ( / CURRENTTAB LAYER P1 P2 P3 POINTLIST SELECTIONSET1 SELECTIONSETALL SELECTIONSETCROSSINGPOLYGON)

	(vl-load-com)
  
	(setq p1 (list -500.0 0.0))	 
	(setq p2 (list 500.0 0.0))
  	(setq p3 (list 0.0 500.0))
  	(setq pointList (list p1 p2 p3))
  
  	;selection sets are created with ssget
  	;filters can be applied as well, the "x" filter gets all objects
  	
  	;following the filter, is the list of dxfcodes and dxfcode values to narrow the selection further
  	;the dxfcode values can be a hardcoded list using the ' operator
	(setq selectionSetAll(ssget "x" '((0 . "LWPOLYLINE")(8 . "MSTP"))))						;create selectionSet1 from all objects that LWPOLYLINE objects on the MSTP layer

  
  	;the "cp" filter gets a crossing polygon using a list of supplied points
  	;it's important that the polygon doesn't intersect itself
  	(setq selectionSetCrossingPolygon(ssget "_CP" pointList '((0 . "INSERT")(8 . "WATTSTOPPER"))))


  	(setq layer "WATTSTOPPER")
  	(setq currentTab "Model")
  
  	;the dxfcodes values can also be sent in with variables
	(setq selectionSet1 (ssget "x" (list (cons 0 "INSERT") (cons 8 layer)(cons 410 currentTab))))			;create selection set with variables

  	;passes control to user to select items, they are stored to ss1
  	(setq ss1 (ssget))

	;example of combining selection sets with express tools
	(setq selectionSetCombination (acet-ss-union (list selectionSetAll selectionSetCrossingPolygon selectionSet1)) 
	
  	(entity-selection-set-loop selectionSet1)
)

; --- entity-selection-set-loop---									
; Notes:												
;1													
; an association list, is a list of items, each with a two element structure and a dot in between	
; (label3.info10)  (label1.info5)  (label2.info1)							
; in AutoCAD, dxf codes follow this convention of association lists					
; the example above shows that the organization is not necessarily by number				
; the entire list can be searched by label, or the first element, with the assoc function		
;													
;2													
; cdr is a language primitive which returns everything but the first element in a list			
; for a two item list, this would return just the second item						
; in the above example that would be the info element from any designated item				
;													
; Alex Lundin 06-14-2017										
(defun entity-selection-set-loop (selectionSet / CMAX CNTR DXF-1 DXF2 DXF8 ENAME ENT ENTDXF ENTITYNAMELIST)
;;; Arguments:
;;;	selectionSet
  
	(setq cntr 0)													;initialize counter to 0 for while loop
  	(setq cmax (sslength selectionSet))										;set cmax to length of items in selection
	(while (< cntr cmax)												;set loop to continue while the counter varaible cntr is less than cmax
		(setq ent(ssname selectionSet cntr))									;set variable ent to the name of the item from the selection that matches the current counter value
		(setq entDXF(entget ent))										;set the varaible entDxf to the list of entities from the en varaible

	  	;you can use the method here to find any dxfcode
	  	(setq dxf-1 (cdr (assoc -1 entDXF )))									;set dxf-1 to the second element of the item that has -1 as it's first element, this is the entity name
		(setq dxf2 (cdr (assoc 2 entDXF )))									;set dxf8 to the second element of the item that has 8 as it's first element, this is the name, or block name
		(setq dxf8 (cdr (assoc 8 entDXF )))									;set dxf8 to the second element of the item that has 8 as it's first element, this is the layer
		
	  
	  	(setq entityNameList (cons eName entityNameList))							;creates a list of every entity name in the loop by adding eName to the beginning of entityNameList

		;ENTITY-TO-BLOCK-TRUENAME sub-function call
	  	;AutoCAD is able to change a blocks name for some other functionality used with dynamic blocks
	  	;this makes finding blocks based by dxf challanging
	  	;this truename filter can use some vlax functions to get the effective name, aka truename
		(setq trueName (ENTITY-TO-BLOCK-TRUENAME ent entDXF))

	  
	  	;pause for user at each iteration
	  	;useful if user has to verify some change made inside the loop
	  	;loop continues to next iteration when user presses enter, then pauses again and so on
	  	;uncomment this line to add it
	 
;;;	  	(command "\\")
	  
		(setq cntr(+ cntr 1))											;once while loop reaches this point, increment counter varaible by one			
	)
  	(setq entityNameList (reverse entityNameList))									;reverse list because cons builds list backwards
  	(princ)														;clean return, the last line of a function call is always returned to caller incase it is expecting a return result
  															;princ ensures no value is sent back or sent to command line
)





