; --- WSCPL Function ---
; Copy all items from certain layers
; Alex Lundin 07-10-2017
(defun c:wscpl
       			(
		   	/
		  	BLOCKS BRIDGES CABLE CABLE2 MATCHLINE NOID OHM PSPACE ROOMID TOTAL EMPTYSET EMPTYSETLAYER EMPTYSETPOINT1 EMPTYSETPOINT2 EMPTYSETXCOORD1 EMPTYSETXCOORD2 EMPTYSETYCOORD1 LAYER
			DRAWINGEXTENTSLAYER DRAWINGEXTENTSSET DRAWINGNAME INSERTIONPOINT SS STARTINGCURRENTLAYER TEXTCONTENT TEXTHEIGHT
		   	)




  	(setq drawingName (vl-filename-base (getvar "dwgname")))
  
  	(setq startingcurrentlayer (getvar "clayer"))								; save current layer to layer variable
    	(setq pspace(getvar "ctab"))										; save current space to pspace variable
  
  	(setq emptysetlayer "emptysetlayer")									; set variables for making the line on the emptysetlayer
  	(setq emptysetxcoord1 0)
  	(setq emptysetycoord1 0)
  	(setq emptysetxcoord2 30)
  	(setq emptysetpoint1 (list emptysetxcoord1 emptysetycoord1))
  	(setq emptysetpoint2 (list emptysetxcoord2 emptysetycoord1))

	(setq drawingextentslayer "drawingextentslayer")							; create layer for extents information
	(command "_.Layer" "_Make" "drawingextentslayer" "_Color" "2" "" "LType" "Continuous" "" "")
	
  
	(setvar "ctab" "Model")											; set current tab to model
  
	(setq blocks(ssget "x" '((8 . "WATTSTOPPER")(410 . "Model"))))						; selection all blocks from WATTSTOPPER LAYER
  	(setq cable(ssget "x" '((8 . "_WS_MSTP")(410 . "Model"))))						; same as above for other layers
  	(setq ohm(ssget "x" '((8 . "_WS_120_OHM")(410 . "Model"))))						; AutoCAD lisp does not use brackets, so anything in brackets below must be replaced
	(setq cable2(ssget "x" '((8 . "_WS_CAT5E")(410 . "Model"))))						; below the [setname] is a varaible name of your choosing, and [layername] is the layer you want to copy to the set.
  	(setq matchline(ssget "x" '((8 . "WATTSTOPPER MATCHLINE")(410 . "Model"))))				; to add new line do this (setq [setname] (ssget "x" '((8 . "[layername]"))))
	(setq roomid(ssget "x" '((8 . "WS-ROOM-ID")(410 . "Model"))))
  	(setq noid(ssget "x" '((8 . "NOID")(410 . "Model"))))
  	(setq bridges(ssget "x" '((8 . "BRIDGES")(410 . "Model"))))
  	(setq total (acet-ss-union (list blocks cable ohm cable2 matchline roomid noid bridges)))		; combine multiple selection sets into one, here are steps to add more:
  														; add the new [setname] to this line (setq total (acet-ss-union (list blocks cable ohm matchline [setname])))

  	(cond
	  	((/= total nil)											; when total is not nil
  											
		
		(setvar "clayer" drawingextentslayer)
		 
		(setq insertionPoint (list 0 0))
		(setq textcontent drawingName)
		(setq layer drawingextentslayer)
		(setq textheight 15.0)
		(insert-text insertionPoint textcontent layer textheight)
		
		
		(setq drawingextentsSet(ssget "x" '((8 . "drawingExtentsLayer")(410 . "Model"))))		; include drawing text before drawing rectangle
		(setq total (acet-ss-union (list blocks cable ohm cable2 matchline roomid noid bridges drawingextentsSet)))

		(setq ss total)
		(get-smallest-rectangle-around-set ss)								; draw rectangle around set
		 
		(setq drawingextentsSet(ssget "x" '((8 . "drawingExtentsLayer")(410 . "Model"))))
		(setq total (acet-ss-union (list blocks cable ohm cable2 matchline roomid noid bridges drawingextentsSet)))
		 
		(command "copybase" "0,0" total "")								; copy from 0,0 basepoint
		(command "erase" drawingextentsSet "")								; erase created rectangle and text
		(setvar "clayer" startingcurrentlayer)
		(command "_.purge" "LA" " drawingextentslayer" "N")
		 
		)
		
		((= total nil)											; when total is nil
		(command "_.Layer" "_Make" "emptysetlayer" "_Color" "30" "" "LType" "Continuous" "" "")		; create emptysetlayer layer

		(setvar "clayer" emptysetlayer)
		 
		(setq insertionPoint (list 0 20))
		(setq textcontent "0 blocks found on WATTSTOPPER layer of this drawing:")
		(setq layer emptysetlayer)
		(setq textheight 15.0)
		(insert-text insertionPoint textcontent layer textheight)
		 
		(setq insertionPoint (list 0 0))
		(setq textcontent drawingName)
		(setq layer emptysetlayer)
		(setq textheight 15.0)
		(insert-text insertionPoint textcontent layer textheight)
		 
		(setq emptyset (ssget "x" '((8 . "emptysetlayer")(410 . "Model"))))				; selection of all items on the emptysetlayer layer in modelspace
		(get-smallest-rectangle-around-set emptyset)

		(setq emptyset (ssget "x" '((8 . "emptysetlayer")(410 . "Model")))) 
		(command "copybase" "0,0" emptyset "")								; copy to clipboard
		(command "erase" emptyset "")									; erase all items on emptysetlayer with the previous command
		(setvar "clayer" startingcurrentlayer)
		(command "_.purge" "LA" "emptysetlayer" "N")							; purge emptysetlayer
		)
	)
  	(setvar "clayer" startingcurrentlayer)									; restore current layer
  	(setvar "ctab" pspace)											; restore current tab
  
)



; --- get-smallest-rectangle-around-set-batch Function ---
; Accepts selection set as arugment
; uses a inline function to create a bounding box around the first entity
; stores the data and continues to next entity
; at the end, the function finds the points that are the farthest apart and uses them for the rectangle
; Alex Lundin 12-16-2016
(defun get-smallest-rectangle-around-set ( ss / LL MAXPT MINPT OLL OUR UR )

  	;;; inline function to get bounding box for a object
	(defun obb (ent / ); = Object's Bounding Box corners 
	(vla-getboundingbox (vlax-ename->vla-object ent) 'minpt 'maxpt)
	(setq
	oLL (vlax-safearray->list minpt); Object's Lower Left
	oUR (vlax-safearray->list maxpt); Object's Upper Right
	); setq
	); defun -- obb
(vl-load-com)
(obb (ssname ss 0))
(setq LL oLL UR oUR); initial overall LL & UR [of first object]
(ssdel (ssname ss 0) ss)
(repeat (sslength ss)
(obb (ssname ss 0))
(setq
LL (mapcar 'min oLL LL); least of each component
UR (mapcar 'max oUR UR); greatest of each component
); setq
(ssdel (ssname ss 0) ss)
); repeat
(command "_.rectangle" "_none" LL "_none" UR)
(princ)
)



; --- insert-text Function ---
; Paste all items from clipboard
; Alex Lundin 12-16-2016
(defun insert-text
       	(insertionPoint textcontent layer textheight / )
    	(entmake
    	(list
      	(cons 0 "MTEXT")         		;; Entity Name
      	(cons 100 "AcDbEntity")  		;; Subclass Marker
      	(cons 410 "Model")       		;; Space
      	(cons 8 layer)         			;; Layer
      	(cons 100 "AcDbMText")   		;; Subclass Marker
      	(cons 10 insertionPoint) 		;; Insertion Point
      	(cons 40 textheight)            	;; Text Height
      	(cons 71 1)              		;; Attachment Point (top-center)
      	(cons 1 textcontent)    		;; Text Content
      	(cons 7 "Arial")			;; text style
      	)
   	)
)

