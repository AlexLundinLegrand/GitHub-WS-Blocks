;;; --- wsquotetoolbom ---
;;; Notes:
;;; Alex Lundin 07-24-2017
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
				     (setq listTotalLength (length (setq nameList					;set nameList to new list with all of the items that matched the first element removed
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
		      
		(setq string (strcat "<MarkupSummary Version=\"1.0\" Document=\"" argument-drawingName "_Bill_of_Material.pdf\">"))
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

		;mask all incompatible AutoCAD blocks
		;this ensures no error during quote tool import
  		;important to note that these parts are dropped entirely from the extraction
  		;the switches go through the formatter first so they aren't blocked by this if they are missing the -W color identifier
	(if
	  	(AND
		(/= "ADF-120-277" argument-blockName)
		(/= "ALM5-010" argument-blockName)
		(/= "ALM5-DLM" argument-blockName)
		(/= "ALM5-DMX" argument-blockName)
		(/= "AW11-900" argument-blockName)
		(/= "AW15-900" argument-blockName)
		(/= "BCI" argument-blockName)
		(/= "BLM1-010" argument-blockName)
		(/= "BLM1-DLM" argument-blockName)
		(/= "BLM1-DMX" argument-blockName)
		(/= "BLM2-010" argument-blockName)
		(/= "BLM2-DLM" argument-blockName)
		(/= "BLM2-DMX" argument-blockName)
		(/= "BLM3-010" argument-blockName)
		(/= "BLM3-DLM" argument-blockName)
		(/= "BLM3-DMX" argument-blockName)
		(/= "CD" argument-blockName)
		(/= "CH" argument-blockName)
		(/= "CS" argument-blockName)
		(/= "CU" argument-blockName)
		(/= "DCD-270" argument-blockName)
		(/= "DM-100" argument-blockName)
		(/= "DRLV1" argument-blockName)
		(/= "EW-200" argument-blockName)
		(/= "EW-205" argument-blockName)
		(/= "FS-305-L2W" argument-blockName)
		(/= "FS-305-L3W" argument-blockName)
		(/= "FS-305-L4W" argument-blockName)
		(/= "FS-355-L6" argument-blockName)
		(/= "FS-PP-V2" argument-blockName)
		(/= "FSP-201-B" argument-blockName)
		(/= "FSP-201-B-D" argument-blockName)
		(/= "FSP-201-B-S" argument-blockName)
		(/= "FSP-211-B" argument-blockName)
		(/= "FSP-211-B-D" argument-blockName)
		(/= "FSP-211-B-S" argument-blockName)
		(/= "FSP-221" argument-blockName)
		(/= "FSP-221-B" argument-blockName)
		(/= "FSP-221-B-D" argument-blockName)
		(/= "FSP-221-B-S" argument-blockName)
		(/= "GENERIC-CIRCLE" argument-blockName)
		(/= "GENERIC-SQUARE" argument-blockName)
		(/= "GENERIC-SUN" argument-blockName)
		(/= "GENERIC-SWITCH" argument-blockName)
		(/= "GENERIC-SWITCH-1" argument-blockName)
		(/= "GENERIC-SWITCH-2" argument-blockName)
		(/= "GENERIC-X" argument-blockName)
		(/= "HB-300-C" argument-blockName)
		(/= "HB-300-L1" argument-blockName)
		(/= "HB-300-L1M" argument-blockName)
		(/= "HB-300-L2" argument-blockName)
		(/= "HB-300-L3" argument-blockName)
		(/= "HB-300-L3W" argument-blockName)
		(/= "HB-300-L4" argument-blockName)
		(/= "HB-300-L4W" argument-blockName)
		(/= "HB-330-C" argument-blockName)
		(/= "HB-340-C" argument-blockName)
		(/= "HB-350-C" argument-blockName)
		(/= "HB-350-L1" argument-blockName)
		(/= "HB-350-L1M" argument-blockName)
		(/= "HB-350-L2" argument-blockName)
		(/= "HB-350-L3" argument-blockName)
		(/= "HB-350-L3W" argument-blockName)
		(/= "HB-350-L4" argument-blockName)
		(/= "HB-350-L4W" argument-blockName)
		(/= "HBP" argument-blockName)
		(/= "HBP-112" argument-blockName)
		(/= "HD-1103" argument-blockName)
		(/= "HD-703" argument-blockName)
		(/= "HD-703 1103" argument-blockName)
		(/= "HDA-1103" argument-blockName)
		(/= "HDA-703" argument-blockName)
		(/= "HDCL-453" argument-blockName)
		(/= "HDFC-1A" argument-blockName)
		(/= "HDFM-8A" argument-blockName)
		(/= "HDLS1SS" argument-blockName)
		(/= "HDLS2SS" argument-blockName)
		(/= "HDLS4SS" argument-blockName)
		(/= "HDLS8SS" argument-blockName)
		(/= "HDMLV-703 1103" argument-blockName)
		(/= "HG903RD-RSP" argument-blockName)
		(/= "KEY-SWITCH" argument-blockName)
		(/= "L1S" argument-blockName)
		(/= "L3S" argument-blockName)
		(/= "L5S" argument-blockName)
		(/= "L9S" argument-blockName)
		(/= "LI-ARP" argument-blockName)
		(/= "LI-LP" argument-blockName)
		(/= "LIC24" argument-blockName)
		(/= "LIC48" argument-blockName)
		(/= "LIC8" argument-blockName)
		(/= "LICA24" argument-blockName)
		(/= "LICA48" argument-blockName)
		(/= "LICA8" argument-blockName)
		(/= "LILM24" argument-blockName)
		(/= "LILM48" argument-blockName)
		(/= "LILM8" argument-blockName)
		(/= "LMBC-600" argument-blockName)
		(/= "LMBR-600" argument-blockName)
		(/= "LMCP12" argument-blockName)
		(/= "LMCP24" argument-blockName)
		(/= "LMCP48" argument-blockName)
		(/= "LMCP8" argument-blockName)
		(/= "LMDI-100" argument-blockName)
		(/= "LMIN-104" argument-blockName)
		(/= "LMNC" argument-blockName)
		(/= "LMOR-102" argument-blockName)
		(/= "LMRC-211-347V" argument-blockName)
		(/= "LMRC-212-347V" argument-blockName)
		(/= "LMRC-213-347V" argument-blockName)
		(/= "LMSM-600" argument-blockName)
		(/= "LVS-1K" argument-blockName)
		(/= "MCD26" argument-blockName)
		(/= "MCD267" argument-blockName)
		(/= "MCD68" argument-blockName)
		(/= "MCF8" argument-blockName)
		(/= "MDR24" argument-blockName)
		(/= "MDR246" argument-blockName)
		(/= "MDR247" argument-blockName)
		(/= "MDS246" argument-blockName)
		(/= "MDS248" argument-blockName)
		(/= "MDS266" argument-blockName)
		(/= "MDS268" argument-blockName)
		(/= "MDS269" argument-blockName)
		(/= "MNM2" argument-blockName)
		(/= "MNM4" argument-blockName)
		(/= "MNM5" argument-blockName)
		(/= "MR232" argument-blockName)
		(/= "MRD2" argument-blockName)
		(/= "MRD3" argument-blockName)
		(/= "MRD4" argument-blockName)
		(/= "MRD5" argument-blockName)
		(/= "MRD6" argument-blockName)
		(/= "MRD8" argument-blockName)
		(/= "MRD9" argument-blockName)
		(/= "MRDS10" argument-blockName)
		(/= "MRH5G" argument-blockName)
		(/= "MRH6G" argument-blockName)
		(/= "MRHC3" argument-blockName)
		(/= "MRPP1" argument-blockName)
		(/= "MRR2G" argument-blockName)
		(/= "MRRC3" argument-blockName)
		(/= "MRRC4" argument-blockName)
		(/= "MSC-100" argument-blockName)
		(/= "NWTL-111" argument-blockName)
		(/= "P-NUT" argument-blockName)
		(/= "P8AX09-N" argument-blockName)
		(/= "PHASE-TO-PHASE" argument-blockName)
		(/= "PS-CD4FBL" argument-blockName)
		(/= "PS-CD700" argument-blockName)
		(/= "PS-H1103P" argument-blockName)
		(/= "PS-H4FBL3P" argument-blockName)
		(/= "PS-H703P" argument-blockName)
		(/= "PS-H703PTU" argument-blockName)
		(/= "PS-H703PTUW" argument-blockName)
		(/= "PS-HCL453P" argument-blockName)
		(/= "PS-HDH163P" argument-blockName)
		(/= "PS-HFB83P" argument-blockName)
		(/= "PW-103" argument-blockName)
		(/= "RACCESS" argument-blockName)
		(/= "RD-250" argument-blockName)
		(/= "RH" argument-blockName)
		(/= "RH-250" argument-blockName)
		(/= "RS" argument-blockName)
		(/= "RS-232" argument-blockName)
		(/= "RT" argument-blockName)
		(/= "TD-603" argument-blockName)
		(/= "TDA-603" argument-blockName)
		(/= "TDFC-1A" argument-blockName)
		(/= "TDR" argument-blockName)
		(/= "V-CIS10-DIN" argument-blockName)
		(/= "V-DMX-DALI-GW" argument-blockName)
		(/= "V-EASYTOUCH-II-1" argument-blockName)
		(/= "V-EASYTOUCH-II-2" argument-blockName)
		(/= "V-EASYTOUCH-II-3" argument-blockName)
		(/= "V-EASYTOUCH-II-4" argument-blockName)
		(/= "V-EASYTOUCH-II-5" argument-blockName)
		(/= "V-EM-LIGHTSENSOR" argument-blockName)
		(/= "V-EQ40TB-TI" argument-blockName)
		(/= "V-EQ41TB-TI" argument-blockName)
		(/= "V-EQ73TB-TI" argument-blockName)
		(/= "V-FANMOD" argument-blockName)
		(/= "V-IC-36" argument-blockName)
		(/= "V-IC-DIN-II-LITE" argument-blockName)
		(/= "V-IC-DIN-II-LITE-RF" argument-blockName)
		(/= "V-IRX-II" argument-blockName)
		(/= "V-LCAP" argument-blockName)
		(/= "V-LCAP32L" argument-blockName)
		(/= "V-LCAP32M" argument-blockName)
		(/= "V-LCAP32S" argument-blockName)
		(/= "V-LCAP44A" argument-blockName)
		(/= "V-LCAP44H" argument-blockName)
		(/= "V-LCAP44HS" argument-blockName)
		(/= "V-LCAP44M" argument-blockName)
		(/= "V-LCAP44S" argument-blockName)
		(/= "V-LVOS" argument-blockName)
		(/= "V-LVRS8-DIN" argument-blockName)
		(/= "V-MDR8CW301" argument-blockName)
		(/= "V-RS8-L-DIN" argument-blockName)
		(/= "V-SDM12-EM" argument-blockName)
		(/= "V-SLDS4-DIN" argument-blockName)
		(/= "V-STIDER121" argument-blockName)
		(/= "V-STPERW101" argument-blockName)
		(/= "V-STPERW201" argument-blockName)
		(/= "V-STPSRW101" argument-blockName)
		(/= "V-STPSRW201" argument-blockName)
		(/= "V-UDM08-EM" argument-blockName)
		(/= "V-VA-EPC-DFS-120V" argument-blockName)
		(/= "V-VA-EPC-DFS-277V" argument-blockName)
		(/= "V-VA-RRU-1-120V" argument-blockName)
		(/= "V-VA-RRU-1-277V" argument-blockName)
		(/= "WBT-900-IP" argument-blockName)
		(/= "WD-170" argument-blockName)
		(/= "WRC-20-2" argument-blockName)
		(/= "WS-301" argument-blockName)
		;extra blocks
		(/= "REV DELTA" argument-blockName)
		(/= "120ohm" argument-blockName)
		(/= "TW_Cable_ID" argument-blockName)
		(/= "TW_Cable_Splice" argument-blockName)
		(/= "TW_MTSP" argument-blockName)
		(/= "TW_MTSP_S" argument-blockName)
		(/= "TW_REV" argument-blockName)
		(/= "WS_Layout_Cable" argument-blockName)
		(/= "WS_Layout_Cable_Splice" argument-blockName)
		(/= "WS_Layout_Cable_Text" argument-blockName)
		(/= "WS_Layout_Matchline" argument-blockName)
		(/= "WS_ROOMID" argument-blockName)
		;residual errors
		(/= " '233912'" argument-blockName)
		(/= " 'N' " argument-blockName)
		)
	  	(progn
		
  
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
	)
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

  	;;; if product is a regular switch, add -W
	(if
		(OR
		  
		;ANALOG not supported even with the -W tag added
		;(= "PW-103" argument-product)
		;(= "RS-232" argument-product)
		;(= "LVS-1K" argument-product)
		  
		;ANALOG
		(= "AS-100" argument-product) 
		(= "CD-250" argument-product) 
		(= "CH-250" argument-product) 
		(= "CS-350-N" argument-product) 
		(= "CS-50" argument-product) 
		(= "CU-250" argument-product) 
		(= "DCC2" argument-product) 
		(= "DCC7" argument-product) 
		(= "DCD267" argument-product) 
		(= "DCD68" argument-product) 
		(= "DCLV1" argument-product) 
		(= "DRD2" argument-product) 
		(= "DRD3" argument-product) 
		(= "DRD4" argument-product) 
		(= "DRD40" argument-product) 
		(= "DRD5" argument-product) 
		(= "DRD6" argument-product) 
		(= "DRD8" argument-product) 
		(= "DRD9" argument-product) 
		(= "DSW-100" argument-product) 
		(= "DSW-200" argument-product) 
		(= "DSW-301" argument-product) 
		(= "DSW-301-347" argument-product) 
		(= "DSW-302" argument-product) 
		(= "DSW-302-347" argument-product) 
		(= "DW-100" argument-product) 
		(= "DW-100-24" argument-product) 
		(= "DW-100-347" argument-product) 
		(= "DW-103" argument-product) 
		(= "DW-200" argument-product) 
		(= "DW-203" argument-product) 
		(= "DW-311" argument-product) 
		(= "EOHR-101" argument-product) 
		(= "EOHR-102" argument-product) 
		(= "EORS-101" argument-product) 
		(= "EORS-102" argument-product) 
		(= "EOSW-101" argument-product) 
		(= "EOSW-102" argument-product) 
		(= "EOSW-111" argument-product) 
		(= "EOSW-112" argument-product) 
		(= "HS-100" argument-product) 
		(= "HS-150" argument-product)
		(= "LVS-1" argument-product) 	
		(= "LVSW-101" argument-product) 
		(= "LVSW-102" argument-product) 
		(= "LVSW-103" argument-product) 
		(= "LVSW-104" argument-product) 
		(= "LVSW-108" argument-product) 
		(= "MRP6" argument-product) 
		(= "MRP7" argument-product) 
		(= "PW-100" argument-product) 
		(= "PW-100-24" argument-product) 
		(= "PW-100D" argument-product) 
		(= "PW-101" argument-product) 
		(= "PW-200" argument-product) 
		(= "PW-201" argument-product) 
		(= "PW-203" argument-product) 
		(= "PW-301" argument-product) 
		(= "PW-301-347" argument-product) 
		(= "PW-302" argument-product) 
		(= "PW-302-347" argument-product) 
		(= "PW-311" argument-product) 
		(= "RS-150BA" argument-product) 
		(= "RS-250" argument-product) 
		(= "RS-150BA" argument-product) 
		(= "RS-250" argument-product) 
		(= "RT-200" argument-product) 
		(= "TS-400" argument-product) 
		(= "TS-400-24" argument-product) 
		(= "UW-100" argument-product) 
		(= "UW-100-24" argument-product) 
		(= "UW-200" argument-product) 
		(= "WS-250" argument-product) 
		(= "WS-301-347" argument-product)

		;DLM not supported even with -W
		;(= "LMCT-100" argument-product)
		
		;DLM
		(= "LMDM-101" argument-product) 
		(= "LMDW-101" argument-product) 
		(= "LMDW-102" argument-product) 
		(= "LMSW-101" argument-product) 
		(= "LMSW-102" argument-product) 
		(= "LMSW-103" argument-product) 
		(= "LMSW-104" argument-product) 
		(= "LMSW-105" argument-product) 
		(= "LMSW-105-CCT" argument-product) 
		(= "LMSW-108" argument-product) 
		(= "LMTS-101-CCT" argument-product)
		(= "LMPW-101" argument-product) 
		(= "LMPW-102" argument-product)
		(= "LMPS-104" argument-product)
		)
	  	(progn
		(setq formatedProductString (strcat argument-product "-W"))
		)
	)


  	;;; if product is a dataline switch, add 7k
	;;; 2 ivory, 2k is key ivory, 7 is white, 7k is white with key"
  	(if
		(OR
		(= "HDLS1SS" argument-product) 
		(= "HDLS2SS" argument-product) 
		(= "HDLS4SS" argument-product) 
		(= "HDLS8SS" argument-product) 
		)
	  	(progn
		(setq skip "true")
		;(setq formatedProductString (strcat argument-product "7k"))
		)
	)
  	(setq returnlist (list formatedProductString))
)
