; --- runtimeupdate ---
; Notes:
; Get most recent WS_Blocks version from the run time updates folder
; Use the client perfered drive
; Call update batch process if version is current
; 07-14-2017
; Alex Lundin 
(defun c:runtimeupdate
       		(
		;arguments
		/
		;local variables
		DRIVENAME textFilePerferedDrive textFilePerferedDriveObject LOCALVERSIONNUMBER perferedDriveVersionNumber textFileLocalVersionNumber textFileLocalVersionNumberObject textFilePerferedDriveVersionNumber textFilePerferedDriveVersionNumberObject
		BATCHPROCESSFULLNAME CLIENTWS_BLOCKSLOCATION HDRIVEWS_BLOCKSLOCATION PDRIVEWS_BLOCKSLOCATION RUNTIMEUPDATEBATCHPROCESSNAME WS_BLOCKSPERFEREDDRIVETXTLOCATION WS_BLOCKSVERSIONTXTLOCATION
		ENHANCEMENTSDOCUMENT ENHANCEMENTSDOCUMENTLOCATION ENHANCEMENTSDOCUMENTLOCATIONOBJECT ENHANCEMENTSFOLDER ENHANCEMENTSFOLDERLOCATION
		)
;;; Arguments:
;;;	1
;;;	2
  	;(setq pDriveWS_BlocksLocation "P:\\1 AutoCAD Main\\Run_Time_Updates\\")
	;(setq hDriveWS_BlocksLocation "H:\\Drawings\\Run_Time_Updates\\")
  	(setq runTimeUpdateFlag 0)
  	(setq pDriveWS_BlocksLocation "H:\\Drawings\\Run_Time_Updates\\")
	(setq hDriveWS_BlocksLocation "H:\\Drawings\\Run_Time_Updates\\")
	(setq clientWS_BlocksLocation "C:\\")
	(setq WS_BlocksPerferedDriveTxtLocation "WS_Blocks\\Custom\\LISP_Custom\\perferednetworkdrive.txt")
  	(setq WS_BlocksVersionTxtLocation "WS_Blocks\\Default\\LISP\\Version_Number.txt")
  	(setq runTimeUpdateBatchProcessName "Start_Call.bat")
  
  
	(setq textFilePerferedDrive (strcat clientWS_BlocksLocation WS_BlocksPerferedDriveTxtLocation))
	(setq textFilePerferedDriveObject (open textFilePerferedDrive "r"))
  	(setq textFileLocalVersionNumber (strcat clientWS_BlocksLocation WS_BlocksVersionTxtLocation))
  	(setq textFileLocalVersionNumberObject (open textFileLocalVersionNumber "r"))

  	(setq enhancementsFolder "WS_Blocks\\Default\\CAD Documentation\\Enchancments Summary")
  	(setq enhancementsFolderLocation (strcat clientWS_BlocksLocation enhancementsFolder))
  	(setq enhancementsDocument "WS_Blocks\\Default\\CAD Documentation\\Enchancments Summary\\Enhancements Summary.docx")
  	(setq enhancementsDocumentLocation (strcat clientWS_BlocksLocation enhancementsDocument))
  
	(if
	  	;if perfered drive is set
		(/= textFilePerferedDriveObject nil)
		(progn
		(setq driveName (read-line textFilePerferedDriveObject))
		)
	  	;otherwise perfered drive is not set, let client set it now
	  	(progn
		(c:getperferednetworkdrive)
		(setq textFilePerferedDrive (strcat clientWS_BlocksLocation WS_BlocksPerferedDriveTxtLocation))
		(setq textFilePerferedDriveObject (open textFilePerferedDrive "r"))
		(setq driveName (read-line textFilePerferedDriveObject))
		)
	)




	(if	
		(= driveName "H")
		(progn
		(setq batchProcessFullName (strcat hDriveWS_BlocksLocation runTimeUpdateBatchProcessName))
		(if
		  	;if text file doesn't exist, set version number to nil
		  	(= textFileLocalVersionNumberObject nil)
		  	(progn
			(setq localVersionNumber nil)
			)
		  	;else text file exists, open and read version number
		  	(progn
			(setq localVersionNumber (read-line textFileLocalVersionNumberObject))
			)
		)
			
		(setq textFilePerferedDriveVersionNumber (strcat hDriveWS_BlocksLocation WS_BlocksVersionTxtLocation))
		(setq textFilePerferedDriveVersionNumberObject (open textFilePerferedDriveVersionNumber "r"))
		
		(if
			(/= textFilePerferedDriveVersionNumberObject  nil)
			(progn
			(setq perferedDriveVersionNumber (read-line textFilePerferedDriveVersionNumberObject))
			
			(if
				;if newer version exists, call windows batch process through windows explorer
				(/= localVersionNumber perferedDriveVersionNumber)
				(progn
				;(startapp "EXPLORER" batchProcessFullName)
				
				(setq runTimeUpdateFlag 1)

				)
			)
			)
		)	

			
	
		)
	)


	(if	
		(= driveName "P")
		(progn
		(setq batchProcessFullName (strcat pDriveWS_BlocksLocation runTimeUpdateBatchProcessName))
		(if
		  	;if text file doesn't exist, set version number to nil
		  	(= textFileLocalVersionNumberObject nil)
		  	(progn
			(setq localVersionNumber nil)
			)
		  	;else text file exists, open and read version number
		  	(progn
			(setq localVersionNumber (read-line textFileLocalVersionNumberObject))
			)
		)
			
		(setq textFilePerferedDriveVersionNumber (strcat pDriveWS_BlocksLocation WS_BlocksVersionTxtLocation))
		(setq textFilePerferedDriveVersionNumberObject (open textFilePerferedDriveVersionNumber "r"))
		
		(if
			(/= textFilePerferedDriveVersionNumberObject  nil)
			(progn
			(setq perferedDriveVersionNumber (read-line textFilePerferedDriveVersionNumberObject))
			
			(if
				;if newer version exists, call windows batch process
				(/= localVersionNumber perferedDriveVersionNumber)
				(progn
				;(startapp "EXPLORER" batchProcessFullName)
				(setq runTimeUpdateFlag 1)
				)
			)
			)
		)	

			
	
		)
	)
  
  	(if
	  	(/= textFilePerferedDriveObject nil)
	  	(progn
		(close textFilePerferedDriveObject)
		)
	)
  	(if
	  	(/= textFileLocalVersionNumberObject nil)
	  	(progn
		(close textFileLocalVersionNumberObject)
		)
	)
  	(if	(/= textFilePerferedDriveVersionNumberObject nil)
	  	(progn
		(close textFilePerferedDriveVersionNumberObject)
		)
	)
  	(if
	  	(= runTimeUpdateFlag 1)
	  	(progn
		(alert 
		(strcat 
		"\nAutoCAD found a newer version of the WS_Blocks folder."
		"\nThis function can apply the update for you."
		"\nAutoCAD will close if you choose to update now."
		"\nIt would be ideal to get the enhancements now."
		"\nThe Enhancment Summary document will open once the update completes."
		"\n"
		"\nYou can continue to use this version if you'd like."
		"\nHowever, this version is no longer supported."
		"\nYou can call this function anytime by typing RUNTIMEUPDATE."
		"\n"
		"\nClick OK"
		"\nType R to continue."
		"\nType E to exit."
		)
		)
		(setq validContinuationChoice 0)
		(while	(= validContinuationChoice 0)
			(setq continue (getstring "Type r to complete the RUNTIMEUPDATE, type e to exit this function."))
			(if
				(or (= continue "e")(= continue "E"))
				(progn
				(quit)
				)
			)		  
			(if
				(or (= continue "r")(= continue "R"))
				(progn
				(setq validContinuationChoice 1)
				
				(alert 
				(strcat 
				"\nAutoCAD is now closing."
				"\nDo not re-open AutoCAD until the update is complete"
				"\n"
				"\nA word document tell you when the update is done"
				"\n"
				"\nClick OK"
				)
				)				
				
				(startapp "EXPLORER" batchProcessFullName)
				(command "_QUIT" "_Y")
				)
			)
		)		

		)
	)

  	(princ)
)

; --- c:getperferednetworkdrive ---
; Notes:
; Prompt client to choose fastest network drive
; Write to text file for use with other functions
; 07-14-2017
; Alex Lundin 
(defun c:getperferednetworkdrive
       		(
		;arguments
		/
		;local variables
		CONTINUE textFilePerferedDrive textFilePerferedDriveObject VALIDCONTINUATIONCHOICE
		)
;;; Arguments:
;;;	1
;;;	2
	(alert 
	(strcat 
	"\nThe WS_Blocks folder now handles updates Automatically."
	"\nSpecify the fastest network drive for your machine."
	"\n"
	"\nClick OK"
	"\nType H - for the Plano Network Drive H"
	"\nType P - for the Birmingham Network Drive P"
	)
	)
	(setq validContinuationChoice 0)
	(while		(= validContinuationChoice 0)
			(setq continue (getstring "Type H - for the Plano Network Drive H, or Type P - for the Birmingham Network Drive P"))
			(if
				(or (= continue "h")(= continue "H"))
				(progn
				(setq validContinuationChoice 1)
				(setq continue "H")
				)
			)
			(if
				(or (= continue "p")(= continue "P"))
				(progn
				(setq validContinuationChoice 1)
				(setq continue "P")
				)
			)			
			
	)
	(setq textFilePerferedDrive (strcat "C:\\WS_Blocks\\Custom\\LISP_Custom\\perferednetworkdrive.txt"))
	(setq textFilePerferedDriveObject (open textFilePerferedDrive "w"))
	(write-line continue textFilePerferedDriveObject)
	(write-line (strcat "This is the perfered drive used for run time update batch process calls.") textFilePerferedDriveObject)
  	(close textFilePerferedDriveObject)
	(alert 
	(strcat 	
	"\n"
	"\nYou can always overwrite your choice by calling this function again."
	"\nAfter this function completes."
	"\nType GETPERFEREDNETWORKDRIVE into the AutoCAD command line."
	"\nClick OK"
	)
	)



	
)