--Alex Lundin
--6/13/2017
------------------------------------------------------------------------------------------------------------------------------------------------------------
--This note appears in all CUI Readmes for managing AutoCAD at Legrand, Wattstopper.
--This article explains the Philosophy of CUI's and AutoCAD more in depth.
--http://forums.augi.com/showthread.php?18955-Notes-on-the-philosophy-of-CUI&highlight=philosophy
------------------------------------------------------------------------------------------------------------------------------------------------------------
CUI Header
--The WS_Blocks folder contains all Custom User Interface files for AutoCAD to operate.
--These files cover a large range of functionality in AutoCAD and can represent any of the following:
--toolbars,mouse and keyboard settings, menus, workspaces and much more.
--CUI's can also serve as blank spaces to combine any of the above items into one file.
--This makes it simple to edit one small peice and incorporate the changes into a larger CUI file.
------------------------------------------------------------------------------------------------------------------------------------------------------------
Information about the files stored in this folder:

C:\WS_Blocks\Custom\CUI_Custom

	1.) WSMAIN.cuix 
	
	This file is the base file for all customers using Legrand, Wattstopper blocks.
	This file serves as a holder for all Default items that the customer needs to have the ability to move around.
	Anytime the customer moves the toolbars around, the orientation changes are saved to this file, WSMAIN.
	Anytime the AutoCAD team adds to the WSTOOLBARS or WSWORKSPACES, the changes are automatically reflected here, WSMAIN.
	WSMAIN pulls toolbars and workspaces from the folder C:\WS_Blocks\Default\CUI.
	
	Contents:
	-C:\WS_Blocks\Default\CUI\Toolbar Files\WSTOOLBARS.cuix
	-C:\WS_Blocks\Default\CUI\Workspace Files\WSWORKSPACES.cuix
	-Started off from the standard Acad.cuix, so all the basic factory functionality is included


	2.)Custom.cuix
	This file is loaded into main with LISP routines and serves as a blank space for the customer to make additions.
	It is stored outside of the file locations that get updates on a weekly basis.
	
	Contents:
	-A blank space for the customer to add any custom toolbars or workspaces
	-nothing else by default
	-open to creative needs of the customer

	1.) WSMAIN2.cuix 
	
	This file is the base file for all Birmingham customers using Legrand, Wattstopper blocks.
	This file serves as a holder for all Default items that the customer needs to have the ability to move around.
	Anytime the customer moves the toolbars around, the orientation changes are saved to this file, WSMAIN2.
	Anytime the AutoCAD team adds to the WSTOOLBARS or WSWORKSPACES, the changes are automatically reflected here, WSMAIN2.
	WSMAIN2 pulls toolbars and workspaces from the folder C:\WS_Blocks\Default\CUI.
	
	Contents:
	-C:\WS_Blocks\Default\CUI\Toolbar Files\WSTOOLBARS.cuix
	-C:\WS_Blocks\Default\CUI\Workspace Files\WSWORKSPACES.cuix
	-Started off from the standard Acad.cuix, so all the basic factory functionality is included