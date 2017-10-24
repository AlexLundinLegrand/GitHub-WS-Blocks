 ;                                                    ;
 ;                                                    ;
 ;                                                    ;
 ;                 WattStopper Print                  ;
 ;                                                    ;
 ;                                                    ;
 ;                                                    ;

(defun C:wscanon (/ tblss cnt laynmlst laynm laynmlst itm)
    (setvar "PLOTTRANSPARENCYOVERRIDE" 2)
  (setq tblss (ssget "x" (list (cons 0 "insert") (cons 8 "WS_TITLEBLOCK"))))
  (setq cnt 0)
  (setq laynmlst (list))
  (while (< cnt (sslength tblss))
    (setq laynm (cdr (assoc 410 (entget (ssname tblss cnt)))))
    (setq cnt (1+ cnt))
    (setq laynmlst (append laynmlst (list laynm)))
  ) ;_ end of while
  (foreach itm laynmlst
    (command "-plot"		"Y"		   itm		      "\\\\wsplfp1\\Canon iR-ADV C5030/5035 PS3"		       "11x17"		  "Inches"	     "Landscape"
	     "No"		"Extents"	   "Fit"	      "Center"		 "Yes"		    "Watt Stopper Lite 160.ctb"		  "Yes"		     "No"
	     "No"		"No"		   ""		      "No"		 "Yes"
	    ) ;_ end of command

  ) ;_ end of foreach
    (setvar "PLOTTRANSPARENCYOVERRIDE" 1)
  (princ)
) ;_ end of defun


(defun C:hp500 ()
  (foreach tn (layoutlist)
    (if	(/= itm "Model")
      (command "-plot"		"Y"		 ""		  "\\\\Wsplfp1\\HP DesignJet 500 24 by HP"	     "Arch D "	      "Inches"	       "Landscape"	"No"
	       "Extents"	"Fit"		 "Center"	  "Yes"		   "Monochrome.ctb" "Yes"	     "No"	      "No"	       "No"		""
	       "No"		"Yes"
	      ) ;_ end of command

    ) ;_ end of if
  ) ;_ end of foreach
  (princ)
) ;_ end of defun

(defun C:hp5200	()
  (foreach tn (layoutlist)
    (if	(/= itm "Model")
      (command "-plot"		 "Y"		   ""		     "\\\\wsplfp1\\HP LaserJet 5200 PS - 8x11"		   "11x17"	     "Inches"	       "Landscape"	 "No"
	       "Extents"	 "Fit"		   "Center"	     "Yes"	       "Monochrome.ctb"	 "Yes"		   "No"		     "No"	       "No"		 ""
	       "No"		 "Yes"
	      ) ;_ end of command

    ) ;_ end of if
  ) ;_ end of foreach
  (princ)
) ;_ end of defun

(defun C:hp5000	()

      (command "-plot"		 "Y"		   ""		     "\\\\Wsplfp1\\HP LaserJet 5000 Series PS"		   "Letter"	     "Inches"	       "Landscape"	 "No"
	       "Extents"	 "Fit"		   "Center"	     "Yes"	       "Monochrome.ctb"	 "Yes"		   "No"		     "No"	       "No"		 ""
	       "No"		 "Yes"
	      ) ;_ end of command
        (command "_qsave"
	      ) ;_ end of command
  	(command "_close"
	      ) ;_ end of command
  (princ)
) ;_ end of defun

(defun c:pdf (/ tblss cnt laynmlst laynm laynmlst itm)
  (foreach tn (layoutlist)
    (if	(/= itm "Model")
      (command "-plot" "Y" tn "DWG To PDF.pc3" "ARCH D (36.00 x 24.00 Inches)" "Inches"	"Landscape" "No" "Extents" "Fit" "Center" "Yes"	"Monochrome.ctb" "Yes" "No" "No" "No" "" "No" "Yes") ;_ end of command
 ;_ end of command
    ) ;_ end of if
  ) ;_ end of foreach
  (princ)
) ;_ end of defun


(defun C:ws500 (/ tblss cnt laynmlst laynm laynmlst itm)
  (setq tblss (ssget "x" (list (cons 0 "insert") (cons 8 "WS_TITLEBLOCK"))))
  (setq cnt 0)
  (setq laynmlst (list))
  (while (< cnt (sslength tblss))
    (setq laynm (cdr (assoc 410 (entget (ssname tblss cnt)))))
    (setq cnt (1+ cnt))
    (setq laynmlst (append laynmlst (list laynm)))
  ) ;_ end of while
  (foreach itm laynmlst
    (command "-plot"		"Y"		   itm		      "\\\\Wsplfp1\\HP DesignJet 500 24 by HP"		       "Arch D "		  "Inches"	     "Landscape"
	     "No"		"Extents"	   "Fit"	      "Center"		 "Yes"		    "Watt Stopper Lite 160.ctb"		  "Yes"		     "No"
	     "No"		"No"		   ""		      "No"		 "Yes"
	    ) ;_ end of command

  ) ;_ end of foreach
  (princ)
) ;_ end of defun


(defun C:wspdf (/ tblss cnt laynmlst laynm laynmlst itm)
      (setvar "PLOTTRANSPARENCYOVERRIDE" 2)
  (setq tblss (ssget "x" (list (cons 0 "insert") (cons 8 "WS_TITLEBLOCK"))))
  (setq cnt 0)
  (setq laynmlst (list))
  (while (< cnt (sslength tblss))
    (setq laynm (cdr (assoc 410 (entget (ssname tblss cnt)))))
    (setq cnt (1+ cnt))
    (setq laynmlst (append laynmlst (list laynm)))
  ) ;_ end of while
  (foreach itm laynmlst
    (command "-plot" "Y" itm "DWG To PDF.pc3" "ARCH D (36.00 x 24.00 Inches)" "Inches" "Landscape" "No"	"Extents" "Fit"	"Center" "Yes" "Watt Stopper Lite 160.ctb" "Yes" "No" "No" "No"	"" "No"	"Yes") ;_ end of command
 ;_ end of command
 ;_ end of command
  ) ;_ end of foreach
      (setvar "PLOTTRANSPARENCYOVERRIDE" 1)
  (princ)
) ;_ end of defun

(defun C:wspdfal (/ tblss cnt laynmlst laynm laynmlst itm)
      (setvar "PLOTTRANSPARENCYOVERRIDE" 2)
  (setq tblss (ssget "x" (list (cons 0 "insert") (cons 8 "WS_TITLEBLOCK"))))
  (setq cnt 0)
  (setq laynmlst (list))
  (while (< cnt (sslength tblss))
    (setq laynm (cdr (assoc 410 (entget (ssname tblss cnt)))))
    (setq cnt (1+ cnt))
    (setq laynmlst (append laynmlst (list laynm)))
  ) ;_ end of while
  (foreach itm laynmlst
    (command "-plot" "Y" itm "DWG To PDF.pc3" "ANSI A (8.50 x 11.00 Inches)" "Inches" "Landscape" "No" "Extents" "Fit" "Center"	"Yes" "Watt Stopper Lite 160.ctb" "Yes"	"No" "No" "No" "" "No" "Yes") ;_ end of command
  ) ;_ end of foreach
      (setvar "PLOTTRANSPARENCYOVERRIDE" 1)
  (princ)
) ;_ end of defun

(defun C:wspdfap (/ tblss cnt laynmlst laynm laynmlst itm)
      (setvar "PLOTTRANSPARENCYOVERRIDE" 2)
  (setq tblss (ssget "x" (list (cons 0 "insert") (cons 8 "WS_TITLEBLOCK"))))
  (setq cnt 0)
  (setq laynmlst (list))
  (while (< cnt (sslength tblss))
    (setq laynm (cdr (assoc 410 (entget (ssname tblss cnt)))))
    (setq cnt (1+ cnt))
    (setq laynmlst (append laynmlst (list laynm)))
  ) ;_ end of while
  (foreach itm laynmlst
    (command "-plot" "Y" itm "DWG To PDF.pc3" "ANSI A (8.50 x 11.00 Inches)" "Inches" "Portrait" "No" "Extents"	"Fit" "Center" "Yes" "Watt Stopper Lite 160.ctb" "Yes" "No" "No" "No" "" "No" "Yes") ;_ end of command
  ) ;_ end of foreach
    (setvar "PLOTTRANSPARENCYOVERRIDE" 1)
  (princ)
) ;_ end of defun



(defun C:ws500C	(/ tblss cnt laynmlst laynm laynmlst itm)
      (setvar "PLOTTRANSPARENCYOVERRIDE" 2)
  (setq tblss (ssget "x" (list (cons 0 "insert") (cons 8 "WS_TITLEBLOCK"))))
  (setq cnt 0)
  (setq laynmlst (list))
  (while (< cnt (sslength tblss))
    (setq laynm (cdr (assoc 410 (entget (ssname tblss cnt)))))
    (setq cnt (1+ cnt))
    (setq laynmlst (append laynmlst (list laynm)))
  ) ;_ end of while
  (foreach itm laynmlst
    (command "-plot"		 "Y"		     itm		 "\\\\Wsplfp1\\HP DesignJet 500 24 by HP"		     "Arch C  (landscape)"		     "Inches"
	     "Landscape"	 "No"		     "Extents"		 "Fit"		     "Center"		 "Yes"		     "Watt Stopper Lite 160.ctb"	     "Yes"
	     "No"		 "No"		     "No"		 ""		     "No"		 "Yes"
	    ) ;_ end of command

  ) ;_ end of foreach
    (setvar "PLOTTRANSPARENCYOVERRIDE" 1)
  (princ)
) ;_ end of defun


(defun C:ws5200	(/ tblss cnt laynmlst laynm laynmlst itm)
      (setvar "PLOTTRANSPARENCYOVERRIDE" 2)
  (setq tblss (ssget "x" (list (cons 0 "insert") (cons 8 "WS_TITLEBLOCK"))))
  (setq cnt 0)
  (setq laynmlst (list))
  (while (< cnt (sslength tblss))
    (setq laynm (cdr (assoc 410 (entget (ssname tblss cnt)))))
    (setq cnt (1+ cnt))
    (setq laynmlst (append laynmlst (list laynm)))
  ) ;_ end of while
  (foreach itm laynmlst
    (command "-plot"		"Y"		   itm		      "\\\\wsplfp1\\HP LaserJet 5200 PS - 8x11"	       "11x17"		  "Inches"	     "Landscape"
	     "No"		"Extents"	   "Fit"	      "Center"		 "Yes"		    "Watt Stopper Lite 160.ctb"		  "Yes"		     "No"
	     "No"		"No"		   ""		      "No"		 "Yes"
	    ) ;_ end of command
  ) ;_ end of foreach
      (setvar "PLOTTRANSPARENCYOVERRIDE" 1)
  (princ)
) ;_ end of defun


(defun C:ws5200AL (/ tblss cnt laynmlst laynm laynmlst itm)
      (setvar "PLOTTRANSPARENCYOVERRIDE" 2)
  (setq tblss (ssget "x" (list (cons 0 "insert") (cons 8 "WS_TITLEBLOCK"))))
  (setq cnt 0)
  (setq laynmlst (list))
  (while (< cnt (sslength tblss))
    (setq laynm (cdr (assoc 410 (entget (ssname tblss cnt)))))
    (setq cnt (1+ cnt))
    (setq laynmlst (append laynmlst (list laynm)))
  ) ;_ end of while
  (foreach itm laynmlst
    (command "-plot"		"Y"		   itm		      "\\\\wsplfp1\\HP LaserJet 5200 PS - 8x11"		       "Letter"		  "Inches"	     "Landscape"
	     "No"		"Extents"	   "Fit"	      "Center"		 "Yes"		    "Watt Stopper Lite 160.ctb"		  "Yes"		     "No"
	     "No"		"No"		   ""		      "No"		 "Yes"
	    ) ;_ end of command

  ) ;_ end of foreach
      (setvar "PLOTTRANSPARENCYOVERRIDE" 1)
  (princ)
) ;_ end of defun

(defun C:ws5200AP (/ tblss cnt laynmlst laynm laynmlst itm)
      (setvar "PLOTTRANSPARENCYOVERRIDE" 2)
  (setq tblss (ssget "x" (list (cons 0 "insert") (cons 8 "WS_TITLEBLOCK"))))
  (setq cnt 0)
  (setq laynmlst (list))
  (while (< cnt (sslength tblss))
    (setq laynm (cdr (assoc 410 (entget (ssname tblss cnt)))))
    (setq cnt (1+ cnt))
    (setq laynmlst (append laynmlst (list laynm)))
  ) ;_ end of while
  (foreach itm laynmlst
    (command "-plot"		"Y"		   itm		      "\\\\wsplfp1\\HP LaserJet 5200 PS - 8x11"		       "Letter"		  "Inches"	     "Portrait"
	     "No"		"Extents"	   "Fit"	      "Center"		 "Yes"		    "Watt Stopper Lite 160.ctb"		  "Yes"		     "No"
	     "No"		"No"		   ""		      "No"		 "Yes"
	    ) ;_ end of command

  ) ;_ end of foreach
      (setvar "PLOTTRANSPARENCYOVERRIDE" 1)
  (princ)
) ;_ end of defun


(defun C:Bham8150 (/ tblss cnt laynmlst laynm laynmlst itm)
  (setq tblss (ssget "x" (list (cons 0 "insert") (cons 8 "WS_TITLEBLOCK"))))
  (setq cnt 0)
  (setq laynmlst (list))
  (while (< cnt (sslength tblss))
    (setq laynm (cdr (assoc 410 (entget (ssname tblss cnt)))))
    (setq cnt (1+ cnt))
    (setq laynmlst (append laynmlst (list laynm)))
  ) ;_ end of while
  (foreach itm laynmlst
    (command "-plot"		  "Y"		       itm		    "\\\\datusbrm001\\HP LaserJet 8150 Series PCLxx"		   "11x17"		"Inches"
	     "Landscape"	  "No"		       "Extents"	    "Fit"		 "Center"	      "Yes"		   "Watt Stopper Lite 160.ctb"
	     "Yes"		  "No"		       "No"		    "No"		 ""		      "No"		   "Yes"
	    ) ;_ end of command
  ) ;_ end of foreach
  (princ)
) ;_ end of defun


(defun C:Bhamcanon (/ tblss cnt laynmlst laynm laynmlst itm)
  (setq tblss (ssget "x" (list (cons 0 "insert") (cons 8 "WS_TITLEBLOCK"))))
  (setq cnt 0)
  (setq laynmlst (list))
  (while (< cnt (sslength tblss))
    (setq laynm (cdr (assoc 410 (entget (ssname tblss cnt)))))
    (setq cnt (1+ cnt))
    (setq laynmlst (append laynmlst (list laynm)))
  ) ;_ end of while
  (foreach itm laynmlst
    (command "-plot"		"Y"		   itm		      "\\\\datusbrm001\\Canon iR5020/iR6020 PCL6"	       "11x17"		  "Inches"	     "Landscape"
	     "No"		"Extents"	   "Fit"	      "Center"		 "Yes"		    "Watt Stopper Lite 160.ctb"		  "Yes"		     "No"
	     "No"		"No"		   ""		      "No"		 "Yes"
	    ) ;_ end of command
  ) ;_ end of foreach
  (princ)
) ;_ end of defun

(defun C:Bham500 (/ tblss cnt laynmlst laynm laynmlst itm)
  (setq tblss (ssget "x" (list (cons 0 "insert") (cons 8 "WS_TITLEBLOCK"))))
  (setq cnt 0)
  (setq laynmlst (list))
  (while (< cnt (sslength tblss))
    (setq laynm (cdr (assoc 410 (entget (ssname tblss cnt)))))
    (setq cnt (1+ cnt))
    (setq laynmlst (append laynmlst (list laynm)))
  ) ;_ end of while
  (foreach itm laynmlst
    (command "-plot"		 "Y"		     itm		 "\\\\datusbrm001\\HP DesignJet 500 24 by HP"		     "ANSI D"		 "Inches"	     "Landscape"
	     "No"		 "Extents"	     "Fit"		 "Center"	     "Yes"		 "Watt Stopper Lite 160.ctb"		 "Yes"		     "No"
	     "No"		 "No"		     ""			 "No"		     "Yes"
	    ) ;_ end of command

  ) ;_ end of foreach
  (princ)
) ;_ end of defun

(defun C:Bham500C (/ tblss cnt laynmlst laynm laynmlst itm)
  (setq tblss (ssget "x" (list (cons 0 "insert") (cons 8 "WS_TITLEBLOCK"))))
  (setq cnt 0)
  (setq laynmlst (list))
  (while (< cnt (sslength tblss))
    (setq laynm (cdr (assoc 410 (entget (ssname tblss cnt)))))
    (setq cnt (1+ cnt))
    (setq laynmlst (append laynmlst (list laynm)))
  ) ;_ end of while
  (foreach itm laynmlst
    (command "-plot"		 "Y"		     itm		 "\\\\datusbrm001\\HP DesignJet 500 24 by HP"		     "ANSI C"		 "Inches"	     "Landscape"
	     "No"		 "Extents"	     "Fit"		 "Center"	     "Yes"		 "Watt Stopper Lite 160.ctb"		 "Yes"		     "No"
	     "No"		 "No"		     ""			 "No"		     "Yes"
	    ) ;_ end of command


  ) ;_ end of foreach
  (princ)
) ;_ end of defun




(defun saveVars	()

;;;--- Get the key of the choice made
  (setq myChoice (get_tile "mychoice"))
  (setq myChoice1 (get_tile "mychoice1"))
;;;--- Get the value of each item
;;;  (setq choice1 (atoi (get_tile "WS500")))
;;;  (setq choice2 (atoi (get_tile "WS500C")))
;;;  (setq choice3 (atoi (get_tile "WSPDF")))
;;;  (setq choice4 (atoi (get_tile "WSPDFAL")))
;;;  (setq choice5 (atoi (get_tile "Bham500")))
;;;  (setq choice6 (atoi (get_tile "Bham500c")))
;;;  (setq choice7 (atoi (get_tile "WS5200")))
;;;  (setq choice8 (atoi (get_tile "BhamPDF")))
;;;  (setq choice9 (atoi (get_tile "BhamPDFAL")))
;;;  (setq choice10 (atoi (get_tile "BhamCannon")))
;;;  (setq choice11 (atoi (get_tile "Bham8150")))
) ;_ end of defun

(defun C:Batch-Print (/ *error*)
;;;  (defun *error* (msg)
;;;    (setvar "cmdecho" 1)
;;;    (princ "\nRoutine terminated.")
;;;    (princ)
;;;  ) ;_ end of defun
  (if (setq dwgss (dos_getfilem
		    "Select DWGs to create PDFs"
		    "H:\\sfdc"
		    "Drawing files (*.dwg)|*.dwg|All files (*.*)|*.*||"
		  ) ;_ end of dos_getfilem
      ) ;_ end of setq
    (progn
      (setq dirpath (car dwgss))
      (setq dwglist (strp_mmbr dirpath dwgss))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;--- Load the dcl file
      (setq dcl_id (load_dialog "WSPrint.dcl"))
       
;;;--- Load the dialog definition if it is not already loaded
      (if (not (new_dialog "WSPrint" dcl_id))
	(progn
	  (alert "The WSPrint.DCL file was not found.")
	  (exit)
	) ;_ end of progn
      ) ;_ end of if

;;;--- If an action event occurs, do this function
      (action_tile "accept" "(setq ddiag 2)(savevars)(done_dialog)") ;(savevars)
      (action_tile "cancel" "(setq ddiag 1)(done_dialog)")

;;;--- Display the dialog box
      (start_dialog)

;;;--- Unload the dialog box
      (unload_dialog dcl_id)

;;;--- If the user pressed the Cancel button
      (if (= ddiag 1)
	(princ "\n WSPrint cancelled!")
      ) ;_ end of if

;;;--- If the user pressed the Okay button
      (if (= ddiag 2)
	(progn
	  (princ "\nUser selected Okay button")
	  (cond
	    ((= myChoice "WS500") (setq prntyp "WS500"))
	    ((= myChoice "WS500C") (setq prntyp "WS500C"))
	    ((= myChoice "WSPDF") (setq prntyp "WSPDF"))
	    ((= myChoice "WSPDFAL") (setq prntyp "WSPDFAL"))
	    ((= myChoice "WS5200") (setq prntyp "WS5200"))
	    ((= myChoice "WSCANON") (setq prntyp "WSCANON"))
	    ((= myChoice1 "Bham500") (setq prntyp "Bham500"))
	    ((= myChoice1 "Bham500C") (setq prntyp "Bham500C"))
	    ((= myChoice1 "BhamPDF") (setq prntyp "BhamPDF"))
	    ((= myChoice1 "BhamPDFAL") (setq prntyp "BhamPDFAL"))
	    ((= myChoice1 "BhamCannon") (setq prntyp "BhamCannon"))
	    ((= myChoice1 "Bham8150") (setq prntyp "Bham8150"))
	  ) ;_ end of cond
	) ;_ end of progn
      ) ;_ end of if


;;;--- Display the dialog box
      (start_dialog)

;;;--- Unload the dialog box
      (unload_dialog dcl_id)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Run Batch

      (setq ScrFile (strcat "c:\\batchme.scr"))
      (setq Ofil (open ScrFile "W"))
      (write-line "SDI 0" Ofil) ; Force Multi-Document mode
      (write-line (strcat "(setvar " (chr 34) "FILEDIA" (chr 34) " 0)") Ofil)
      (foreach Dwg DwgList
	(setq FullPath (strcat DirPath Dwg))
	(write-line (strcat "_.open " (chr 34) FullPath (chr 34)) Ofil)
	(write-line (strcat "(load " (chr 34) "wsprint.lsp" (chr 34) ")") Ofil)
	(write-line (strcat prntyp) Ofil)
	(write-line (strcat "(setvar " (chr 34) "FILEDIA" (chr 34) " 1)") Ofil)
	(write-line "_.qsave" Ofil)
	(write-line "_.close" Ofil)
      ) ; foreach
      (write-line (strcat "(setvar " (chr 34) "FILEDIA" (chr 34) " 1)") Ofil)
      (close Ofil)
      (command "_.script" ScrFile)
    ) ; progn
  ) ; if
  (princ)
) ;_ end of defun

(defun STRP_MMBR (list_elem list2strip) ; by Serge Volkov
(apply 'append
(subst nil (list list_elem) (mapcar 'list list2strip))
)
)