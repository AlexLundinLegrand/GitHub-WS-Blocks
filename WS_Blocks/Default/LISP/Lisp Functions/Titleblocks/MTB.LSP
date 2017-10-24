; --- MTB Function ---
; Pull all attributes and values from one title block
; Alex Lundin 03-07-2016
(defun C:MTB ()
  
  (setq tb (car(entsel "\n Select the titleblock with desired values: ")))

					(setq en(entget tb))
			  		(setq group66(cdr(assoc 66 en)))
  					
					(setq en2(entnext tb))
  					(setq enlist2(entget en2))
  					(setq attVal2(cdr(assoc 2 enlist2)))
  					(setq attName(cdr(assoc 1 enlist2)))

  					(setq en3(entnext en2))
  					(setq enlist3(entget en3))
  					(setq attVal3(cdr(assoc 2 enlist3)))
  					(setq attDate(cdr(assoc 1 enlist3)))

  					(setq en4(entnext en3))
  					(setq enlist4(entget en4))
  					(setq attVal4(cdr(assoc 2 enlist4)))
  					(setq attSheet(cdr(assoc 1 enlist4)))

  					(setq en5(entnext en4))
  					(setq enlist5(entget en5))
  					(setq attVal5(cdr(assoc 2 enlist5)))
  					(setq attPRJB(cdr(assoc 1 enlist5)))

  					(setq en6(entnext en5))
  					(setq enlist6(entget en6))
  					(setq attVal6(cdr(assoc 2 enlist6)))
  					(setq attProject1(cdr(assoc 1 enlist6)))
  
  					(setq en7(entnext en6))
  					(setq enlist7(entget en7))
  					(setq attVal7(cdr(assoc 2 enlist7)))
  					(setq attProject2(cdr(assoc 1 enlist7)))

  					(setq en8(entnext en7))
  					(setq enlist8(entget en8))
  					(setq attVal8(cdr(assoc 2 enlist8)))
  					(setq attProject3(cdr(assoc 1 enlist8)))
  
    					(setq en9(entnext en8))
  					(setq enlist9(entget en9))
  					(setq attVal9(cdr(assoc 2 enlist9)))
  					(setq attProject4(cdr(assoc 1 enlist9)))

      					(setq en10(entnext en9))
  					(setq enlist10(entget en10))
  					(setq attVal10(cdr(assoc 2 enlist10)))
  					(setq attProject5(cdr(assoc 1 enlist10)))

        				(setq en11(entnext en10))
  					(setq enlist11(entget en11))
  					(setq attVal11(cdr(assoc 2 enlist11)))
  					(setq attRev1(cdr(assoc 1 enlist11)))

          				(setq en12(entnext en11))
  					(setq enlist12(entget en12))
  					(setq attVal12(cdr(assoc 2 enlist12)))
  					(setq attRev2(cdr(assoc 1 enlist12)))

            				(setq en13(entnext en12))
  					(setq enlist13(entget en13))
  					(setq attVal13(cdr(assoc 2 enlist13)))
  					(setq attRev3(cdr(assoc 1 enlist13)))

  

)




; --- MTB2 Function ---
; Paste all values into an identical title block
; Alex Lundin 03-07-2016
(defun C:MTB2 ()
  
  (setq 2tb(car(entsel "\n Select the new titleblock: ")))
  					(setq 2en(entget 2tb))
			  		(setq group66(cdr(assoc 66 2en)))
  
  					(setq 2en2(entnext 2tb))
  					(setq 2enlist2(entget 2en2))
  					(setq 2attVal2(cdr(assoc 2 2enlist2)))
  					(setq 2attName(cdr(assoc 1 2enlist2)))
  					(entmod (subst (assoc 1 enlist2) (assoc 1 2enlist2) 2enlist2))

    					(setq 2en3(entnext 2en2))
  					(setq 2enlist3(entget 2en3))
  					(setq 2attVal3(cdr(assoc 2 2enlist3)))
  					(setq 2attDate(cdr(assoc 1 2enlist3)))
					(entmod (subst (assoc 1 enlist3) (assoc 1 2enlist3) 2enlist3))
  
  					(setq 2en4(entnext 2en3))
  					(setq 2enlist4(entget 2en4))
  					(setq 2attVal4(cdr(assoc 2 2enlist4)))
  					(setq 2attSheet(cdr(assoc 1 2enlist4)))
					(entmod (subst (assoc 1 enlist4) (assoc 1 2enlist4) 2enlist4))
  
  					(setq 2en5(entnext 2en4))
  					(setq 2enlist5(entget 2en5))
  					(setq 2attVal5(cdr(assoc 2 2enlist5)))
  					(setq 2attPRJB(cdr(assoc 1 2enlist5)))
					(entmod (subst (assoc 1 enlist5) (assoc 1 2enlist5) 2enlist5))
  
  					(setq 2en6(entnext 2en5))
  					(setq 2enlist6(entget 2en6))
  					(setq 2attVal6(cdr(assoc 2 2enlist6)))
  					(setq 2attProject1(cdr(assoc 1 2enlist6)))
					(entmod (subst (assoc 1 enlist6) (assoc 1 2enlist6) 2enlist6))
  
  					(setq 2en7(entnext 2en6))
  					(setq 2enlist7(entget 2en7))
  					(setq 2attVal7(cdr(assoc 2 2enlist7)))
  					(setq 2attProject2(cdr(assoc 1 2enlist7)))
					(entmod (subst (assoc 1 enlist7) (assoc 1 2enlist7) 2enlist7))
  
  					(setq 2en8(entnext 2en7))
  					(setq 2enlist8(entget 2en8))
  					(setq 2attVal8(cdr(assoc 2 2enlist8)))
  					(setq 2attProject2(cdr(assoc 1 2enlist8)))
					(entmod (subst (assoc 1 enlist8) (assoc 1 2enlist8) 2enlist8))
  
    					(setq 2en9(entnext 2en8))
  					(setq 2enlist9(entget 2en9))
  					(setq 2attVal9(cdr(assoc 2 2enlist9)))
  					(setq 2attProject3(cdr(assoc 1 2enlist9)))
					(entmod (subst (assoc 1 enlist9) (assoc 1 2enlist9) 2enlist9))
  
      					(setq 2en10(entnext 2en9))
  					(setq 2enlist10(entget 2en10))
  					(setq 2attVal10(cdr(assoc 2 2enlist10)))
  					(setq 2attProject4(cdr(assoc 1 2enlist10)))
					(entmod (subst (assoc 1 enlist10) (assoc 1 2enlist10) 2enlist10))

        				(setq 2en11(entnext 2en10))
  					(setq 2enlist11(entget 2en11))
  					(setq 2attVal11(cdr(assoc 2 2enlist11)))
  					(setq 2attProject5(cdr(assoc 1 2enlist11)))
					(entmod (subst (assoc 1 enlist11) (assoc 1 2enlist11) 2enlist11))
  
          				(setq 2en12(entnext 2en11))
  					(setq 2enlist12(entget 2en12))
  					(setq 2attVal12(cdr(assoc 2 2enlist12)))
  					(setq 2attRev1(cdr(assoc 1 2enlist12)))
					(entmod (subst (assoc 1 enlist12) (assoc 1 2enlist12) 2enlist12))
  
            				(setq 2en13(entnext 2en12))
  					(setq 2enlist13(entget 2en13))
  					(setq 2attVal13(cdr(assoc 2 2enlist13)))
  					(setq 2attRev2(cdr(assoc 1 2enlist13)))
					(entmod (subst (assoc 1 enlist13) (assoc 1 2enlist13) 2enlist13))
  
              				(setq 2en14(entnext 2en13))
  					(setq 2enlist14(entget 2en14))
  					(setq 2attVal14(cdr(assoc 2 2enlist14)))
  					(setq 2attRev3(cdr(assoc 1 2enlist14)))
					(entmod (subst (assoc 1 enlist14) (assoc 1 2enlist14) 2enlist14))
  
  
)
