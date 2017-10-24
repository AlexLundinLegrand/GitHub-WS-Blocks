WSPrint : dialog { 
    label="Select A Print Style";

       : row {
         : radio_column {
         key = "mychoice";
         label      = "Plano, TX" ;
	  : radio_button {
                key        = "WS500" ;
                label      = "HP DesignJet 500 24 by HP - D Size" ;
            }
           : radio_button {
                key        = "WS500C" ;
                label      = "HP DesignJet 500 24 by HP - C Size" ;
            }
	   : radio_button {
                key        = "WSPDF" ;
                label      = "PDF - D Size" ;
            }
	   : radio_button {
                key        = "WSPDFAL" ;
                label      = "PDF - A Size Landscape" ;
            }
	   : radio_button {
                key        = "WS5200" ;
                label      = "HP LaserJet 5200 PS - 11x17" ;
            }
	   : radio_button {
                key        = "WSCANON" ;
                label      = "Canon iR-ADV C5030/5035 PS3" ;
            }
          }
       : radio_column {
         label      = "Birmingham, AL" ;
         key = "mychoice1";         
           : radio_button {
                key        = "Bham500" ;
                label      = "HP DesignJet 500 24 by HP - D Size" ;
            }
           : radio_button {
                key        = "Bham500C" ;
                label      = "HP DesignJet 500 24 by HP - C Size" ;
            }
	   : radio_button {
                key        = "BhamPDF" ;
                label      = "PDF - D Size" ;
            }
	   : radio_button {
                key        = "BhamPDFAL" ;
                label      = "PDF - A Size Landscape" ;
            }
	   : radio_button {
                key        = "Bham5200" ;
                label      = "HP LaserJet 5200 PS - 11x17" ;
            }
	   : radio_button {
                key        = "BhamCannon" ;
                label      = "Canon iR5020/iR6020 PCL6 - 11x17" ;
            }
	   : radio_button {
                key        = "Bham8150" ;
                label      = "HP LaserJet 8150 Series PCLxx - 11x17" ;
            }
        }
}
ok_cancel;
}
