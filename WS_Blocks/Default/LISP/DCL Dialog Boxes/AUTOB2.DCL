AUTOB2 : dialog {				//dialog name
      	label = "AUTOB2" ;			//give it a label
 
       
	: paragraph {				//define paragraph


     	: text_part {
     	label = "";
     	}
     	: text_part {
     	label = "_______________________________________________AUTOB2_______________________________________________";
     	}
     	: text_part {
     	label = "";
     	}
     	: text_part {
     	label = "This command will restore your layers and devices to their prior states";
     	}
     	: text_part {
     	label = "It will also delete any MTEXT notes placed on the MSTP layer.";
     	}
     	: text_part {
     	label = "";
     	}



//     	: text_part {				//define more text
//     	label = "";				//some more text
//     	}


     	}						//end paragraph


 
//     	: radio_button {				//define radio button
//     	  key = "rb1" ;					//give it a name
//     	  label = "Place instructions in text file" ;	//give it a label
//     	}						//end definition


     	
	: button {
       	 label = "OK";
	 key = "accept";
             alignment = left;
             width = 5;
             is_default = true;
	}
	
	: button {
       	label = "Cancel";
	key = "cancel";
	mnemonic = "C";
   	alignment = right;
   	width = 5;
	}

	
     }						//end dialog