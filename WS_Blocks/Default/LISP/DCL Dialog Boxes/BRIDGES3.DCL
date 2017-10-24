BRIDGES3 : dialog {				//dialog name
      	label = "BRIDGES3" ;			//give it a label
 
       
	: paragraph {				//define paragraph


     	: text_part {
     	label = "";
     	}
        : text_part {
     	label = "___________________________________________Change Notes___________________________________________";
     	}
     	: text_part {
     	label = "";
     	}
     	: text_part {
     	label = "Please read these prompts, they will help you navigate the changes.";
     	}
     	: text_part {
     	label = "Updated 12-22-2016";
     	}
     	: text_part {
     	label = "";
     	}
     	: text_part {
     	label = "____________________________________________BRIDGES3____________________________________________";
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
     	label = "If everything looks okay, use the \"AUTOB\" command.";
     	}
     	: text_part {
     	label = "";
     	}



//     	: text_part {				//define more text
//     	label = "";				//some more text
//     	}


     	}						//end paragraph


//      button for placing text in drawing, commented out for now
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