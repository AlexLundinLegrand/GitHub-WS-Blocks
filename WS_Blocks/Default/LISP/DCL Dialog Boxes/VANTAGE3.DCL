VANTAGE3 : dialog {				//dialog name
      	label = "VANTAGE3" ;			//give it a label
 
       
	: paragraph {				//define paragraph


// example of how text is formated below
//     	: text_part {				//define more text
//     	label = "";				//some more text
//     	}

// area for change notes

//     	: text_part {
//     	label = "";
//     	}
//        : text_part {
//     	label = "___________________________________________Change Notes___________________________________________";
//     	}
//     	: text_part {
//     	label = "";
//     	}
//     	: text_part {
//     	label = "Please read these prompts, they will help you navigate the changes.";
//     	}
//     	: text_part {
//     	label = "Updated 12-22-2016";
//     	}
//     	: text_part {
//     	label = "We are creating a more automated process and there are minor changes to mention.";
//     	}
//     	: text_part {
//     	label = "In the past we used corner point osnaps for MSTP wire to connect bridges.";
//     	} 
//     	: text_part {
//     	label = "In an effort to keep manual cable and automated cable consistent, BRIDGES uses center osnaps now.";
//     	}
//     	: text_part {
//     	label = "";
//     	}
     	



     	: text_part {
     	label = "";
     	}
     	: text_part {
     	label = "____________________________________________VANTAGE3____________________________________________";
     	}
     	: text_part {
     	label = "";
     	}
     	: text_part {
     	label = "This command will restore your layers and devices to their prior states";
     	}
     	: text_part {
     	label = "It will also delete any MTEXT notes placed on the VANTAGE layer.";
     	}
     	: text_part {
     	label = "If everything looks okay, use the \"AUTOV\" command.";
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