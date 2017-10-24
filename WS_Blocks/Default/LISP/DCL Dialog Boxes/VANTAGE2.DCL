VANTAGE2 : dialog {				//dialog name
      	label = "VANTAGE2" ;			//give it a label
 
       
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
     	label = "____________________________________________VANTAGE2_____________________________________________";
     	}  	
     	: text_part {
     	label = "Verify all the wires are correct and identify which components are in each panel.";
     	}
     	: text_part {
     	label = "After you press OK:";
     	}
     	: text_part {
     	label = "";
     	}
     	: text_part {
     	label = "1)";
     	}
	: text_part {
     	label = "All remaining product blocks will be visible.";
     	}
     	: text_part {
     	label = "";
     	}
     	: text_part {
     	label = "2)";
     	}
     	: text_part {
     	label = "Verify that your cables are not crossing:";
     	}
     	: text_part {
     	label = "Wires - avoid crossing cables of any kind at all times.";
     	}
     	: text_part {
     	label = "Parts   - room controllers, sensors, switches, etc...";
     	}
     	: text_part {
     	label = "";
     	}
	: text_part {
     	label = "3)";
     	}
     	: text_part {
     	label = "When possible, verify that your cables are not crossing:";
     	}
     	: text_part {
     	label = "Walls - run cabling through hallways when possible.";
     	}
     	: text_part {
     	label = "";
     	}
     	: text_part {
     	label = "4)";
     	}
     	: text_part {
     	label = "Move any cables that you need to.";
     	}
     	: text_part {
     	label = "Select the polyline with a left click, and then hover over a blue fit point to get sub options for the polyline fit points.";
     	}
	: text_part {
     	label = "From here you can stretch\/add\/remove fit points.";
     	}
     	: text_part {
     	label = "";
     	}
     	: text_part {
     	label = "5)";
     	}
     	: text_part {
     	label = "Draw boundaries around all the components and the panel they are in.";
     	}
     	: text_part {
     	label = "This will make it clear what parts are in each cabinet.";
     	}
     	: text_part {
     	label = "Use the \"RECT\" command to draw rectangles.";
     	}
     	: text_part {
     	label = "The _WS_VANTAGE_PANEL_BOUNDARY is a no plot layer, so it will not show on the print.";
     	}
	: text_part {
     	label = "";
     	}
     	: text_part {
     	label = "6)";
     	}
     	: text_part {
     	label = "use \"VANTAGE3\" when you are done.";
     	}

//empty block for text lines     	
//     	: text_part {
//     	label = "";
//     	}
//     	: text_part {
//     	label = "";
//     	}
//     	: text_part {
//     	label = "";
//     	}
//     	: text_part {
//     	label = "";
//     	}
//	: text_part {
//     	label = "";
//     	}
//     	: text_part {
//     	label = "";
//     	}
//     	: text_part {
//     	label = "";
//     	}
//     	: text_part {
//     	label = "";
//     	}
//     	: text_part {
//     	label = "";
//     	}
//     	: text_part {
//     	label = "";
//     	}
//     	: text_part {
//     	label = "";
//     	}
//	: text_part {
//     	label = "";
//     	}
     	
  	
	



//     	: text_part {				//define more text
//     	label = "";				//some more text
//     	}


     	}					//end paragraph


        : radio_button {				//define radion button
     	  key = "rb1" ;					//give it a name
     	  label = "Place instructions in drawing" ;	//give it a label
        }						//end definition
 
     	: radio_button {				//define radio button
     	  key = "rb2" ;					//give it a name
     	  label = "Place instructions in text file" ;	//give it a label
     	}						//end definition


     	
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