AUTOVNOTES : dialog {				//dialog name
      	label = "AUTOVNOTES" ;			//give it a label
 
       
	: paragraph {				//define paragraph


// example of how text is formated below
//     	: text_part {				//define more text
//     	label = "";				//some more text
//     	}

	
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
     	label = "New Command";
     	}
     	: text_part {
     	label = "";
     	}


     	
     	: text_part {
     	label = "";
     	}
     	: text_part {
     	label = "_______________________________________________AUTOV_______________________________________________";
     	}
     	: text_part {
     	label = "";
     	}
     	: text_part {
     	label = "This command will assist in filling out attributes on layout blocks for Vantage Risers.";
     	}
     	: text_part {
     	label = "First, use the \"VANTAGE\" command to isolate the product blocks and existing wires.";
     	}
     	: text_part {
     	label = "You can stay in this layer state for the entire process of drawing wire and filling out attributes.";
     	}
     	: text_part {
     	label = "To reset blocks to the VANTAGE state after using \"AUTOV\", use the \"CLEAR\" command.";
     	}
     	: text_part {
     	label = "To exit use the \"VANTAGE3\" command which will restore layers and drafting settings.";
     	}
     	: text_part {
     	label = "";
     	}
     	: text_part {
     	label = "After you type \"AUTOV\":";
     	}
     	: text_part {
     	label = "";
     	}
     	: text_part {
     	label = "1)";
     	}
     	: text_part {
     	label = "You can choose any VANTAGE wire or panel and the code will guide you through filling out the attributes.";
     	}
     	: text_part {
     	label = "";
     	}
     	: text_part {
     	label = "2)Main Panels";
     	}
     	: text_part {
     	label = "Choose either a single block or polyline on the controller bus layer (red).";
     	}
     	: text_part {
     	label = "Enter the starting number for the main panels.";
     	}
     	: text_part {
     	label = "The code will loop through the series of wires and blocks, filling out attributes until the final panel is reached.";
     	}
     	: text_part {
     	label = "";
     	}
     	: text_part {
     	label = "3)Secondary Panels";
     	}
     	: text_part {
     	label = "Choose a single polyline on the enclosure bus layer (green) that connects to the main panel (red).";
     	}
     	: text_part {
     	label = "Enter the starting number for the secondary run.";
     	}
     	: text_part {
     	label = "The options are 2,3 or 4.";
     	}
     	: text_part {
     	label = "The code will add and A or B to the end of the number.";
     	}
     	: text_part {
     	label = "The code will loop through the series of wires and blocks, filling out attributes until the final panel is reached.";
     	}
     	: text_part {
     	label = "";
     	}
     	: text_part {
     	label = "4)Auxiliary Panels";
     	}
     	: text_part {
     	label = "Choose the first auxiliary panel (white).";
     	}
     	: text_part {
     	label = "The code will loop through the series blocks, filling out attributes until the final panel is reached.";
     	}
     	: text_part {
     	label = "";
     	}
     	: text_part {
     	label = "5)Panel Components";
     	}
     	: text_part {
     	label = "Choose the polyline on the panel components layer (white) that connects to the panel (any kind of panel).";
     	}
     	: text_part {
     	label = "The code will loop through the series of wires and blocks, filling out attributes until the final component is reached.";
     	}
     	: text_part {
     	label = "";
     	}
     	: text_part {
     	label = "6)Station Bus";
     	}
     	: text_part {
     	label = "Move the fitpoints of any wires necessary so only ONE STAION BUS WIRE connects to the processor.";
     	}
     	: text_part {
     	label = "Choose a single station bus wire (purple) that connects to the processor (white).";
     	}
     	: text_part {
     	label = "The code will loop through the series of wires and blocks, filling out attributes until the final component is reached.";
     	}
     	: text_part {
     	label = "The code can reach 3_18_AWG wire (orange) as well.";
     	}
     	: text_part {
     	label = "";
     	}
     	: text_part {
     	label = "6)RS232";
     	}
     	: text_part {
     	label = "Choose a single RS232 bus wire (cyan) that connects to the processor.";
     	}
     	: text_part {
     	label = "The code will loop through the series of wires and blocks, filling out attributes until the final component is reached.";
     	}
     	: text_part {
     	label = "";
     	}
     	: text_part {
     	label = "7)";
     	}
     	: text_part {
     	label = "Finally, use the \"VANTAGE3\" command when you are done with all your networking on this layout.";
     	}
     	: text_part {
     	label = "";
     	}
     	
     	}						//end paragraph


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