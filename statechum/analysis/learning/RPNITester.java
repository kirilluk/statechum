 package statechum.analysis.learning;

import java.util.*;



public class RPNITester {

	public static void main(String[] args){
		HashSet sPlus = new HashSet();
		HashSet sMinus = new HashSet();
		
		/*String[] string1 = {"text_tool","set_position","edit_text","text_tool","get_textbox","edit_text","delete_text"};
		String[] string2 = {"figure_tool","set_position","set_dimensions","set_dimensions","set_dimensions","figure_tool","set_position","set_dimensions","set_dimensions","set_dimensions","figure_tool","set_position","set_dimensions","set_dimensions","set_dimensions","set_dimensions"};
		String[] string3 = {"text_tool","set_position","edit_text","figure_tool","set_position","set_dimensions","set_dimensions","set_dimensions","figure_tool","set_position","set_dimensions","set_dimensions","set_dimensions","text_tool","get_textbox","edit_text","text_tool","get_textbox","edit_text","delete_text"};
		
		sPlus.add(Arrays.asList(string1));
		sPlus.add(Arrays.asList(string2));
		sPlus.add(Arrays.asList(string3));*/
		/*String[] string1 = {"open", "close", "start", "stop", "start"};
		String[] string2 = {"start", "alarm", "propagated", "emergency stop", "emergency open"};
		String[] string3 = {"start", "stop", "alarm", "propagated", "emergency open", "close"};
		sPlus.add(Arrays.asList(string1));
		sPlus.add(Arrays.asList(string2));
		sPlus.add(Arrays.asList(string3));
		String[] string4 = {"start", "open"};
		sMinus.add(Arrays.asList(string4));*/
		String[] string1 = {"load", "edit", "edit", "save", "close"};
		String[] string2 = {"load", "edit", "save", "close"};
		String[] string3 = {"load", "close", "load"};
		String[] string4 = {"close"};
		sPlus.add(Arrays.asList(string1));
		sPlus.add(Arrays.asList(string2));
		sPlus.add(Arrays.asList(string3));
		sMinus.add(Arrays.asList(string4));
		//sMinus.add(Arrays.asList(new String[]{"load","save","close"}));
		
		//sPlus.add(Arrays.asList(new String[]{"m","a","z"}));
		//sPlus.add(Arrays.asList(new String[]{"m","z"}));
		//sPlus.add(Arrays.asList(new String[]{"m","a","z","u"}));
		//sPlus.add(Arrays.asList(new String[]{"m","z","u"}));
		//sPlus.add(Arrays.asList(new String[]{"n","o","p","q","a","z"}));
		//sMinus.add(Arrays.asList(new String[]{"n","o","p","q","z"}));
		//sMinus.add(Arrays.asList(new String[]{"a"}));
		LearningVisualiser v = new LearningVisualiser();
		v.construct(sPlus, sMinus);
		
	}
	
	
	
	
}
