/*Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 
This file is part of StateChum

StateChum is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

StateChum is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with StateChum.  If not, see <http://www.gnu.org/licenses/>.
*/ 

package statechum.TraceMaker;

import java.io.BufferedReader;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;


public class TraceFactory {

	private static String JAVA_DRAW_APP = "CH.ifa.draw.samples.javadraw.JavaDrawApp";
	private static String TRACE_OUTPUT_DIR = "Traces/";
	private static String ABSTRACT_FUNCTION_INPUT = "jHotDrawMappings.txt";	
	private  ArrayList absfunc_list = new ArrayList();
	private String daikon_output = "";
	
	
	//--------------------------------------------------------------------------------------------------------
	private void writeDaikonOutput(String fileName){
    	FileOutputStream fout;		
        
    	try {
			fout = new FileOutputStream (fileName);
			    new PrintStream(fout).println(daikon_output);
						
	     	fout.close();			
		    } catch (IOException e) {		
		e.printStackTrace();
		}
		 
	}
	//--------------------------------------------------------------------------------------------------------	
	private ArrayList<String> _readBlock(BufferedReader in)
	{
		ArrayList<String> aFunction = new ArrayList<String>();
		
	for (int i=0;i<9;i++){
		try {
			String currentLine = in.readLine();
			aFunction.add(currentLine.trim());			
		} catch (IOException e) {			
		e.printStackTrace();
		}
	}
	return aFunction;
	}
	//--------------------------------------------------------------------------------------------------------
	private  void readAbstractFunctionFile()
	{
	   try {
		    BufferedReader in = new BufferedReader(new FileReader(ABSTRACT_FUNCTION_INPUT));
	     do {
	    	 absfunc_list.add(_readBlock(in));
	    	 }
	    	 while (in.readLine() != null);
	      	        
	     in.close();
	   } catch (IOException e) {}
	   	  
	   System.out.println("reading abstract file listing complete.");
	}
	//--------------------------------------------------------------------------------------------------------
	private int executeChicory(String[] _args)
	{
		   List<String> cmdlist = new ArrayList<String>();
		    cmdlist.add ("java");
		    cmdlist.add ("-cp");
		    String cp = System.getProperty("java.class.path");
		    cmdlist.add (cp);
		    cmdlist.add ("-ea");		  
		    cmdlist.add("daikon.Chicory");
		    
		    cmdlist.addAll(Arrays.asList(_args));
		    
	    java.lang.Runtime rt = java.lang.Runtime.getRuntime();
	    int result = -1;
	    
	    try {
	    	StringBuffer chicory_args = new StringBuffer();for(String arg:cmdlist) 
	    	                                    { chicory_args.append(arg);chicory_args.append(' '); };
	    	
	    	Process chicory_proc = rt.exec(chicory_args.toString().trim());
	        result = chicory_proc.waitFor();
	    }
	    catch (Exception e) {
	      System.out.printf("Exception '%s' while executing chicory\n",e.getMessage());
	      e.printStackTrace();
	      result = -2;		      
	    }

	   
	    if (result == 0)
	    	System.out.println("Chicory finished");
	    else
	    	throw new IllegalArgumentException("Chicory failed with reslult "+result);
	     
		
	    return result;
	}
	//--------------------------------------------------------------------------------------------------------
	private int executeDaikon(String daikon_args, String dtrace_file){
		
		java.lang.Runtime rt = java.lang.Runtime.getRuntime();
	    String cp = System.getProperty("java.class.path");
	    if (cp == null)
	      cp = ".";

	    String cmdstr;
	     
	    cmdstr = String.format("java -Xmx1024m -cp %s -ea daikon.Daikon"
	                             + "%s %s --config_option daikon.inv.filter.UnjustifiedFilter.enabled=false", cp, daikon_args, dtrace_file);
	    //cmdstr = String.format("java -Xmx1024m -cp %s -ea daikon.Daikon"
	  	  //                           + "%s %s", cp, daikon_args, dtrace_file);
	  	   
	    System.out.println("Executing Daikon with the following cmdstring: "+cmdstr);  
	    Process daikon_proc = null;
	    
	    try {
	      daikon_proc = rt.exec(cmdstr);
	      
	    //save output to a string
	      daikon_output="";
	      InputStream in = daikon_proc.getInputStream();
	      int c;
	      while ((c = in.read()) != -1) 	        
	    	  daikon_output = daikon_output.concat(String.valueOf((char)c));
	          System.out.print((char)c);
	      
	      daikon_proc.waitFor();
	      
	    } catch (Exception e) {
	      System.out.printf("Exception '%s' while executing '%s'\n", e, cmdstr);
	      return 1;
	    }
	    System.out.println("Daikon executed successfully.");
	    System.out.println("output"+daikon_output);
		return 0;
	}

//--------------------------------------------------------------------------------------------------------
	private void rawTraces()
	{
		String dtracelist="";
		
		for(int i=0;i<1;i++){
		String [] args = {"--dtrace-file=mytrace"+String.valueOf(i)+".dtrace",
				 "--output-dir="+TRACE_OUTPUT_DIR,				 
	             JAVA_DRAW_APP};
		
	    //executeChicory(args);
	    dtracelist=dtracelist+TRACE_OUTPUT_DIR+"mytrace"+String.valueOf(i)+".dtrace ";
		}
	    
	    executeDaikon("",dtracelist);
	    writeDaikonOutput(TRACE_OUTPUT_DIR+"invariants.txt");
	    System.exit(1);
		
	}
//--------------------------------------------------------------------------------------------------------
	TraceFactory(){
	
	//	rawTraces();
	    
		readAbstractFunctionFile();
		

		for(int j=0;j<absfunc_list.size();j++){			 				 
			 ArrayList<String> absfunc = new ArrayList<String>();
			 absfunc = (ArrayList<String>) absfunc_list.get(j);

			 String abstractFunctionName= absfunc.get(0);
			 String sENTER=absfunc.get(1);
			 String startFunction=absfunc.get(2);
			 String sEXIT=absfunc.get(3);
			 String eENTER=absfunc.get(4);
			 String endFunction=absfunc.get(5);
			 String eEXIT=absfunc.get(6);
			 String merge=absfunc.get(7);
			 int sampleSize=Integer.valueOf((absfunc.get(8)));
			 
			 
			 String output_dir         = TRACE_OUTPUT_DIR+abstractFunctionName+"/";
			 String dtrace_file_parsed_list = "";
			 for(int i=0;i<sampleSize;i++){
				 String sample= String.valueOf(i);
			     String dtrace_file        = abstractFunctionName+sample+".dtrace";
			     String dtrace_file_parsed = abstractFunctionName+"_parsed"+sample+".dtrace";
			     String inv_file = abstractFunctionName+"_parsed"+sample+".inv.gz";
			 
			 
			     System.out.println("["+String.valueOf(i+1)+"/"+String.valueOf(sampleSize)+"] Processing Abstract Function: "+abstractFunctionName);
			     System.out.println("Start method: "+startFunction);
			     System.out.println("End method: "+endFunction);
			     System.out.println("----------------------------------------------------");
			    
			     String [] args = {"--dtrace-file="+dtrace_file,
					 "--output-dir="+output_dir,
					 "--ppt-select-pattern="+startFunction,
					 "--ppt-select-pattern="+endFunction,
	                  JAVA_DRAW_APP};
			 
			   executeChicory(args);
			 
     				AutoAbstractParser traceParser=
				        new AutoAbstractParser(output_dir+dtrace_file,
						                       output_dir+dtrace_file_parsed,
						                       abstractFunctionName,
						                       sENTER, startFunction,sEXIT,
						                       eENTER,endFunction,eEXIT,merge);
		     
		     
		     
		     
			      dtrace_file_parsed_list =  dtrace_file_parsed_list.concat(output_dir+dtrace_file_parsed+" ");
			  
			 }
			 
		     executeDaikon("",dtrace_file_parsed_list);		
		     writeDaikonOutput(output_dir+abstractFunctionName+"invariants.txt");
		}						   						     
			
	}
//--------------------------------------------------------------------------------------------------------
	public static void main(String[] args) {
	  
		TraceFactory abs = new TraceFactory();			
	}

}
