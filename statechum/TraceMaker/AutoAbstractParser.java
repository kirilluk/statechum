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

import java.io.*; 

import java.util.ArrayList;
import java.util.StringTokenizer;

public class AutoAbstractParser {
	
	private  ArrayList absfunc_list = new ArrayList();
	
	
	private static String absfunction;
	private static String start_method;
	private static String end_method;
	private static String start_method_ENTER;
	private static String start_method_EXIT;
	private static String end_method_ENTER; 
	private static String end_method_EXIT;
	private static Boolean merge_methods;
	
	private static String abstract_function_input;
	private static String dtracefile_input;
	private static String DECLARE = "DECLARE";
	private static String PPT = "this_invocation_nonce";
    private String currentLine = "";
    private char NEWLINE = '\n';
    private ArrayList<String> blocks;
    private int arrayIndex=0;
    private String arrayString="";
    private String dtracefile_output;
    private ArrayList searchResult = new ArrayList();
   
    private String necessaryWords[] = {"ListImplementors","VarComparability"};
	
//------------------------------------------------------------------------------------------
    private void outputTrace()
    {
    	FileOutputStream fout;		
     
    	try {
			fout = new FileOutputStream (dtracefile_output);
			for(java.util.Iterator<String> it = blocks.iterator();it.hasNext();){
				String aBlock = it.next();
				System.out.println(aBlock);
			    new PrintStream(fout).println(aBlock);
			}
			
	     	fout.close();			
		    } catch (IOException e) {		
		e.printStackTrace();
		}
		 	 
	     
    }
 //------------------------------------------------------------------------------------------
    private void replaceRegExpWithTextInBlock(int index,String replaceThisEx,String withThat)
    {
    	String s = blocks.get(index);
    	s = s.replaceAll(replaceThisEx, withThat);
    	blocks.set(index,s);
    	
    	//blocks.set(index,blocks.get(index).replaceAll(replaceThisEx, withThat));
    }
   
//------------------------------------------------------------------------------------------
	private void inputTrace(){
	   try {
		   BufferedReader in = new BufferedReader(new FileReader(dtracefile_input));
	        while ((currentLine = in.readLine()) != null) {
	        	if (currentLine.length()==0){
	        		blocks.add(arrayIndex,arrayString+NEWLINE);
	        		arrayIndex++;
	        		arrayString="";
	        	} else 
	        		if (arrayString == "")//don't put a newline on an empty line.
	        			arrayString = currentLine; else
				        arrayString = arrayString+NEWLINE+currentLine;
	        }	        
	       in.close();
	       } catch (IOException e) {e.printStackTrace();}
	}
//------------------------------------------------------------------------------------------	
private ArrayList findAllIndiciesContaining(String aString)
{
	ArrayList r = new ArrayList();
	int i=0;
	for(java.util.Iterator<String> it = blocks.iterator();it.hasNext();){
		String aBlock =  it.next();
		if (aBlock.contains(aString))
			r.add(i);
		  
		i++;
	}
	return r;
}
//----------------------------------------------------------------------------------------------------
	private String abbrMethod(String aMethodName)
	{
		return (aMethodName.substring(aMethodName.lastIndexOf(".")+1));
	}
	
//------------------------------------------------------------------------------------------
	private void renameMethodVariables(String method_name){
		searchResult = findAllIndiciesContaining(method_name);
		for(java.util.Iterator<Integer> it = searchResult.iterator();it.hasNext();){
			int index = it.next().intValue();
			//if(!(blocks.get(index).contains("this_invocation_nonce")))//do not change the string this_invocation_nonce
			//need regular expression here.
			    blocks.set(index,blocks.get(index).replace("this",abbrMethod(method_name)+".this"));
		}
			
		
	}
//------------------------------------------------------------------------------------------
private void renameMethods(String method_name)	
{
	searchResult = findAllIndiciesContaining(method_name);
	for(java.util.Iterator<Integer> it = searchResult.iterator();it.hasNext();){
		int index = it.next().intValue();		  
		   replaceRegExpWithTextInBlock(index,method_name,absfunction);		   		   	
	}
}
//------------------------------------------------------------------------------------------
private String extractDeclare(String aString)
{
	int firstNL = aString.indexOf(NEWLINE);
    int secNL = aString.indexOf(NEWLINE,firstNL+1);
    
    return aString.substring(0,secNL+1);

}
//------------------------------------------------------------------------------------------
private String pasteDeclare(String aString)
{
	int firstNL = aString.indexOf(NEWLINE);
    int secNL = aString.indexOf(NEWLINE,firstNL+1);
    
    return aString.substring(0,secNL+1);
	
}

//------------------------------------------------------------------------------------------
private String cutDeclare(String aString)
{
	int firstNL = aString.indexOf(NEWLINE);
    int secNL = aString.indexOf(NEWLINE,firstNL+1);
    
    return aString.substring(secNL+1);
	
}
//------------------------------------------------------------------------------------------
private ArrayList<String> findAllBlocksContaining(String aString)
{
	ArrayList<String> searchResults = new ArrayList<String>();
	for(java.util.Iterator<String> it = blocks.iterator();it.hasNext();){		
		String aBlock =  it.next();
		if (aBlock.contains(aString))
			searchResults.add(aBlock);
	}
	
	
	return searchResults;
	
}
//------------------------------------------------------------------------------------------
private String findBlock(String s1,String s2,String s3){
	
	ArrayList<String> searchResults = new ArrayList<String>();
	for(java.util.Iterator<String> it = blocks.iterator();it.hasNext();){		
		String aBlock =  it.next();
		if(aBlock.contains(s1)&&(aBlock.contains(s2))&&(aBlock.contains(s3)))
			return aBlock;
	}
	
	return null;
}
//------------------------------------------------------------------------------------------
private void mergeMethods(String methodStart,String methodEnd)//merge the methods for abstract_function
{
	ArrayList<String> mergedBlocks = necessaryBlocks();
	
	  
	String DECLARE_method1=findBlock(DECLARE,methodStart,start_method_ENTER);
	String DECLARE_method2=findBlock(DECLARE,methodEnd,end_method_EXIT);
	String method1_enter=findBlock(PPT,methodStart,start_method_ENTER);
	String method1_exit=findBlock(PPT,methodStart,start_method_EXIT);
	String method2_enter=findBlock(PPT,methodEnd,end_method_ENTER);
	String method2_exit=findBlock(PPT,methodEnd,end_method_EXIT);
	
	System.out.println("merging");
	
	String DECLARE_A_ENTER  =  DECLARE_method1+cutDeclare(DECLARE_method2);
	String DECLARE_A_EXIT   =  pasteDeclare(DECLARE_method2)+cutDeclare(DECLARE_method1)+cutDeclare(DECLARE_method2);
	String A_enter    =  method1_enter+cutDeclare(method2_enter);
	String A_exit     =  pasteDeclare(method2_exit)+cutDeclare(method1_exit)+cutDeclare(method2_exit);
	
	mergedBlocks.add(DECLARE_A_ENTER);
	mergedBlocks.add(DECLARE_A_EXIT);
	mergedBlocks.add(A_enter);
	mergedBlocks.add(A_exit);
	
	blocks=mergedBlocks;

}
//------------------------------------------------------------------------------------------
	private void _mergeMethods(String methodStart,String methodEnd)//merge the methods for abstract_function
	{  
		ArrayList<String> s1 = new ArrayList<String>();
		ArrayList<String> s2 = new ArrayList<String>();
		
		 ArrayList<String> mergedBlocks = necessaryBlocks();
		 
		
		s1 = findAllBlocksContaining(methodStart);
		s2 = findAllBlocksContaining(methodEnd);
		
		int mergeLength=mergeLength=s1.size();
		
		if(s1.size() != s2.size()){
		   System.out.println("Method merging mismatch.");			
		   if (s1.size() > s2.size()) mergeLength=s2.size(); else
			   mergeLength=s1.size();
		};
	
			for (int i=0;i<mergeLength;i++){				
			   mergedBlocks.add(s1.get(i)+cutDeclare(s2.get(i)));
			   
		    }
			for(int i=0;i<mergeLength;i++){
				
				
				mergedBlocks.add(extractDeclare(s2.get(i))+cutDeclare(s1.get(i))+cutDeclare(s2.get(i)));
			}
			
							
		blocks = mergedBlocks;
		
	}
//--------------------------------------- ---------------------------------------------------
	private static ArrayList getListFromBlock(String aBlock){
	       ArrayList list = new ArrayList();
	       
	       StringTokenizer tokens = new StringTokenizer(aBlock,"\n");
	       while(tokens.hasMoreTokens()){
	           list.add(tokens.nextElement());
	       }
	       System.out.println(list);
	       return list;
	   }
//------------------------------------------------------------------------------------------
	private String blockListtoString(ArrayList<String> aList)
	{
		String aBlock = "";
		for(java.util.Iterator<String> it = aList.iterator();it.hasNext();)
		  aBlock = aBlock.concat(it.next())+"\n";
		  
			
		
		return aBlock;
	}
//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------
/*makes sure the IDs for the ENTER/EXIT abstract functions are the same. otherwise,
	  daikon will not make invariants on the function.
*/
private void syncMethods(){
	   for(int i=0;i<blocks.size();i++){	  
		   String aBlock = blocks.get(i);
		
		   		       
		   //here we make sure that it is not a declare point and it is an entry point.
		   if((!aBlock.contains(DECLARE))&&(aBlock.contains(absfunction+start_method_ENTER))){
			     ArrayList<String> blockLines = getListFromBlock(aBlock);
		         int methodNumber = Integer.valueOf(blockLines.get(2));
		         
		         boolean change=true;
		         for(int j=i;j<blocks.size();j++){//now look ahead for the matching method
		        	 String matchMethod=blocks.get(j);
		        	 
		        	 if(matchMethod.contains(absfunction+end_method_EXIT)&&change){//we found a matching method, so change it
		        		 ArrayList<String> matchLines = getListFromBlock(matchMethod);
		        		 matchLines.set(2,String.valueOf(methodNumber));
		        		 blocks.set(j, blockListtoString(matchLines));//replace it in the blocks array
		        		 change=false;//we found the function. so stop looking.
		        	 }
		         }
		   }
		}
	   
		 		   	   
}	   
//------------------------------------------------------------------------------------------	   
	//misc parsing
	private void miscParsing(){
		//isDeclareBlock
		
		ArrayList<String> newBlocks   = necessaryBlocks();
		
		   for(java.util.Iterator<String> it = blocks.iterator();it.hasNext();){
			   String aBlock = it.next();
			   ArrayList<String> blockLines = getListFromBlock(aBlock);
			   
			   int lineNumber=0;
			   if(!containsNecessaryWord(aBlock)){				  
			       if(!aBlock.contains(DECLARE)){			    	   
				       blockLines.set(1,"this_invocation_nonce");//fix 2nd line
			       } else lineNumber=1;
			       
				   if(blockLines.get(lineNumber).contains(start_method_ENTER))//remove parameters from abstract functions
						   blockLines.set(lineNumber,absfunction+start_method_ENTER);    
				   if(blockLines.get(lineNumber).contains(end_method_EXIT))//remove parameters from abstract functions
					       blockLines.set(lineNumber,absfunction+end_method_EXIT);
				   
				   aBlock = blockListtoString(blockLines);		 
				   newBlocks.add(aBlock);
			   }
		   }
			   
			   		
	   blocks = newBlocks;
	}
//------------------------------------------------------------------------------------------	
	private void process()
	{
		if (merge_methods){
		     renameMethodVariables(start_method);//this -> start_method.this
		     renameMethodVariables(end_method);//this->end_method.this		
		     mergeMethods(start_method,end_method);
		}
		renameMethods(start_method);
		renameMethods(end_method);
		miscParsing();
		syncMethods();
	}
//------------------------------------------------------------------------------------------
	private ArrayList<String> necessaryBlocks()
	{
		ArrayList<String> searchResults = new ArrayList<String>();
		for(int i=0;i<necessaryWords.length;i++){
	       searchResults.addAll(findAllBlocksContaining(necessaryWords[i]));
		}
		return searchResults;
	}
//------------------------------------------------------------------------------------------	
	private boolean containsNecessaryWord(String aBlock)
	{
			for (int i=0;i<necessaryWords.length;i++){
				if(aBlock.contains(necessaryWords[i]))
					return true;
			}
		return false;	
	}
//------------------------------------------------------------------------------------------
	private void deleteBlocks()//removes all unnecessary blocks in the trace.
	{
	   ArrayList<String> temp = new ArrayList<String>();

	   for(java.util.Iterator<String> it = blocks.iterator();it.hasNext();){
		   String aBlock = it.next();
		   if (aBlock.contains(start_method)&&aBlock.contains(start_method_ENTER) || 
		       aBlock.contains(end_method) && aBlock.contains(end_method_EXIT) || 
		       containsNecessaryWord(aBlock)){
			          aBlock = aBlock.replace("# isParam=true","");
			          aBlock = aBlock.replace("# isParam=false","");			   
			          temp.add(aBlock);			    
		        }	
	   }
	    
	 blocks=temp;	
	}
//------------------------------------------------------------------------------------------	
	private void modifyBlocks()
	{
	   ArrayList<String> temp = new ArrayList<String>();

	   for(java.util.Iterator<String> it = blocks.iterator();it.hasNext();){
		   String aBlock = it.next();
		             aBlock = aBlock.replace("# isParam=true","");
			          aBlock = aBlock.replace("# isParam=false","");			   
			          temp.add(aBlock);			     
	  }		   	    
	 blocks=temp;	
	}

//------------------------------------------------------------------------------------------;
	AutoAbstractParser(String _dtrace,String _outText,
			           String _absfunction,String _sENTER, 
			           String _start_method,String _sEXIT, 
			           String _eENTER, String _end_method,String _eEXIT,
		               String _merge_methods)
{
 
	     absfunction  = _absfunction; 		
	     start_method = _start_method;
	     end_method   = _end_method;
	     
	     start_method_ENTER = ":::"+_sENTER;
	     end_method_ENTER = ":::"+_eENTER;
	     
	     start_method_EXIT = ":::"+_sEXIT;
	     end_method_EXIT = ":::"+_eEXIT;
	     
	     
	     merge_methods =  (_merge_methods.toLowerCase().contains("true"));
	
		 blocks = new ArrayList<String>();	    	    
		 dtracefile_input=_dtrace;	
		 dtracefile_output=_outText;
		 
		 inputTrace();
		 
		 //when we merge methods, we do not want to delete blocks. we may need them later.
		 //otherwise, delete all unnecessary blocks.
		 if(merge_methods)
		 modifyBlocks(); else
			 deleteBlocks();
		 process();
		 outputTrace();
		 		 
}
	
//------------------------------------------------------------------------------------------	
public static void main(String[] args) {
		AutoAbstractParser a = new AutoAbstractParser(args[0],args[1],args[2],args[3],args[4],args[5],args[6],args[7],args[8],args[9]);
		
}

}
