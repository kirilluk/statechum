package statechum.analysis.learning.profileStringExtractor;

import java.util.*;
import java.io.*;

import javax.xml.parsers.*;
import javax.swing.tree.*;
import javax.swing.*;

public class Extractor {
	
	private Map<File, ClassMethodDefsHandler> fileToHandler;
	
	public Extractor(File[] xmlFiles){
		SAXParserFactory factory = SAXParserFactory.newInstance();
		try{
			SAXParser parser = factory.newSAXParser();
			parser.setProperty("http://xml.org/sax/features/validation", false);

			process(parser, xmlFiles);
		}
		catch(Exception e){
			System.out.println(e);
			return;
		}
	}

	private void process(SAXParser parser, File[] xmlFiles){
		fileToHandler = new HashMap<File, ClassMethodDefsHandler>();
		ClassMethodDefsHandler classDefsHandler = null;
		try{
			for(int i = 0;i<xmlFiles.length;i++){
				String workingDir = System.getProperty("user.dir");
				String filePath = xmlFiles[i].getPath();
				filePath = filePath.substring(workingDir.length());
				File f = new File(filePath);
				System.out.println(filePath);
				classDefsHandler = new ClassMethodDefsHandler(f);
				fileToHandler.put(xmlFiles[i], classDefsHandler);
				parser.parse(xmlFiles[i], classDefsHandler);
			}
		}
		catch(Exception e){System.out.println(e);}
	}
	
	public Map<File, ClassMethodDefsHandler> getFileToHandler(){
		return fileToHandler;
	}
	
	/** Builds a tree from classes/methods stored in the supplied XML file. */
	public JTree getTree(){
		DefaultMutableTreeNode root = new DefaultMutableTreeNode();
		Iterator handlerIt = fileToHandler.values().iterator();
		while(handlerIt.hasNext()){
			ClassMethodDefsHandler current = (ClassMethodDefsHandler)handlerIt.next();
			Map<Integer,Set<Integer>> classesToMethods = current.getClassesToMethods();
			Map<Integer,String> methodDefs = current.getMethodDefs();
			Map<Integer,String> classDefs = current.getClassDefs();
			Iterator<Integer> methodKeyIt = classesToMethods.keySet().iterator();
			while(methodKeyIt.hasNext()){
				Integer classId = methodKeyIt.next();
				String className = classDefs.get(classId).toString();
				DefaultMutableTreeNode classNameNode = getNodeForClassName(className, root);
				root = (DefaultMutableTreeNode)classNameNode.getRoot();
				Set<Integer> methods = classesToMethods.get(classId);
				Iterator<Integer> methodIt = methods.iterator();
				while(methodIt.hasNext()){
					Integer methodId = methodIt.next();
					String methodName = methodDefs.get(methodId).toString();
					if(!hasChildWithName(classNameNode, methodName)){
						DefaultMutableTreeNode methodNode = new DefaultMutableTreeNode(methodName);
						classNameNode.add(methodNode);
					}
				}
			}
		}
		return new JTree(root);
	}
	
	private static boolean hasChildWithName(DefaultMutableTreeNode node, String name){
		for(int i=0;i<node.getChildCount();i++){
			DefaultMutableTreeNode current = (DefaultMutableTreeNode)node.getChildAt(i);
			if(current.getUserObject().toString().equals(name)){
				return true;
			}
		}
		return false;
	}
	
	private static DefaultMutableTreeNode getNodeForClassName(String className, DefaultMutableTreeNode root){
		StringTokenizer tokenizer = new StringTokenizer(className, ".");
		DefaultMutableTreeNode currentNode = root;
		while(tokenizer.hasMoreTokens()){
			String token = tokenizer.nextToken();
			boolean foundToken=false;
			for(int i=0;i<currentNode.getChildCount();i++){
				DefaultMutableTreeNode node = (DefaultMutableTreeNode)currentNode.getChildAt(i);
				if(node.getUserObject().toString().equals(token)){
					currentNode = node;
					foundToken = true;
					continue;
				}
			}
			if(!foundToken){
				DefaultMutableTreeNode newNode = new DefaultMutableTreeNode(token);
				currentNode.add(newNode);
				currentNode = newNode;
			}
		}
		return currentNode;
	}
}
