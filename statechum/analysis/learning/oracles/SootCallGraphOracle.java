package statechum.analysis.learning.oracles;

import java.io.File;
import java.util.*;

import javax.swing.*;

import soot.*;
import soot.options.*;
import soot.util.*;
import soot.jimple.*;
import soot.jimple.spark.*;

import statechum.analysis.learning.RPNIBlueFringeLearner;


public class SootCallGraphOracle extends JFrame implements AbstractOracle {
	
	private File packageRoot;
	private Collection classes;
	
	public SootCallGraphOracle(){
		classes = new HashSet();
		init();
	}
	
	private void init(){
		Options.v().set_whole_program(true);
		Options.v().setPhaseOption("cg.spark", "enabled:true");
		Options.v().setPhaseOption("cg.spark", "vta:false");
		selectLibraries();
		selectPackageRoot();
		selectMainClass();
		buildCallGraph();
	}
	
	private void setApplicationClasses(){
		Iterator classIt = classes.iterator();
		while(classIt.hasNext()){
			String className = classIt.next().toString();
			SootClass current = Scene.v().getSootClass(className);
			current.setApplicationClass();
		}
	}
	
	private void buildCallGraph(){
		
		setApplicationClasses();
		Transform sparkTransform = PackManager.v().getTransform("cg.spark");
		sparkTransform.apply();
		System.out.println(Scene.v().getCallGraph().size());
		System.out.println("done");
	}
	
	private void selectPackageRoot(){
		JFileChooser fc = new JFileChooser();
		fc.setMultiSelectionEnabled(false);
		fc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
		int choice = fc.showDialog(this, "Select package root directory");
		if(choice == JFileChooser.APPROVE_OPTION){
			File directory = fc.getSelectedFile();
			packageRoot = directory;
			String sootClassPath=Scene.v().getSootClassPath();
			Scene.v().setSootClassPath( sootClassPath + System.getProperty("path.separator") + packageRoot.getPath());
			addAllSubRoot(packageRoot);
		}
		Scene.v().loadNecessaryClasses();
	}
	
	private void selectMainClass(){
		String name = (String)JOptionPane.showInputDialog(
                this, "Enter name of main class",
                "Customized Dialog",JOptionPane.PLAIN_MESSAGE,
                null,null,null);
		try{
			SootClass mainClass = Scene.v().getSootClass(name);
			Scene.v().setMainClass(mainClass);
		}
		catch(Exception e){
			e.printStackTrace();
			selectMainClass();
		}
	}
	
	/**
	 * recursively adds all files that belong to subdirectories of the root
	 * directory as analysis files
	 */ 
	private void addAllSubRoot(File dir){
        if (dir.isDirectory()) {
            String[] children = dir.list();
            for (int i=0; i<children.length; i++) {
            	addAllSubRoot(new File(dir, children[i]));
            }
        } else {
        	File currentFile[] = new File[1];
        	currentFile[0]=dir;
        	populateClasses(currentFile);
        }
	    
	}
	
	private void populateClasses(File[] analysisFiles){
		String fileName = null;
		String packagePath = null;
		for(int i = 0;i<analysisFiles.length;i++){
			File f = analysisFiles[i];
            String rootPath = packageRoot.getPath();
            packagePath = f.getPath().toString().substring(rootPath.length()+1);
            packagePath = packagePath.replace(File.separatorChar,'.');
            fileName = (packagePath.substring(0,packagePath.length()-6));
            if(!(packagePath.substring(packagePath.length()-6, packagePath.length()).equals(".class")))
            	continue;
            try
            {
            	Scene.v().addBasicClass(fileName, SootClass.BODIES);
            	classes.add(fileName);
            	
            }
            catch ( Exception e )
            {
            	System.out.println("warning: could not load: "+fileName+", check libraries are specified");
            }
		}
		
	}
	
	private void populateLibraries(File[] libraryFiles){
		for(int i = 0;i<libraryFiles.length;i++){
			String sootClassPath=Scene.v().getSootClassPath();
			File f = libraryFiles[i];
            try
            {
            	Scene.v().setSootClassPath( sootClassPath + System.getProperty("path.separator") + f.getPath());
            }
            catch ( RuntimeException e )
            {
            	System.out.println(e);
            	String title = "File Path Problem";
    			String message = "Files could not be loaded";
    			JOptionPane optionPane = new JOptionPane(
    					message, JOptionPane.INFORMATION_MESSAGE);
    			JDialog dialog = optionPane.createDialog(this, title);
    			dialog.setVisible(true);
    			return;
            }
		}
	}
	
	private void selectLibraries(){
		JFileChooser fc = new JFileChooser();
		fc.setMultiSelectionEnabled(true);
		fc.setFileSelectionMode(JFileChooser.FILES_ONLY);
		if(packageRoot!=null)
			fc.setCurrentDirectory(packageRoot);
		int choice = fc.showDialog(this, "Select libraries");
		if(choice == JFileChooser.APPROVE_OPTION){
			File[] libraryFiles;
			libraryFiles = fc.getSelectedFiles();
			populateLibraries(libraryFiles);
		}
	}

	public int getAnswer(List<String> question) {
		return RPNIBlueFringeLearner.USER_CANCELLED;
	}

}
