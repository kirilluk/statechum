/* Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 * 
 * This file is part of StateChum
 * 
 * StateChum is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * StateChum is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with StateChum.  If not, see <http://www.gnu.org/licenses/>.
 */ 

package statechum.analysis.learning.oracles;

import java.io.File;
import java.util.*;

import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JOptionPane;

import soot.EntryPoints;
import soot.G;
import soot.Pack;
import soot.Scene;
import soot.SootClass;
import soot.options.Options;

public class SootCallGraphManager extends JFrame {
	
	/**
	 * ID for serialisation.
	 */
	private static final long serialVersionUID = -7064964842501276326L;
	private File packageRoot;
	private Collection classes;
	
	public SootCallGraphManager (){
		init();
	}
	
	public  void init(){
		classes = new HashSet();
		ArrayList<String> dynamicClasses = new ArrayList<String>();
		dynamicClasses.add("CH.ifa.draw.util.collections.jdk12.CollectionsFactoryJDK12");
		Options.v().set_whole_program(true);
		Options.v().set_app(true);
		Options.v().set_dynamic_class(dynamicClasses);
		Options.v().setPhaseOption("cg", "verbose:true");
		Options.v().setPhaseOption("cg.spark", "enabled:true");
		Options.v().setPhaseOption("cg.spark", "rta:false");
		Options.v().setPhaseOption("cg", "all-reachable:true");
		Options.v().setPhaseOption("cg", "safe-newinstance:true");

		selectLibraries();
		selectPackageRoot();
		selectMainClass();
		buildCallGraph();
	}
	
	private  void setApplicationClasses(){
		Iterator classIt = classes.iterator();
		while(classIt.hasNext()){
			String className = classIt.next().toString();
			SootClass current = Scene.v().getSootClass(className);
			current.setApplicationClass();
		}
	}
	
	private void buildCallGraph(){
		setApplicationClasses();
		Scene.v().setEntryPoints(EntryPoints.v().all());
		Pack cg = G.v().soot_PackManager().getPack("cg");
		cg.apply();
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
		else
			System.exit(0);
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
            	e.printStackTrace();
            	System.exit(0);
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
		else
			System.exit(0);
	}

}
