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

package statechum.analysis.learning.profileStringExtractor;

import java.awt.BorderLayout;
import java.awt.event.*;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.*;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

public class SplitFrame extends JFrame implements ActionListener{
	private List<TreePath> list; 
	private JList methodList;
	private JScrollPane treePanel;
	private AbstractFunctionFrame frame;
	private Map<File,ClassMethodDefsHandler> filesToHandlers;

	private static final long serialVersionUID = 1L;


	/**
	 * This is the default constructor
	 */
	public SplitFrame() {
		super();
		initialize();
	}

	/**
	 * This method initializes this
	 * 
	 * @return void
	 */
	private void initialize() {
		list = new ArrayList<TreePath>();
		frame = new AbstractFunctionFrame(filesToHandlers, this);
		this.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		this.setContentPane(getJContentPane());
		this.setTitle("Trace Methods");
		this.setResizable(true);
		this.pack();
		this.setSize(800, 600);
		this.setVisible(true);
	}

	/**
	 * This method initializes jContentPane
	 * 
	 * @return javax.swing.JPanel
	 */
	private JPanel getJContentPane() {
		JPanel contentPanel = new JPanel();
		contentPanel = new JPanel();
		contentPanel.setLayout(new BorderLayout());
		contentPanel.add(getJSplitPane(), BorderLayout.CENTER);
		return contentPanel;
	}

	/** This method executes a pre-set series of operations to permit loading files and running 
	 * experiments without having to manually click buttons on the interface.
	 * 
	 *  @param args command-line args passed to this instance of an application.
	 */
	public void runAnExperiment(String [] args)
	{
		String resourcesDir = System.getProperty("user.dir")+"/resources/";
		File[] traceFiles = new File[] {new File(resourcesDir+"part1.xml"),new File(resourcesDir+"part2.xml"),new File(resourcesDir+"part3.xml"),new File(resourcesDir+"part4.xml"),new File(resourcesDir+"part5.xml")};
		Extractor ex = new Extractor(traceFiles);
		JTree methodTree = ex.getTree();
		this.setTree(methodTree);
		frame.setFilesToHandlers(ex.getFileToHandler());
		frame.readFromFile(new File(resourcesDir+"jhotdraw"));
		if (args.length > 1)
		{
				try {
					frame.loadAnswers(new File(resourcesDir+args[1]));
				} catch (Exception e) {
					e.printStackTrace();
				}
		}
		frame.actionPerformed(new ActionEvent(this,ActionEvent.ACTION_PERFORMED,AbstractFunctionFrame.buttonInferMachine));
	}
	
	/**
	 * This method initializes jSplitPane	
	 * 	
	 * @return javax.swing.JSplitPane	
	 */
	private JSplitPane getJSplitPane() {
		JSplitPane splitPane = new JSplitPane();
		splitPane = new JSplitPane();
		splitPane.setDividerLocation(300);
		splitPane.setRightComponent(getMethodList());
		splitPane.setLeftComponent(getTreeScrollPane());
		return splitPane;
	}

	/**
	 * This method initializes jScrollPane	
	 * 	
	 * @return javax.swing.JScrollPane	
	 */
	private JScrollPane getTreeScrollPane() {
		treePanel = new JScrollPane();
		return treePanel;
	}
	
	private void setTree(JTree methodTree){
		methodTree.addTreeSelectionListener(new TreeSelectionHandler());
		treePanel.setViewportView(methodTree);
	}

	/**
	 * This method initializes methodList	
	 * 	
	 * @return javax.swing.JScrollPane	
	 */
	private JPanel getMethodList() {
		JPanel panel = new JPanel();
		panel.setLayout(new BoxLayout(panel, BoxLayout.PAGE_AXIS));
		JScrollPane scrollPane = new JScrollPane();
		scrollPane = new JScrollPane();
		scrollPane.setViewportView(getJList());
		panel.add(scrollPane);
		JPanel buttonPanel = new JPanel();
		buttonPanel.setLayout(new BoxLayout(buttonPanel, BoxLayout.LINE_AXIS));
		JButton remove = new JButton("Remove");
		remove.addActionListener(this);
		JButton asFunction = new JButton("Abstract Function");
		asFunction.addActionListener(this);
		JButton load = new JButton("Load");
		load.addActionListener(this);
		JButton save = new JButton("Save");
		save.addActionListener(this);
		JButton addTraces = new JButton("Set Traces");
		addTraces.addActionListener(this);
		buttonPanel.add(remove);
		buttonPanel.add(asFunction);
		buttonPanel.add(addTraces);
		buttonPanel.add(load);
		buttonPanel.add(save);
		panel.add(buttonPanel);
		return panel;
	}

	/**
	 * This method initializes jList	
	 * 	
	 * @return javax.swing.JList	
	 */
	private JList getJList() {
		methodList = new JList();
		return methodList;
	}
	
	private String[] pathToStrings(){
		Iterator<TreePath> listIt = list.iterator();
		String[] returnArray = new String[list.size()];
		for(int i=0;i<list.size();i++){
			TreePath current = listIt.next();
			String pathString = new String();
			for(int j=1;j<current.getPathCount();j++){
				if(j<current.getPathCount()-1)
					pathString = pathString.concat(current.getPathComponent(j).toString()+".");
				else
					pathString = pathString.concat(current.getPathComponent(j).toString());
			}
			
			returnArray[i]=pathString;
		}
		return returnArray;
	}
	
	public void actionPerformed(ActionEvent e){
		if(e.getActionCommand().equals("Remove")){
			int[] selected = methodList.getSelectedIndices();
			Vector<TreePath> removeValues = new Vector<TreePath>();
			for(int i=0;i<selected.length;i++){
				removeValues.add(list.get(selected[i]));
			}
			list.removeAll(removeValues);
			methodList.setListData(pathToStrings());
		}
		else if(e.getActionCommand().equals("Abstract Function")){
			abstractFrame();
		}
		else if(e.getActionCommand().equals("Save")){
			String name = (String)JOptionPane.showInputDialog(
	                this, "Enter file name",
	                "Customized Dialog",JOptionPane.PLAIN_MESSAGE,
	                null,null,null);
			frame.writeToFile(name);
		}
		else if(e.getActionCommand().equals("Load")){
			JFileChooser fc = new JFileChooser();
			fc.setCurrentDirectory(new File(System.getProperty("user.dir")));
			fc.setMultiSelectionEnabled(false);
			fc.setFileSelectionMode(JFileChooser.FILES_ONLY);
			int choice = fc.showDialog(this, "Select File");
			if(choice == JFileChooser.APPROVE_OPTION){
				File file = new File(fc.getSelectedFile().getPath());
				System.out.println(file.getPath());
				frame.readFromFile(file);
			}
		}
		else if(e.getActionCommand().equals("Set Traces")){
			JFileChooser fc = new JFileChooser();
			fc.setCurrentDirectory(new File(System.getProperty("user.dir")));
			fc.setMultiSelectionEnabled(true);
			fc.setFileSelectionMode(JFileChooser.FILES_ONLY);
			int choice = fc.showDialog(frame, "Select XML files");
			if(choice == JFileChooser.APPROVE_OPTION){
				File[] file = fc.getSelectedFiles();
				Extractor ex = new Extractor(file);
				JTree methodTree = ex.getTree();
				this.setTree(methodTree);
				frame.setFilesToHandlers(ex.getFileToHandler());
			}
		}
	}
	
	public Set<String[]> addTest(Set sPlus){
		Set<String[]> set = null;
		JFileChooser fc = new JFileChooser();
		fc.setMultiSelectionEnabled(false);
		fc.setFileSelectionMode(JFileChooser.FILES_ONLY);
		int choice = fc.showDialog(this, "Select File");
		if(choice == JFileChooser.APPROVE_OPTION){
			File file = fc.getSelectedFile();
			Set<File> files = new HashSet<File>();
			files.add(file);
			files.addAll(filesToHandlers.keySet());
			Extractor extractor = new Extractor((File[])files.toArray());
			this.filesToHandlers = extractor.getFileToHandler();
			JTree methodTree = extractor.getTree();
			setTree(methodTree);
			frame.dispose();
			frame = new AbstractFunctionFrame(filesToHandlers, this);
			set=frame.getStrings(sPlus);
		}
		else if(choice == JFileChooser.CANCEL_OPTION)
			return null;
		return set;
	}
	
	private void abstractFrame(){
		List<TreePath> abstractFunction = new ArrayList<TreePath>();
		abstractFunction.addAll(list);
		frame.addAbstractFunction(abstractFunction);
		list.clear();
		methodList.setListData(pathToStrings());
	}
	
	public void dispose(){
		super.dispose();
		frame.dispose();
	}
	
	class TreeSelectionHandler implements TreeSelectionListener {
		public void valueChanged(TreeSelectionEvent e) { 
			TreePath path = e.getPath();
			DefaultMutableTreeNode lastComponent = (DefaultMutableTreeNode) path.getLastPathComponent();
			if(lastComponent.getChildCount()>0||!e.isAddedPath())
				return;
			addPathToMethods(path);
			update(getGraphics());
		}
		
		private void addPathToMethods(TreePath path){
			list.add(path);
			methodList.setListData(pathToStrings());
		}
		
		
    }


}
