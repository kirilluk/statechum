package statechum.analysis.learning.profileStringExtractor;

import java.awt.BorderLayout;
import java.awt.event.*;
import java.io.File;
import java.util.*;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

public class SplitFrame extends JFrame implements ActionListener{
	private List list; 
	private JList methodList;
	private AbstractFunctionFrame frame;

	private static final long serialVersionUID = 1L;


	/**
	 * This is the default constructor
	 */
	public SplitFrame(JTree methodTree, HashMap filesToHandlers) {
		super();
		initialize(methodTree, filesToHandlers);
	}

	/**
	 * This method initializes this
	 * 
	 * @return void
	 */
	private void initialize(JTree methodTree, HashMap filesToHandlers) {
		list = new ArrayList();
		frame = new AbstractFunctionFrame(filesToHandlers);
		this.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		this.setContentPane(getJContentPane(methodTree));
		this.setTitle("Profiled Methods");
		this.setResizable(false);
		this.pack();
		this.setSize(800, 600);
		this.setVisible(true);
	}

	/**
	 * This method initializes jContentPane
	 * 
	 * @return javax.swing.JPanel
	 */
	private JPanel getJContentPane(JTree methodTree) {
		JPanel contentPanel = new JPanel();
		contentPanel = new JPanel();
		contentPanel.setLayout(new BorderLayout());
		contentPanel.add(getJSplitPane(methodTree), BorderLayout.CENTER);
		return contentPanel;
	}

	/**
	 * This method initializes jSplitPane	
	 * 	
	 * @return javax.swing.JSplitPane	
	 */
	private JSplitPane getJSplitPane(JTree methodTree) {
		JSplitPane splitPane = new JSplitPane();
		splitPane = new JSplitPane();
		splitPane.setDividerLocation(300);
		splitPane.setRightComponent(getMethodList());
		splitPane.setLeftComponent(getTreeScrollPane(methodTree));
		return splitPane;
	}

	/**
	 * This method initializes jScrollPane	
	 * 	
	 * @return javax.swing.JScrollPane	
	 */
	private JScrollPane getTreeScrollPane(JTree methodTree) {
		JScrollPane scrollPane = new JScrollPane();
		scrollPane = new JScrollPane();
		methodTree.addTreeSelectionListener(new TreeSelectionHandler());
		scrollPane.setViewportView(methodTree);
		return scrollPane;
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
		buttonPanel.add(remove);
		buttonPanel.add(asFunction);
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
			Vector removeValues = new Vector();
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
			fc.setMultiSelectionEnabled(false);
			fc.setFileSelectionMode(JFileChooser.FILES_ONLY);
			int choice = fc.showDialog(this, "Select File");
			if(choice == JFileChooser.APPROVE_OPTION){
				File file = fc.getSelectedFile();
				frame.readFromFile(file);
				
			}
		}
	}
	
	private void abstractFrame(){
		ArrayList abstractFunction = new ArrayList();
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
