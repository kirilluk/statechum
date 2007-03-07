package statechum.analysis.learning.profileStringExtractor;

import java.awt.BorderLayout;
import java.awt.event.*;
import java.io.*;
import java.util.*;
import statechum.analysis.learning.*;
import javax.xml.parsers.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;
import javax.xml.parsers.SAXParser;

public class AbstractFunctionFrame extends JFrame implements ActionListener{

	private HashMap namesToMethods, filesToHandlers;
	private JList names, methods;
	private List nameList;
	private SplitFrame split;
	private static final long serialVersionUID = 1L;


	/**
	 * This is the default constructor
	 */
	public AbstractFunctionFrame(HashMap filesToHandlers, SplitFrame reference) {
		super();
		this.split = reference;
		this.filesToHandlers = filesToHandlers;
		initialize();
	}
	
	public void writeToFile(String fileName){
		try {
			
			StorableFile sf = new StorableFile(namesToMethods);
		    FileOutputStream fo = new FileOutputStream(fileName);
		    ObjectOutputStream so = new ObjectOutputStream(fo);
		    so.writeObject(sf);
		    so.flush();
		    so.close();
		} catch (Exception e) {
		    e.printStackTrace();
		    System.exit(1);
		}
	}
	
	public void readFromFile(File f){
		try {
		    FileInputStream fi = new FileInputStream(f);
		    ObjectInputStream si = new ObjectInputStream(fi);  
		    StorableFile sf = (StorableFile) si.readObject();
		    this.namesToMethods = sf.getNamesToMethods();
		    updateContents();
	        si.close();
		} catch (Exception e) {
		    e.printStackTrace();
		    System.exit(1);
		}
	}

	/**
	 * This method initializes this
	 * 
	 * @return void
	 */
	private void initialize() {
		this.namesToMethods = new HashMap();
		nameList = new ArrayList();
		this.setLocation(0, 700);
		this.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
		this.setContentPane(getJContentPane());
		this.setTitle("Abstract Functions");
		this.setResizable(false);
		this.pack();
		this.setSize(600, 300);
		this.setVisible(true);
	}
	
	public void addAbstractFunction(List methods){
		String name = (String)JOptionPane.showInputDialog(
                this, "Enter name of abstract function",
                "Customized Dialog",JOptionPane.PLAIN_MESSAGE,
                null,null,null);
		this.namesToMethods.put(name, methods);
		updateContents();
	}
	
	private void updateContents(){
		nameList.clear();
		nameList.addAll(namesToMethods.keySet());
		names.setListData(nameList.toArray());
		update(getGraphics());
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

	/**
	 * This method initializes jSplitPane	
	 * 	
	 * @return javax.swing.JSplitPane	
	 */
	private JSplitPane getJSplitPane() {
		JSplitPane splitPane = new JSplitPane();
		splitPane = new JSplitPane();
		splitPane.setDividerLocation(300);
		splitPane.setRightComponent(getScrollPane());
		splitPane.setLeftComponent(getLeftScrollPane());
		return splitPane;
	}

	/**
	 * This method initializes jScrollPane	
	 * 	
	 * @return javax.swing.JScrollPane	
	 */
	private JScrollPane getScrollPane() {
		JScrollPane scrollPane = new JScrollPane();
		scrollPane = new JScrollPane();
		methods = new JList();
		scrollPane.setViewportView(methods);
		return scrollPane;
	}

	/**
	 * This method initializes methodList	
	 * 	
	 * @return javax.swing.JScrollPane	
	 */
	private JPanel getLeftScrollPane() {
		JPanel panel = new JPanel();
		panel.setLayout(new BoxLayout(panel, BoxLayout.PAGE_AXIS));
		JScrollPane scrollPane = new JScrollPane();
		scrollPane = new JScrollPane();
		names = new JList();
		ListSelectionHandler handler = new ListSelectionHandler();
		names.addListSelectionListener(handler);
		scrollPane.setViewportView(names);
		panel.add(scrollPane);
		JPanel buttonPanel = new JPanel();
		buttonPanel.setLayout(new BoxLayout(buttonPanel, BoxLayout.LINE_AXIS));
		JButton remove = new JButton("Remove");
		remove.addActionListener(this);
		buttonPanel.add(remove);
		JButton extractStrings = new JButton("Infer Machine from Traces");
		extractStrings.addActionListener(this);
		buttonPanel.add(extractStrings);
		panel.add(buttonPanel);
		return panel;
	}

	
	public void actionPerformed(ActionEvent e){
		if(e.getActionCommand().equals("Remove")){
			String name = names.getSelectedValue().toString();
			namesToMethods.remove(name);
			methods.setListData(new Vector());
			updateContents();
		}
		else if(e.getActionCommand().equals("Infer Machine from Traces")){
			SAXParserFactory factory = SAXParserFactory.newInstance();
			HashSet sPlus = new HashSet();
			try{
				if(filesToHandlers == null){
					JOptionPane.showMessageDialog(this, "No traces loaded");
					return;
				}
				Object[] files = filesToHandlers.keySet().toArray();
				for(int i=0;i<files.length;i++){
					SAXParser parser = factory.newSAXParser();
					
					StackHandler stackHandler = new StackHandler(namesToMethods, (ClassMethodDefsHandler)filesToHandlers.get(files[i]));
					parser.parse((File)files[i], stackHandler);
					System.out.println(stackHandler.getFunctionString(3));
					sPlus.add(stackHandler.getArrayListFunctionString(3));
				}
				Visualiser v = new Visualiser(sPlus, new HashSet(), split);
			}
			catch(Exception ex){
				ex.printStackTrace();
				return;
			}
		}
	}
	
	public Set<String[]> getStrings(Set sPlus){
		SAXParserFactory factory = SAXParserFactory.newInstance();
		try{
			Object[] files = filesToHandlers.keySet().toArray();
			for(int i=0;i<files.length;i++){
				SAXParser parser = factory.newSAXParser();
				
				StackHandler stackHandler = new StackHandler(namesToMethods, (ClassMethodDefsHandler)filesToHandlers.get(files[i]));
				parser.parse((File)files[i], stackHandler);
				System.out.println(stackHandler.getFunctionString(3));
				sPlus.add(stackHandler.getArrayListFunctionString(3));
			}
		}
		catch(Exception ex){
			ex.printStackTrace();
		}
		System.out.println();
		return sPlus;
	}
	
	private String[] pathToStrings(List list){
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

	
	class ListSelectionHandler implements ListSelectionListener {
		public void valueChanged(ListSelectionEvent e) { 
			Object selected = names.getSelectedValue();
			if(selected == null)
				return;
			List functionMethods = (List)namesToMethods.get(selected);
			methods.setListData(pathToStrings(functionMethods));
			update(getGraphics());
		}
	}


	public HashMap getFilesToHandlers() {
		return filesToHandlers;
	}
	
	public void setFilesToHandlers(HashMap filesToHandlers){
		this.filesToHandlers = filesToHandlers;
	}


}
