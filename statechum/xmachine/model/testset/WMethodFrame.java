package statechum.xmachine.model.testset;

import java.util.*;
import java.util.List;
import java.awt.*;

import javax.swing.*;

import statechum.JUConstants;


import edu.uci.ics.jung.graph.impl.*;
import edu.uci.ics.jung.graph.*;

public class WMethodFrame extends JFrame {
	
	
	public WMethodFrame(DirectedSparseGraph g){
		super();
		this.setTitle("Test set information");
		this.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
		
		setUpJTabbedPane(g);
		this.setSize(600, 400);
		this.setVisible(true);
	}
	
	private void setUpJTabbedPane(DirectedSparseGraph g){
		JTabbedPane pane = new JTabbedPane();
		int numberOfExtraStates = Integer.valueOf(JOptionPane.showInputDialog(
                this.getParent(),
                "Number of extra states","0")).intValue();
		WMethod wm = new WMethod(g, numberOfExtraStates);
		Set<List<String>> transitionCover = wm.getTransitionCover();
		Set<List<String>> characterisationSet = wm.getCharacterisationSet();
		Set<List<String>> fullTestSet = wm.getFullTestSet();
		JComponent transitionCoverPanel = makeTextPanel(transitionCover);
		JComponent characteisationPanel = makeTextPanel(characterisationSet);
		JComponent fullTestSetPanel = makeTextPanel(fullTestSet);
		JComponent statsTab = makeStatsTab(transitionCover, characterisationSet, fullTestSet);
		pane.addTab("Test Set Statistics", statsTab);
		pane.addTab("Transition Cover", transitionCoverPanel);
		pane.addTab("Characerisation Set", characteisationPanel);
		pane.addTab("Full Test Set", fullTestSetPanel);
		add(pane);
		pane.setTabLayoutPolicy(JTabbedPane.SCROLL_TAB_LAYOUT);
	}
	
	private JComponent makeStatsTab(Set<List<String>> transitionCover, Set<List<String>> wSet, Set<List<String>> fullTestSet){
		JTextArea area = new JTextArea();
		JScrollPane scrollPane = new JScrollPane(area);
		area.insert("Transition Cover:\n"+getStats(transitionCover)+"\n", area.getCaretPosition());
		area.insert("Characterisation Set:\n"+getStats(wSet)+"\n", area.getCaretPosition());
		area.insert("Full Test Set:\n"+getStats(fullTestSet)+"\n", area.getCaretPosition());
		JPanel panel = new JPanel(false);
		panel.setLayout(new GridLayout(1,1));
		panel.add(scrollPane, BorderLayout.CENTER);
		return panel;
	}
	
	private String getStats(Set<List<String>> paths){
		String returnString = new String();
		returnString = "size: "+paths.size()+"\n";
		Iterator pathIt = paths.iterator();
		int totalInputs = 0;
		while(pathIt.hasNext()){
			Collection c = (Collection)pathIt.next();
			totalInputs = totalInputs+c.size();
		}
		returnString = returnString + "total number of inputs: "+totalInputs+"\n";
		return returnString;
	}
	
	private JComponent makeTextPanel(Set<List<String>> paths){
		JTextArea area = new JTextArea();
		JScrollPane scrollPane = new JScrollPane(area);
		Iterator pathIt = paths.iterator();
		while(pathIt.hasNext()){
			java.util.List l = (java.util.List)pathIt.next();
			area.insert(listPath(l)+"\n", area.getCaretPosition());
		}
		JPanel panel = new JPanel(false);
		panel.setLayout(new GridLayout(1,1));
		panel.add(scrollPane, BorderLayout.CENTER);
		return panel;
	}
	
	private String listPath(java.util.List list){
		Iterator listIt = list.iterator();
		String path = new String();
		while(listIt.hasNext()){
			Object next = listIt.next();
			if(next instanceof Edge){
				Edge e = (Edge)next;
				path = path.concat(String.valueOf(e.getUserDatum("EDGE"))+", ");
			}
			else{
				String s = (String)next;
				path = path.concat(s+", ");
			}
		}
		return path.trim();
	}

}
