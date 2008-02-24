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

package statechum;

import edu.uci.ics.jung.graph.impl.*;

import javax.swing.*;

import java.awt.event.*;
import java.util.*;
import statechum.xmachine.model.*;
import statechum.xmachine.model.randomMachine.RandomMachineGenerator;
import statechum.xmachine.model.graphReader.*;
import statechum.xmachine.model.testset.*;

public class StateChum extends JFrame implements ActionListener, MouseListener{
	
	private XMachineViewer viewer;
	private Stack panelStack;
	
	public StateChum(XMachineViewer viewer){
		super();
		panelStack = new Stack();
		addMenuBar();
		this.addMouseListener(this);
		this.setViewer(viewer);
		this.setTitle(viewer.getModel().getName()+" "+panelStack.size()+" parents");
		this.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
	}
	
	public StateChum(){
		super();
		panelStack = new Stack();
		addMenuBar();
		this.addMouseListener(this);
		this.setTitle("X-Machine Test and Visualisation Tool");
		this.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
	}
	
	
	private void addMenuBar(){
		JMenuBar menu = new JMenuBar();
		menu.add(createGraphMenu());
		menu.add(createModeMenu());
		menu.add(createViewMenu());
		menu.add(createTestSetMenu());
		this.setJMenuBar(menu);
	}
	
	private JMenu createGraphMenu(){
		JMenu graphMenu = new JMenu("Machine");
		JMenuItem random = new JMenuItem("New Random Machine");
		random.addActionListener(this);
		graphMenu.add(random);
		JMenuItem read = new JMenuItem("Read Machine from File");
		read.addActionListener(this);
		graphMenu.add(read);
		return graphMenu;
	}
	
	private JMenu createTestSetMenu(){
		JMenu testSetMenu = new JMenu("Test Sets");
		JMenuItem wMethod = new JMenuItem("W-Method");
		wMethod.addActionListener(this);
		testSetMenu.add(wMethod);
		return testSetMenu;
	}
	
	private JMenu createModeMenu(){
		JMenu modeMenu = new JMenu("Mode");
		JMenuItem transform = new JMenuItem("Transform");
		transform.addActionListener(this);
		JMenuItem pick = new JMenuItem("Pick");
		pick.addActionListener(this);
		modeMenu.add(transform);
		modeMenu.add(pick);
		return modeMenu;
	}
	
	private JMenu createViewMenu(){
		JMenu viewMenu = new JMenu("View");
		JCheckBoxMenuItem stateLabels = new JCheckBoxMenuItem("Label States");
		stateLabels.setActionCommand("labelstates");
		stateLabels.addActionListener(this);
		stateLabels.setState(true);
		JCheckBoxMenuItem transitionLabels = new JCheckBoxMenuItem("Label Transitions");
		transitionLabels.setActionCommand("labeltransitions");
		transitionLabels.addActionListener(this);
		transitionLabels.setState(true);
		viewMenu.add(stateLabels);
		viewMenu.add(transitionLabels);
		return viewMenu;
	}
	
	private void drillDown(Transition trans, int x, int y){
		if(trans.getName()==null)
			return;
		panelStack.push(viewer.getModel());
		String key;
		if(trans.getFunctions().size()>1){
			key = chooseFunction(trans.getFunctions());
			if(key==null)
				return;
		}
		else{
			Iterator setIt = trans.getFunctions().keySet().iterator();
			key = (String)setIt.next();
		}
		TransitionFunction tf = (TransitionFunction)trans.getFunctions().get(key);
		if((tf instanceof XMachineTransitionFunction)){
			XMachineTransitionFunction xtf = (XMachineTransitionFunction)tf;
			if(xtf.getFunctionMachine()!=null){
				XMachineViewer functionViewer = new XMachineViewer(xtf.getFunctionMachine()
						,viewer.getVertices(),viewer.getEdges(), viewer.isCollapsed());
				setViewer(functionViewer);
			}
			else xtf.printBlocks();
		}
	}
	
	
	private void drillUp(){
		if(panelStack.isEmpty()){
			
			return;
		}
		XMachineModel parent = (XMachineModel)panelStack.pop();
		XMachineViewer functionViewer = new XMachineViewer(parent,viewer.getVertices(),viewer.getEdges(), viewer.isCollapsed());
		setViewer(functionViewer);
	}
	
	private void setViewer(XMachineViewer v){
		if(viewer!=null)
			this.getContentPane().remove(viewer.getPanel());
		viewer = v;
		viewer.getViewer().addMouseListener(this);
		this.getContentPane().add(v.getPanel());
		this.getContentPane().validate();
		this.repaint();
	}
	
	
	
	public void actionPerformed(ActionEvent event){
		if(event.getActionCommand().equals("Transform")&&viewer!=null)
			viewer.setTransformable();
		
		else if(event.getActionCommand().equals("Pick")&&viewer!=null)
			viewer.setPickable();
		
		else if(event.getActionCommand().equals("labelstates")&&viewer!=null){
			JCheckBoxMenuItem item = (JCheckBoxMenuItem)event.getSource();
			this.setViewer(new XMachineViewer(viewer.getModel(),item.getState(), viewer.getEdges(), viewer.isCollapsed()));
		}
		else if(event.getActionCommand().equals("labeltransitions")&&viewer!=null){
			JCheckBoxMenuItem item = (JCheckBoxMenuItem)event.getSource();
			setViewer(new XMachineViewer(viewer.getModel(),viewer.getVertices(), item.getState(), viewer.isCollapsed()));
		}
		else if(event.getActionCommand().equals("New Random Machine"))
			this.randomMachine();
		else if(event.getActionCommand().equals("Read Machine from File"))
			this.readMachine();
		else if(event.getActionCommand().equals("W-Method"))
			wMethod();
	}
	
	private void wMethod(){
		WMethodFrame wmf = new WMethodFrame((DirectedSparseGraph)viewer.getModel().getAttachedGraph());
		
	}
	
	private void randomMachine(){
        RandomMachineGenerator rmg = new RandomMachineGenerator();
        XMachineModel machine = rmg.generateGraph();
        XMachineViewer viewer = new XMachineViewer(machine, true, true,false);
  	  	this.setViewer(viewer);
	}
	
	private void readMachine(){
		GraphReader gr = new GraphReader();
		 XMachineModel machine = gr.readGraph(this);
	     XMachineViewer viewer = new XMachineViewer(machine, true, true,false);
	  	 this.setViewer(viewer);
	}
	
	public void mouseReleased(MouseEvent event){
		if(viewer == null)
			return;
		if(viewer.getPickedEdge()==null)
			return;
		Transition trans = viewer.getPickedEdge();
		if(event.getButton() == MouseEvent.BUTTON1)
			drillDown(trans, event.getX(),event.getY());
		else if(event.getButton() == MouseEvent.BUTTON2)
			drillUp();
		
	}
	
	public void mouseExited(MouseEvent event){
			
	}
	
	public void mousePressed(MouseEvent event){
		
	}
	public void mouseClicked(MouseEvent event){
		
	}
	public void mouseEntered(MouseEvent event){
		
	}
	
	private String chooseFunction(HashMap map){
		Object[] possibilities = map.keySet().toArray();
		String s = (String)JOptionPane.showInputDialog(
		                    this.getParent(),
		                    "Choose function to explore",
		                    "Multiple Functions",
		                    JOptionPane.PLAIN_MESSAGE,
		                    null,
		                    possibilities,
		                    null);

		return s;
	}
	
	public static void main(String[] args){
		JFrame jf = new StateChum();
		jf.setSize(800,600);
		jf.setVisible(true);
	}

}
