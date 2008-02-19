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

package statechum.xmachine.model.testset;

import java.util.*;
import java.util.List;
import java.awt.*;

import javax.swing.*;

import statechum.JUConstants;

import edu.uci.ics.jung.graph.impl.*;
import edu.uci.ics.jung.graph.*;

public class WMethodFrame extends JFrame {

	public WMethodFrame(DirectedSparseGraph g) {
		super();
		this.setTitle("Test set information");
		this.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

		setUpJTabbedPane(g);
		this.setSize(600, 400);
		this.setVisible(true);
	}

	private void setUpJTabbedPane(DirectedSparseGraph g) {
		JTabbedPane pane = new JTabbedPane();
		int numberOfExtraStates = Integer.valueOf(
				JOptionPane.showInputDialog(this.getParent(),
						"Number of extra states", "0")).intValue();
		WMethod wm = new WMethod(g, numberOfExtraStates);
		Collection<List<String>> transitionCover = wm.getTransitionCover();
		Collection<List<String>> characterisationSet = wm
				.getCharacterisationSet();
		Collection<List<String>> fullTestSet = wm.getFullTestSet();
		JComponent transitionCoverPanel = makeTextPanel(transitionCover);
		JComponent characteisationPanel = makeTextPanel(characterisationSet);
		JComponent fullTestSetPanel = makeTextPanel(fullTestSet);
		JComponent statsTab = makeStatsTab(transitionCover,
				characterisationSet, fullTestSet);
		pane.addTab("Test Set Statistics", statsTab);
		pane.addTab("Transition Cover", transitionCoverPanel);
		pane.addTab("Characerisation Set", characteisationPanel);
		pane.addTab("Full Test Set", fullTestSetPanel);
		add(pane);
		pane.setTabLayoutPolicy(JTabbedPane.SCROLL_TAB_LAYOUT);
	}

	private JComponent makeStatsTab(Collection<List<String>> transitionCover,
			Collection<List<String>> wSet, Collection<List<String>> fullTestSet) {
		JTextArea area = new JTextArea();
		JScrollPane scrollPane = new JScrollPane(area);
		area.insert("Transition Cover:\n" + getStats(transitionCover) + "\n",
				area.getCaretPosition());
		area.insert("Characterisation Set:\n" + getStats(wSet) + "\n", area
				.getCaretPosition());
		area.insert("Full Test Set:\n" + getStats(fullTestSet) + "\n", area
				.getCaretPosition());
		JPanel panel = new JPanel(false);
		panel.setLayout(new GridLayout(1, 1));
		panel.add(scrollPane, BorderLayout.CENTER);
		return panel;
	}

	private String getStats(Collection<List<String>> paths) {
		String returnString = new String();
		returnString = "size: " + paths.size() + "\n";
		Iterator pathIt = paths.iterator();
		int totalInputs = 0;
		while (pathIt.hasNext()) {
			Collection c = (Collection) pathIt.next();
			totalInputs = totalInputs + c.size();
		}
		returnString = returnString + "total number of inputs: " + totalInputs
				+ "\n";
		return returnString;
	}

	private JComponent makeTextPanel(Collection<List<String>> paths) {
		JTextArea area = new JTextArea();
		JScrollPane scrollPane = new JScrollPane(area);
		Iterator pathIt = paths.iterator();
		while (pathIt.hasNext()) {
			java.util.List l = (java.util.List) pathIt.next();
			area.insert(listPath(l) + "\n", area.getCaretPosition());
		}
		JPanel panel = new JPanel(false);
		panel.setLayout(new GridLayout(1, 1));
		panel.add(scrollPane, BorderLayout.CENTER);
		return panel;
	}

	private String listPath(java.util.List list) {
		Iterator listIt = list.iterator();
		String path = new String();
		while (listIt.hasNext()) {
			Object next = listIt.next();
			if (next instanceof Edge) {
				Edge e = (Edge) next;
				path = path.concat(String.valueOf(e.getUserDatum("EDGE"))
						+ ", ");
			} else {
				String s = (String) next;
				path = path.concat(s + ", ");
			}
		}
		return path.trim();
	}

}
