/* Copyright (c) 2011 The University of Sheffield.
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
 * 
 */
/*
 * ErlangApplicationLoader.java
 *
 * Created on Feb 16, 2011, 10:16:50 AM
 */
package statechum.Interface;

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;

import javax.swing.AbstractListModel;
import javax.swing.ButtonGroup;
import javax.swing.ComboBoxModel;
import javax.swing.DefaultListModel;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;
import javax.swing.filechooser.FileFilter;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;

import statechum.Configuration;
import statechum.Configuration.EXPANSIONOFANY;
import statechum.analysis.Erlang.ErlangApp;
import statechum.analysis.Erlang.ErlangAppReader;
import statechum.analysis.Erlang.ErlangModule;
import statechum.analysis.Erlang.Signatures.AnySignature;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.apps.ErlangQSMOracle;

/**
 * 
 * @author ramsay
 */
@SuppressWarnings("serial")
public class ErlangApplicationLoader extends javax.swing.JFrame {

	protected ErlangApp app;
	protected File folder;
	protected Configuration config = Configuration.getDefaultConfiguration().copy();

	/** Creates new form ErlangApplicationLoader */
	public ErlangApplicationLoader() {
		super("StateChum Erlang Application Loader");
		initComponents();
	}
	
	class InnerAlphabetModel extends AbstractListModel implements ComboBoxModel
	{
		List<EXPANSIONOFANY> expansionValues = Arrays.asList(EXPANSIONOFANY.values());
		List<String>sigValues = new ArrayList<String>(expansionValues.size());
		int selectedItemIdx=0;
		
		public InnerAlphabetModel()
		{// Default constructor
			assert expansionValues.size() > 0;
			for(EXPANSIONOFANY value:expansionValues)
			{
				Configuration conf = config.copy();conf.setErlangAlphabetAnyElements(value);
				StringBuffer alphabetValues = new StringBuffer();
				boolean first = true;
				for(OtpErlangObject obj:new AnySignature(conf, new OtpErlangList()).instantiateAllAlts())
				{
					if (first) first = false;else alphabetValues.append(", ");
					alphabetValues.append(obj.toString());
				}
				String expansionValue = alphabetValues.toString();
				assert !sigValues.contains(expansionValue) : "duplicate alphabet value "+expansionValue;
				sigValues.add(expansionValue);
			}
		}
		
		@Override
		public int getSize() {
			return expansionValues.size();
		}
		
		@Override
		public Object getElementAt(int index) {
			if (index >= 0 && index < expansionValues.size())
				return sigValues.get(index);
			
			return null;
		}
		
	    /**
	     * Set the value of the selected item. The selected item may be null.
	     * <p>
	     * @param anObject The combo box value or null for no selection.
	     */
		@Override
		public void setSelectedItem(Object anItem) 
		{
			int value = sigValues.indexOf(anItem);
			if (value < 0) throw new IllegalArgumentException("alphabet value "+anItem.toString()+" is not known");
			selectedItemIdx = value;
		}
		
		@Override
		public Object getSelectedItem() {
			return sigValues.get(selectedItemIdx);
		}
		
		public EXPANSIONOFANY getSelectedExpansion()
		{
			return expansionValues.get(selectedItemIdx);
		}
		
	}
	

	/**
	 * This method is called from within the constructor to initialize the form.
	 * WARNING: Do NOT modify this code. The content of this method is always
	 * regenerated by the Form Editor.
	 */
	// <editor-fold defaultstate="collapsed"
	// desc="Generated Code">//GEN-BEGIN:initComponents
	private void initComponents() {

		jLabel1 = new javax.swing.JLabel();
		appFile = new javax.swing.JLabel();
		jButton1 = new javax.swing.JButton();
		beginButton = new javax.swing.JButton();
		jLabel2 = new javax.swing.JLabel();
		startModule = new javax.swing.JLabel();
		jLabel3 = new javax.swing.JLabel();
		startModuleArgs = new javax.swing.JLabel();
		jLabel4 = new javax.swing.JLabel();
		jScrollPane1 = new javax.swing.JScrollPane();
		modules = new javax.swing.JList();
		jButton2 = new javax.swing.JButton();
		jButton3 = new javax.swing.JButton();

		setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE);

		jLabel1.setText("App file:");

		jButton1.setText("Choose");
		jButton1.addActionListener(new java.awt.event.ActionListener() {
			@Override
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jButton1ActionPerformed(evt);
			}
		});

		beginButton.setText("Auto-Analyse All");
		beginButton.addActionListener(new java.awt.event.ActionListener() {
			@Override
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				beginButtonActionPerformed(evt);
			}
		});

		jLabel2.setText("Start Module:");

		jLabel3.setText("Start Args:");

		jLabel4.setText("Modules:");

		jScrollPane1.setViewportView(modules);

		jButton2.setText("View selected module");
		jButton2.addActionListener(new java.awt.event.ActionListener() {
			@Override
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jButton2ActionPerformed(evt);
			}
		});

		jButton3.setText("Reload");
		jButton3.addActionListener(new java.awt.event.ActionListener() {
			@Override
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jButton3ActionPerformed(evt);
			}
		});

		JPanel top = new JPanel();
		JPanel mid = new JPanel();
		JPanel bottom = new JPanel();
		this.setLayout(new GridBagLayout());
		GridBagConstraints c = new GridBagConstraints();

		top.setLayout(new GridBagLayout());
		c.fill = GridBagConstraints.HORIZONTAL;
		c.weightx = 0.001;
		c.weighty = 0.001;
		c.gridx = 0;
		c.gridy = 0;
		c.anchor = GridBagConstraints.LINE_START;
		top.add(jLabel1, c);
		c.weightx = 0.999;
		c.weighty = 0.999;
		c.gridx = 1;
		top.add(appFile, c);
		c.weightx = 0.001;
		c.weighty = 0.001;
		c.anchor = GridBagConstraints.LINE_END;
		c.gridx = 2;
		top.add(jButton3, c);
		c.gridx = 3;
		top.add(jButton1, c);

		c.fill = GridBagConstraints.HORIZONTAL;
		c.weightx = 0.001;
		c.weighty = 0.001;
		c.gridx = 0;
		c.gridy = 1;
		c.anchor = GridBagConstraints.LINE_START;
		top.add(jLabel2, c);
		c.weightx = 0.999;
		c.weighty = 0.999;
		c.gridx = 1;
		c.gridy = 1;
		top.add(startModule, c);

		c.fill = GridBagConstraints.HORIZONTAL;
		c.weightx = 0.001;
		c.weighty = 0.001;
		c.gridx = 0;
		c.gridy = 2;
		c.anchor = GridBagConstraints.LINE_START;
		top.add(jLabel3, c);
		c.weightx = 0.999;
		c.weighty = 0.999;
		c.gridx = 1;
		c.gridy = 2;
		top.add(startModuleArgs, c);

		c.weightx = 0.1;
		c.weighty = 0.1;
		c.gridy = 0;
		c.gridx = 0;
		c.fill = GridBagConstraints.HORIZONTAL;
		c.anchor = GridBagConstraints.PAGE_START;
		this.add(top, c);

		mid.setLayout(new GridBagLayout());
		c.weightx = 0.001;
		c.weighty = 0.001;
		c.gridy = 0;
		c.anchor = GridBagConstraints.FIRST_LINE_START;
		mid.add(jLabel4, c);
		c.gridy = 1;
		mid.add(jButton2, c);
		c.gridy = 2;
		c.weightx = 0.999;
		c.weighty = 0.999;
		c.anchor = GridBagConstraints.CENTER;
		c.fill = GridBagConstraints.BOTH;
		mid.add(jScrollPane1, c);

		c.weightx = 0.9;
		c.weighty = 0.9;
		c.gridx = 0;
		c.gridy = 1;
		c.anchor = GridBagConstraints.CENTER;
		this.add(mid, c);

		lenField = new JTextField("10");
		countField = new JTextField("250");
		exhaustAlphabetBox = new JCheckBox("Exhaust the alphabet", true);
		outputMatchingBox = new JCheckBox("Use output matching", true);

		ButtonGroup bgroup = new ButtonGroup();
		exhaustiveButton = new JRadioButton("Use exhaustive generation");
		randomButton = new JRadioButton("Use random generation");
		bgroup.add(exhaustiveButton);
		bgroup.add(randomButton);
		exhaustiveButton.addActionListener(new java.awt.event.ActionListener() {
			@Override
			public void actionPerformed(@SuppressWarnings("unused") java.awt.event.ActionEvent evt) {
				generatorChange(true);
			}
		});
		randomButton.addActionListener(new java.awt.event.ActionListener() {
			@Override
			public void actionPerformed(@SuppressWarnings("unused") java.awt.event.ActionEvent evt) {
				generatorChange(false);
			}
		});
		randomButton.setSelected(true);
		seedField = new JTextField("" + (new Random()).nextLong());

		anyAlphabet = new JComboBox(new InnerAlphabetModel());
		bottom.setLayout(new GridBagLayout());
		c.weightx = 0.001;
		c.gridx = 0;
		c.gridy = 0;
		c.anchor = GridBagConstraints.LINE_START;
		bottom.add(new JLabel("Trace length"), c);
		c.weightx = 0.999;
		c.gridx = 1;
		c.gridy = 0;
		c.anchor = GridBagConstraints.LINE_START;
		bottom.add(lenField, c);
		c.weightx = 0.001;
		c.gridx = 0;
		c.gridy = 1;
		c.anchor = GridBagConstraints.LINE_START;
		bottom.add(new JLabel("Number of traces to generate"), c);
		c.weightx = 0.999;
		c.gridx = 1;
		c.gridy = 1;
		c.anchor = GridBagConstraints.LINE_START;
		bottom.add(countField, c);

		c.weightx = 0.001;
		c.gridx = 0;
		c.gridy = 2;
		c.anchor = GridBagConstraints.LINE_START;
		bottom.add(exhaustAlphabetBox, c);
		c.gridx = 0;
		c.gridy = 3;
		c.anchor = GridBagConstraints.LINE_START;
		bottom.add(outputMatchingBox, c);

		c.weightx = 0.999;
		c.gridx = 1;
		c.gridy = 2;
		c.anchor = GridBagConstraints.LINE_START;
		bottom.add(exhaustiveButton, c);
		c.gridx = 1;
		c.gridy = 3;
		c.anchor = GridBagConstraints.LINE_START;
		bottom.add(randomButton, c);

		c.weightx = 0.001;
		c.gridx = 0;
		c.gridy = 4;
		c.anchor = GridBagConstraints.LINE_START;
		bottom.add(new JLabel("Alphabet for type 'any()' values"), c);

		c.weightx = 0.999;
		c.gridx = 1;
		c.gridy = 4;
		c.anchor = GridBagConstraints.LINE_START;
		bottom.add(anyAlphabet, c);

		c.weightx = 0.001;
		c.gridx = 0;
		c.gridy = 5;
		c.anchor = GridBagConstraints.LINE_START;
		bottom.add(new JLabel("Random seed"), c);

		c.weightx = 0.999;
		c.gridx = 1;
		c.gridy = 5;
		c.anchor = GridBagConstraints.LINE_START;
		bottom.add(seedField, c);

		/*
		 * c.weightx = 0.5; c.weighty = 0.5; c.gridx = 1; c.gridy = 4; c.anchor
		 * = GridBagConstraints.LINE_START; bottom.add(randomButton, c);
		 */

		c.weightx = 0.999;
		c.weighty = 0.2;
		c.gridx = 1;
		c.gridy = 99;
		c.fill = GridBagConstraints.HORIZONTAL;
		c.anchor = GridBagConstraints.PAGE_END;
		bottom.add(beginButton, c);

		c.weightx = 0.2;
		c.weighty = 0.2;
		c.gridx = 0;
		c.gridy = 2;
		c.fill = GridBagConstraints.HORIZONTAL;
		c.anchor = GridBagConstraints.PAGE_END;
		this.add(bottom, c);

		this.setPreferredSize(new Dimension(800, 600));

		pack();
	}// </editor-fold>//GEN-END:initComponents

	protected File selectedFile;

	private void jButton1ActionPerformed(@SuppressWarnings("unused") java.awt.event.ActionEvent evt) {// GEN-FIRST:event_jButton1ActionPerformed
		JFileChooser chooser;
		if (selectedFile != null) {
			chooser = new JFileChooser(selectedFile.getParentFile());
		} else {
			chooser = new JFileChooser();
		}
		chooser.setAcceptAllFileFilterUsed(false);
		chooser.addChoosableFileFilter(new ErlangApplicationLoader.appFilter());
		chooser.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);
		int returnValue = chooser.showOpenDialog(null);
		if (returnValue == JFileChooser.APPROVE_OPTION) {
			selectedFile = chooser.getSelectedFile();
			appFile.setText(selectedFile.getName());
			loadData();
		}
	}// GEN-LAST:event_jButton1ActionPerformed

	/** Loads a list of modules and returns true on success. */
	protected boolean loadData() {
		try {
			if (selectedFile.getName().endsWith(".app")) {
				folder = selectedFile.getParentFile();
				app = ErlangAppReader.readAppFile(selectedFile.getName(), folder);
			} else {
				folder = selectedFile;
				app = ErlangAppReader.readFolder(selectedFile);
			}
			startModule.setText(app.startModule);
			startModuleArgs.setText(app.startModuleArgs);
			DefaultListModel model = new DefaultListModel();
			for (ErlangModule m : app.modules) {
				model.addElement(m);
			}
			modules.setModel(model);
		} catch (IOException e) {
			System.out.println("Failed to load application from " + selectedFile);
			return false;
		}
		return true;
	}

	/** Deletes all auxiliary Erlang files in the supplied directory. */
	public static void zapErlFiles(File where) {
		// Keeping these for debugging and interest:
		// "test2.out","test2.out.covermap",
		for (String str : new String[] { "erl_crash.dump", ".dialyzer_plt", "tmp.cover" }) {
			File file = new File(where.getAbsolutePath(),str);
			if (file.canRead() && !file.delete())
				throw new RuntimeException("failed to delete " + file.getAbsolutePath());
		}

	}

	protected JTextField lenField;
	protected JTextField countField;
	protected JCheckBox exhaustAlphabetBox;
	protected JCheckBox outputMatchingBox;
	protected JRadioButton exhaustiveButton;
	protected JRadioButton randomButton;
	protected JComboBox anyAlphabet;
	protected JTextField seedField;

	void generatorChange(boolean exhaustive) {
		if (exhaustive) {
			seedField.setEnabled(false);
			countField.setEnabled(false);
		} else {
			seedField.setEnabled(true);
			countField.setEnabled(true);
		}
	}

	private void beginButtonActionPerformed(@SuppressWarnings("unused") java.awt.event.ActionEvent evt) {// GEN-FIRST:event_beginButtonActionPerformed
		zapErlFiles(folder);
		try {
			/*
			 * Files in this folder are supposed to be compiled by ErlangRunner
			 * - it has a place where they are listed.
			 * 
			 * for (File f : ErlangRunner.ErlangFolder.listFiles()) if
			 * (ErlangRunner.validName(f.getName())) ErlangRunner.compileErl(f,
			 * ErlangRunner.getRunner());
			 */
			for (Object s : app.modules) {
				try {
					// Load the module
					ErlangModule m = (ErlangModule) s;

					int len = Integer.parseInt(lenField.getText());
					boolean exhaustAlphabet = exhaustAlphabetBox.isSelected();
					boolean useOutputMatching = outputMatchingBox.isSelected();
					EXPANSIONOFANY expansion = ((InnerAlphabetModel)anyAlphabet.getModel()).getSelectedExpansion();
					System.out.println("Generating traces for " + m.name + "...");
					String tracefile = m.name + ".traces";
					if (exhaustiveButton.isSelected()) {
						ErlangTraceGenerator.genComplete(m, new File(tracefile), len, useOutputMatching, expansion);
					} else {
						long seed = Long.parseLong(seedField.getText());
						int count = Integer.parseInt(countField.getText());
						ErlangTraceGenerator.genRandom(m, new File(tracefile), len, count, exhaustAlphabet,
								useOutputMatching, expansion, seed);
					}
					// Run ErlangQSMOracle on the trace file...

					System.out.println("Learning " + m.name + "...");
					LearnerGraph g = ErlangQSMOracle.startInference(tracefile);
					System.out.println("Produced " + g.getStateNumber() + " states");
				} catch (Exception e) {
					JOptionPane.showMessageDialog(this, e.toString());
					e.printStackTrace();
				}
			}
		} finally {
			zapErlFiles(folder);
		}

	}// GEN-LAST:event_beginButtonActionPerformed

	private void jButton2ActionPerformed(@SuppressWarnings("unused") java.awt.event.ActionEvent evt) {// GEN-FIRST:event_jButton2ActionPerformed
		for (Object s : modules.getSelectedValues()) {
			ErlangModuleViewer view = new ErlangModuleViewer((ErlangModule) s);
			view.pack();
			view.setVisible(true);

		}
	}// GEN-LAST:event_jButton2ActionPerformed

	private void jButton3ActionPerformed(@SuppressWarnings("unused") java.awt.event.ActionEvent evt) {// GEN-FIRST:event_jButton3ActionPerformed
		loadData();
	}// GEN-LAST:event_jButton3ActionPerformed

	/**
	 * @param args
	 *            the command line arguments
	 */
	public static void main(String args[]) {
		if (args.length > 0) {// run the analysis directly

			ErlangApplicationLoader loader = new ErlangApplicationLoader();
			loader.setSelectedFile(new File(args[0]));
			ErlangApplicationLoader.zapErlFiles(loader.selectedFile);
			loader.loadData();
			/*
			 * What is this even for?.... if (loader.loadData() &&
			 * loader.modules.getModel().getSize() != 1) { ListModel modules =
			 * loader.modules.getModel(); StringBuffer tooManyException = new
			 * StringBuffer("more than a single module choice for app " +
			 * loader.selectedFile + "\n"); for (int i = 0; i <
			 * modules.getSize(); ++i) {
			 * tooManyException.append(modules.getElementAt(i));
			 * tooManyException.append('\n'); } throw new
			 * IllegalArgumentException(tooManyException.toString()); }
			 * loader.modules.setSelectedIndex(0);
			 * loader.beginButtonActionPerformed(null);
			 */
			loader.setVisible(true);
		} else {
			java.awt.EventQueue.invokeLater(new Runnable() {

				@Override
				public void run() {
					new ErlangApplicationLoader().setVisible(true);
				}
			});
		}
	}

	private void setSelectedFile(File file) {
		selectedFile = file;
		appFile.setText(selectedFile.getName());
	}

	// Variables declaration - do not modify//GEN-BEGIN:variables
	private javax.swing.JLabel appFile;
	private javax.swing.JButton beginButton;
	private javax.swing.JButton jButton1;
	private javax.swing.JButton jButton2;
	private javax.swing.JButton jButton3;
	private javax.swing.JLabel jLabel1;
	private javax.swing.JLabel jLabel2;
	private javax.swing.JLabel jLabel3;
	private javax.swing.JLabel jLabel4;
	private javax.swing.JScrollPane jScrollPane1;
	private javax.swing.JList modules;
	private javax.swing.JLabel startModule;
	private javax.swing.JLabel startModuleArgs;

	// End of variables declaration//GEN-END:variables

	class appFilter extends FileFilter {

		@Override
		public boolean accept(File f) {
			if (f.isDirectory()) {
				return true;
			}
			int i = f.getName().lastIndexOf(".");
			if (i >= 0) {
				String extension = f.getName().substring(i).toLowerCase();
				if (extension.equals(".app")) {
					return true;
				}
			}
			return false;
		}

		// The description of this filter
		@Override
		public String getDescription() {
			return "Just .app files";
		}
	}
}
