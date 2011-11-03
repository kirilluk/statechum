/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

/*
 * ErlangTraceGenerator.java
 *
 * Created on Apr 18, 2011, 10:17:58 AM
 */
package statechum.Interface;

import java.awt.BorderLayout;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Arrays;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Random;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JOptionPane;

import statechum.Configuration;
import statechum.Configuration.EXPANSIONOFANY;
import statechum.Configuration.LABELKIND;
import statechum.Helper;
import statechum.analysis.Erlang.ErlangLabel;
import statechum.analysis.Erlang.ErlangModule;
import statechum.analysis.Erlang.ErlangRunner;
import statechum.analysis.learning.ErlangOracleLearner;
import statechum.analysis.learning.ErlangOracleLearner.TraceOutcome;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;

import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * 
 * @author ramsay
 */
public class ErlangTraceGenerator extends javax.swing.JFrame {

	/**
	 * Serial ID.
	 */
	private static final long serialVersionUID = -5038890683208421642L;
	// protected Set<ErlangLabel> alphabet;
	protected ErlangModule targetModule;

	protected String wrapper;

	public void setAlphabet() {
		// alphabet = al;
		JLabel ta = new JLabel("<html>");
		for (OtpErlangTuple a : targetModule.behaviour.getAlphabet()) {
			if (!ta.getText().equals("<html>")) {
				ta.setText(ta.getText() + "<br />");
			}
			ta.setText(ta.getText() + a.toString());
		}
		ta.setText(ta.getText() + "</html>");
		alphabetPane.getViewport().removeAll();
		alphabetPane.getViewport().add(ta, BorderLayout.CENTER);

	}

	public void setModule(ErlangModule mod) {
		targetModule = mod;
		setAlphabet();
		this.setTitle(targetModule.name);
		outfile = new File(mod.sourceFolder,mod.getName()+"_traces.txt");
		fileNameLabel.setText(outfile.getAbsolutePath());
	}

	/** Creates new form ErlangTraceGenerator */
	public ErlangTraceGenerator() {
		initComponents();
	}

	/**
	 * This method is called from within the constructor to initialize the form.
	 * WARNING: Do NOT modify this code. The content of this method is always
	 * regenerated by the Form Editor.
	 */
	// <editor-fold defaultstate="collapsed"
	// <editor-fold defaultstate="collapsed"
	// desc="Generated Code">//GEN-BEGIN:initComponents
	private void initComponents() {

		jLabel1 = new javax.swing.JLabel();
		alphabetPane = new javax.swing.JScrollPane();
		jSeparator1 = new javax.swing.JSeparator();
		genStyle = new javax.swing.JComboBox();
		jLabel2 = new javax.swing.JLabel();
		jButton1 = new javax.swing.JButton();
		jLabel3 = new javax.swing.JLabel();
		fileNameLabel = new javax.swing.JLabel();
		jButton2 = new javax.swing.JButton();
		useOutputMatchingCheckBox = new javax.swing.JCheckBox();
		allAlphabet = new javax.swing.JCheckBox();
		jScrollPane1 = new javax.swing.JScrollPane();
		countInput = new javax.swing.JTextPane();
		jLabel4 = new javax.swing.JLabel();
		jLabel5 = new javax.swing.JLabel();

		setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE);

		jLabel1.setText("Alphabet:");

		genStyle.setModel(new javax.swing.DefaultComboBoxModel(new String[] {
				"Random (length 3)", "Random (length 25)" }));

		jLabel2.setText("Generation style:");

		jButton1.setText("Generate");
		jButton1.addActionListener(new java.awt.event.ActionListener() {
			@Override
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jButton1ActionPerformed(evt);
			}
		});

		jLabel3.setText("Output file:");

		jButton2.setText("select output file");
		jButton2.addActionListener(new java.awt.event.ActionListener() {
			@Override
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jButton2ActionPerformed(evt);
			}
		});

		useOutputMatchingCheckBox.setSelected(true);
		useOutputMatchingCheckBox.setText("Use output matching");

		allAlphabet.setSelected(true);
		allAlphabet.setText("Include entire alphabet");

		countInput.setText("25");
		jScrollPane1.setViewportView(countInput);

		jLabel4.setText("Produce");

		jLabel5.setText("traces");

		javax.swing.GroupLayout layout = new javax.swing.GroupLayout(
				getContentPane());
		getContentPane().setLayout(layout);
		layout.setHorizontalGroup(layout
				.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
				.addGroup(
						layout.createSequentialGroup()
								.addContainerGap()
								.addGroup(
										layout.createParallelGroup(
												javax.swing.GroupLayout.Alignment.LEADING)
												.addComponent(
														jSeparator1,
														javax.swing.GroupLayout.DEFAULT_SIZE,
														827, Short.MAX_VALUE)
												.addGroup(
														layout.createSequentialGroup()
																.addGroup(
																		layout.createParallelGroup(
																				javax.swing.GroupLayout.Alignment.LEADING)
																				.addComponent(
																						alphabetPane,
																						javax.swing.GroupLayout.DEFAULT_SIZE,
																						807,
																						Short.MAX_VALUE)
																				.addComponent(
																						jLabel1))
																.addContainerGap())
												.addGroup(
														javax.swing.GroupLayout.Alignment.TRAILING,
														layout.createSequentialGroup()
																.addGroup(
																		layout.createParallelGroup(
																				javax.swing.GroupLayout.Alignment.LEADING)
																				.addGroup(
																						layout.createSequentialGroup()
																								.addGroup(
																										layout.createParallelGroup(
																												javax.swing.GroupLayout.Alignment.LEADING)
																												.addGroup(
																														layout.createSequentialGroup()
																																.addComponent(
																																		jLabel2)
																																.addPreferredGap(
																																		javax.swing.LayoutStyle.ComponentPlacement.RELATED)
																																.addComponent(
																																		genStyle,
																																		0,
																																		713,
																																		Short.MAX_VALUE))
																												.addGroup(
																														layout.createSequentialGroup()
																																.addComponent(
																																		jLabel3)
																																.addPreferredGap(
																																		javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
																																.addComponent(
																																		fileNameLabel,
																																		javax.swing.GroupLayout.DEFAULT_SIZE,
																																		654,
																																		Short.MAX_VALUE)
																																.addPreferredGap(
																																		javax.swing.LayoutStyle.ComponentPlacement.RELATED)
																																.addComponent(
																																		jButton2)))
																								.addPreferredGap(
																										javax.swing.LayoutStyle.ComponentPlacement.RELATED,
																										3,
																										javax.swing.GroupLayout.PREFERRED_SIZE))
																				.addComponent(
																						jButton1,
																						javax.swing.GroupLayout.Alignment.TRAILING))
																.addContainerGap())
												.addGroup(
														layout.createSequentialGroup()
																.addComponent(
																		useOutputMatchingCheckBox)
																.addPreferredGap(
																		javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
																.addComponent(
																		allAlphabet)
																.addGap(41, 41,
																		41)
																.addComponent(
																		jLabel4)
																.addPreferredGap(
																		javax.swing.LayoutStyle.ComponentPlacement.RELATED)
																.addComponent(
																		jScrollPane1,
																		javax.swing.GroupLayout.PREFERRED_SIZE,
																		39,
																		javax.swing.GroupLayout.PREFERRED_SIZE)
																.addPreferredGap(
																		javax.swing.LayoutStyle.ComponentPlacement.RELATED)
																.addComponent(
																		jLabel5)
																.addContainerGap()))));
		layout.setVerticalGroup(layout
				.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
				.addGroup(
						layout.createSequentialGroup()
								.addContainerGap()
								.addComponent(jLabel1)
								.addPreferredGap(
										javax.swing.LayoutStyle.ComponentPlacement.RELATED)
								.addComponent(alphabetPane,
										javax.swing.GroupLayout.PREFERRED_SIZE,
										236,
										javax.swing.GroupLayout.PREFERRED_SIZE)
								.addPreferredGap(
										javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
								.addComponent(jSeparator1,
										javax.swing.GroupLayout.PREFERRED_SIZE,
										12,
										javax.swing.GroupLayout.PREFERRED_SIZE)
								.addGap(14, 14, 14)
								.addGroup(
										layout.createParallelGroup(
												javax.swing.GroupLayout.Alignment.TRAILING)
												.addGroup(
														layout.createParallelGroup(
																javax.swing.GroupLayout.Alignment.LEADING,
																false)
																.addComponent(
																		useOutputMatchingCheckBox,
																		javax.swing.GroupLayout.DEFAULT_SIZE,
																		javax.swing.GroupLayout.DEFAULT_SIZE,
																		Short.MAX_VALUE)
																.addGroup(
																		layout.createParallelGroup(
																				javax.swing.GroupLayout.Alignment.BASELINE)
																				.addComponent(
																						allAlphabet,
																						javax.swing.GroupLayout.DEFAULT_SIZE,
																						javax.swing.GroupLayout.DEFAULT_SIZE,
																						Short.MAX_VALUE)
																				.addComponent(
																						jLabel4,
																						javax.swing.GroupLayout.DEFAULT_SIZE,
																						javax.swing.GroupLayout.DEFAULT_SIZE,
																						Short.MAX_VALUE)))
												.addGroup(
														layout.createParallelGroup(
																javax.swing.GroupLayout.Alignment.LEADING)
																.addComponent(
																		jLabel5,
																		javax.swing.GroupLayout.DEFAULT_SIZE,
																		23,
																		Short.MAX_VALUE)
																.addComponent(
																		jScrollPane1,
																		javax.swing.GroupLayout.Alignment.TRAILING,
																		javax.swing.GroupLayout.PREFERRED_SIZE,
																		javax.swing.GroupLayout.DEFAULT_SIZE,
																		javax.swing.GroupLayout.PREFERRED_SIZE)))
								.addPreferredGap(
										javax.swing.LayoutStyle.ComponentPlacement.RELATED,
										8, Short.MAX_VALUE)
								.addGroup(
										layout.createParallelGroup(
												javax.swing.GroupLayout.Alignment.BASELINE)
												.addComponent(jLabel3)
												.addComponent(
														fileNameLabel,
														javax.swing.GroupLayout.PREFERRED_SIZE,
														25,
														javax.swing.GroupLayout.PREFERRED_SIZE)
												.addComponent(jButton2))
								.addPreferredGap(
										javax.swing.LayoutStyle.ComponentPlacement.RELATED)
								.addGroup(
										layout.createParallelGroup(
												javax.swing.GroupLayout.Alignment.BASELINE)
												.addComponent(
														genStyle,
														javax.swing.GroupLayout.PREFERRED_SIZE,
														javax.swing.GroupLayout.DEFAULT_SIZE,
														javax.swing.GroupLayout.PREFERRED_SIZE)
												.addComponent(jLabel2))
								.addPreferredGap(
										javax.swing.LayoutStyle.ComponentPlacement.RELATED)
								.addComponent(jButton1).addContainerGap()));

		pack();
	}// </editor-fold>//GEN-END:initComponents

	protected File outfile;

	void jButton2ActionPerformed(@SuppressWarnings("unused") java.awt.event.ActionEvent evt) {// GEN-FIRST:event_jButton2ActionPerformed
		JFileChooser chooser = new JFileChooser(module.sourceFolder);
		chooser.setAcceptAllFileFilterUsed(false);
		chooser.setFileFilter(new Start.TxtFileFilter());
		int returnValue = chooser.showOpenDialog(null);
		if (returnValue == JFileChooser.APPROVE_OPTION) {
			try {
				fileNameLabel.setText(chooser.getSelectedFile()
						.getCanonicalPath());
				outfile = chooser.getSelectedFile();
			} catch (IOException ex) {
				Logger.getLogger(Start.class.getName()).log(Level.SEVERE, null,
						ex);
			}
		}
	}// GEN-LAST:event_jButton2ActionPerformed

	void jButton1ActionPerformed(@SuppressWarnings("unused") java.awt.event.ActionEvent evt) {// GEN-FIRST:event_jButton1ActionPerformed
		String style = genStyle.getSelectedItem().toString();
		boolean exhaustAlphabet = allAlphabet.isSelected();
		try {
			int count = Integer.parseInt(countInput.getText());
			if (outfile == null) {
				JOptionPane.showMessageDialog(this,
						"No output file specified...");
			} else {
				if (style.equals("Random (length 3)")) {
					genRandom(outfile, 3, count, exhaustAlphabet);
				} else if (style.equals("Random (length 25)")) {
					genRandom(outfile, 25, count, exhaustAlphabet);
				} else {
					Helper.throwUnchecked(
							"Unknown random generation style selected (somehow...)",
							new RuntimeException(
									"Unknown random generation style selected (somehow...)"));
				}
				Traces.main(new String[] { outfile.getAbsolutePath() });
			}
		} catch (NumberFormatException e) {
			System.out.println(countInput.getText() + " is not a number...");
		}
	}// GEN-LAST:event_jButton1ActionPerformed

	public Process erlangProcess = null;

	private void genRandom(File file, int length, int count,
			boolean exhaustAlphabet) {
		genRandom(targetModule, file, length, count, exhaustAlphabet,
				useOutputMatchingCheckBox.isSelected());
	}

	private static BufferedWriter out;
	private static Configuration config;
	private static ErlangOracleLearner learner;
	private static ErlangModule module;

	private static void setupFile(File file, boolean useOutputMatching,
			EXPANSIONOFANY expand) throws IOException {
		out = new BufferedWriter(new FileWriter(file));
		/*
		out.write("config erlangSourceFile " + module.sourceFolder
				+ File.separator + module.name + ".erl\n");
		out.write("config labelKind LABEL_ERLANG\n");
		out.write("config erlangModuleName " + module.name + "\n");*/
		config = Configuration.getDefaultConfiguration().copy();
		if (!useOutputMatching) {
			config.setUseErlangOutputs(false);
			//out.write("config useErlangOutputs false\n");
		}
		config.setLabelKind(LABELKIND.LABEL_ERLANG);
		config.setErlangAlphabetAnyElements(expand);
		config.setErlangModuleName(module.name);
		config.setErlangSourceFile(new File(module.sourceFolder, module.name
				+ ErlangRunner.ERL.ERL.toString()));
		config.writeModifiedIntoWriter(out);
	}

	public static void genRandom(ErlangModule targetmodule, File file,
			int length, int count, boolean exhaustAlphabet,
			boolean useOutputMatching) {
		genRandom(targetmodule, file, length, count, exhaustAlphabet,
				useOutputMatching, EXPANSIONOFANY.ANY_WITHLIST);
	}

	public static void genRandom(ErlangModule targetmodule, File file,
			int length, int count, boolean exhaustAlphabet,
			boolean useOutputMatching, EXPANSIONOFANY expand) {
		genRandom(targetmodule, file, length, count, exhaustAlphabet,
				useOutputMatching,expand, (new Random(0)).nextLong());
	}
	
	public static void genRandom(ErlangModule targetmodule, File file,
			int length, int count, boolean exhaustAlphabet,
			boolean useOutputMatching, EXPANSIONOFANY expand, long seed) {
		try {
			Random rand = new Random(seed);
			module = targetmodule;
			setupFile(file, useOutputMatching, expand);
			learner = new ErlangOracleLearner(null,
					new LearnerEvaluationConfiguration(config));

			// Make sure our copy of the module is the same object as the
			// learner's so that alphabet mods work...
			module = learner.getModule();

			LinkedList<List<ErlangLabel>> pos = new LinkedList<List<ErlangLabel>>();
			LinkedList<List<ErlangLabel>> neg = new LinkedList<List<ErlangLabel>>();

			// If we have been told to include the whole alphabet then do each
			// element at least once to start with.
			// This will also be a useful seed for the random generator, since
			// its likely that only the init calls
			// will be valid as initial elements of traces
			if (exhaustAlphabet) {
				Set<ErlangLabel> newaClone;
				Set<ErlangLabel> aClone = new HashSet<ErlangLabel>();
				do {
					newaClone = new HashSet<ErlangLabel>(
							module.behaviour.getAlphabet());
					newaClone.removeAll(aClone);
					aClone.addAll(newaClone);
					int counter = 1;
					for (ErlangLabel l : newaClone) {
						System.out.println("" + counter + " of "
								+ newaClone.size());
						counter++;
						List<ErlangLabel> line = new LinkedList<ErlangLabel>();
						line.add(l);
						evaluateLine(module, out, learner, pos, neg, line);
					}
				} while (module.behaviour.getAlphabet().size() > aClone.size());

			}

			for (int i = 0; i < count; i++) {
				System.out.print("" + i + "...");
				System.out.flush();
				if (i % 20 == 0) {
					System.out.println("");
				}
				// System.out.print("Generating a line...");
				// System.out.flush();
				List<ErlangLabel> line = randLine2(module,
						module.behaviour.getAlphabet(), length, pos, neg, rand);
				// System.out.println("Evaluating...");
				// System.out.flush();
				if (line.size() > 0) {
					evaluateLine(module, out, learner, pos, neg, line);
				} else {
					// 0 length lines mean there are no possible extensions.
					// Give up.
					break;
				}
			}
			// System.out.println(".");
			out.close();
		} catch (IOException e) {
			Helper.throwUnchecked("Error writing traces file", e);
		}

	}

	/** Generate all possible traces to the specified depth */
	public static void genComplete(ErlangModule targetmodule, File file,
			int depth, boolean useOutputMatching) {
		genComplete(targetmodule, file, depth, useOutputMatching,
				EXPANSIONOFANY.ANY_WITHLIST);
	}

	public static void genComplete(ErlangModule targetmodule, File file,
			int depth, boolean useOutputMatching, EXPANSIONOFANY expand) {
		try {
			module = targetmodule;
			setupFile(file, useOutputMatching, expand);
			learner = new ErlangOracleLearner(null,
					new LearnerEvaluationConfiguration(config));

			// Make sure our copy of the module is the same object as the
			// learner's so that alphabet mods work...
			module = learner.getModule();

			genSet(module.behaviour.getAlphabet(), depth);
			out.close();
		} catch (IOException e) {
			Helper.throwUnchecked("Error writing traces file", e);
		}

	}

	private static Set<List<ErlangLabel>> genSet(Set<ErlangLabel> alphabet,
			int depth) throws IOException {
		LinkedList<List<ErlangLabel>> neg = new LinkedList<List<ErlangLabel>>();
		LinkedList<List<ErlangLabel>> pos = new LinkedList<List<ErlangLabel>>();
		if (depth > 1) {
			Set<List<ErlangLabel>> shorter = genSet(alphabet, depth - 1);
			System.out.print("Extending " + shorter.size() + " children with "
					+ alphabet.size() + " elements...");
			for (List<ErlangLabel> ls : shorter) {
				Set<ErlangLabel> aclone = new HashSet<ErlangLabel>(alphabet);

				for (ErlangLabel l : aclone) {
					LinkedList<ErlangLabel> ls2 = new LinkedList<ErlangLabel>(
							ls);
					ls2.add(l);
					evaluateLine(module, out, learner, pos, neg, ls2);
				}
			}
		} else {
			for (ErlangLabel l : alphabet) {
				LinkedList<ErlangLabel> ls = new LinkedList<ErlangLabel>();
				ls.add(l);
				evaluateLine(module, out, learner, pos, neg, ls);
			}
		}
		System.out.println("Done depth " + depth + ".");
		return new HashSet<List<ErlangLabel>>(pos);
	}

	private static void evaluateLine(ErlangModule module, BufferedWriter out,
			ErlangOracleLearner learner, LinkedList<List<ErlangLabel>> pos,
			LinkedList<List<ErlangLabel>> neg, List<ErlangLabel> line)
			throws IOException {
		TraceOutcome response = learner.askErlang(line);
		// System.out.println("Writing " + response.toTraceFileString());
		out.write(response.toTraceFileString() + "\n");
		switch (response.outcome) {
		case TRACE_FAIL:
			neg.add(Arrays.asList(response.answerDetails));
			break;
		case TRACE_DIFFERENTOUTPUT:
			List<ErlangLabel> n = new LinkedList<ErlangLabel>();
			for (int j = 0; j < response.answerDetails.length; j++) {
				n.add(response.questionDetails[j]);
			}
			if (n.size() > 0) {
				neg.add(n);
			}
			for (int j = 0; j < response.answerDetails.length; j++) {
				// Lets hope Java's Set is an actual Set...
				module.behaviour.getAlphabet().add(response.answerDetails[j]);
			}
			//$FALL-THROUGH$
		case TRACE_OK:
			pos.add(Arrays.asList(response.answerDetails));
			break;
		}
	}

	private static List<ErlangLabel> randLine2(ErlangModule module,
			Set<ErlangLabel> alphabet, int length,
			LinkedList<List<ErlangLabel>> pos, LinkedList<List<ErlangLabel>> neg, Random randGen) {
		LinkedList<ErlangLabel> result = new LinkedList<ErlangLabel>();

		do {
			int rand = randGen.nextInt(pos.size());
			result = new LinkedList<ErlangLabel>(pos.get(rand));
		} while (result.size() >= length);

		for (int i = result.size(); i < length; i++) {
			int rand = randGen.nextInt(alphabet.size());
			result.add((ErlangLabel) alphabet.toArray()[rand]);
		}

		return result;
	}


	/**
	 * @param args
	 *            the command line arguments
	 */
	public static void main(String args[]) {

		java.awt.EventQueue.invokeLater(new Runnable() {

			@Override
			public void run() {
				new ErlangTraceGenerator().setVisible(true);
			}
		});
	}

	// Variables declaration - do not modify//GEN-BEGIN:variables
	private javax.swing.JCheckBox allAlphabet;
	private javax.swing.JScrollPane alphabetPane;
	private javax.swing.JTextPane countInput;
	private javax.swing.JLabel fileNameLabel;
	private javax.swing.JComboBox genStyle;
	private javax.swing.JButton jButton1;
	private javax.swing.JButton jButton2;
	private javax.swing.JLabel jLabel1;
	private javax.swing.JLabel jLabel2;
	private javax.swing.JLabel jLabel3;
	private javax.swing.JLabel jLabel4;
	private javax.swing.JLabel jLabel5;
	private javax.swing.JScrollPane jScrollPane1;
	private javax.swing.JSeparator jSeparator1;
	private javax.swing.JCheckBox useOutputMatchingCheckBox;

	// End of variables declaration//GEN-END:variables

	protected void useOutputMatchingCheckBoxActionPerformed(
			java.awt.event.ActionEvent ev) {
		this.setVisible(false);
		Configuration config = Configuration.getDefaultConfiguration();
		if (!useOutputMatchingCheckBox.isSelected()) {
			config.setUseErlangOutputs(false);
		} else {
			config.setUseErlangOutputs(true);
		}
		config.setErlangSourceFile(new File(targetModule.sourceFolder,
				targetModule.name + ErlangRunner.ERL.ERL.toString()));
		try {
			setModule(ErlangModule.loadModule(config, true));
			this.setVisible(true);
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
}
