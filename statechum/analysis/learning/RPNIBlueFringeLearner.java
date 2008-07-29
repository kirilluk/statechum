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

package statechum.analysis.learning;

import java.awt.Frame;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.XMLEncoder;
import java.io.BufferedOutputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.lang.reflect.InvocationTargetException;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;

import javax.swing.*;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import statechum.Configuration;
import statechum.Pair;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.model.testset.PTASequenceEngine;

import edu.uci.ics.jung.graph.impl.*;

public abstract class RPNIBlueFringeLearner  extends Observable implements Learner {
	protected int questionCounter = 0;
	
	protected final Configuration config;
	
	/** The frame in relation to which to pop dialog boxes. */
	protected Frame parentFrame;

	public RPNIBlueFringeLearner(Frame parent, Configuration c) 
	{
		config = c;
		parentFrame = parent;
	}
	
	/** Retrieves the configuration used by this learner. */
	public Configuration getConfig()
	{
		return config;
	}
	
	public void debugAction(LearnerGraph lg, int iterations){
		if(!config.getDebugMode())
			return;
		notifyObservers(new LearnerState(iterations,lg));
		updateGraph(lg);
	}
	
	/** Initialises this learner. */
	abstract public void init(Collection<List<String>> plus, Collection<List<String>> minus);
	abstract public void init(PTASequenceEngine en, int plus, int minus);

	/** Loads PTA from a supplied file. */
	public void loadPTA(@SuppressWarnings("unused")	String name)
	{
		throw new UnsupportedOperationException();
	}
	

	/** Does the learning, returning the result. */
	public abstract DirectedSparseGraph learnMachine();

	protected int counterRestarted = 0;
	
	/** Returns the number of times learner had to restart. */
	public int getRestarts()
	{
		return counterRestarted;
	}
	
	/** Given a score, we need to determine whether to ask questions. This depends on a number
	 * of configuration parameters.
	 * 
	 * @param score score we are looking at
	 * @return whether any questions should be asked or we could just proceed with merging.
	 */ 
	protected boolean shouldAskQuestions(int score)
	{
		if (!config.getAskQuestions())
			return false;
		
		if (config.getCertaintyThreshold() >= 0 && score > config.getCertaintyThreshold())
			return false;
		
		if (score < config.getMinCertaintyThreshold())
			return false;
		
		return true;
	}
	
	/** Updates listeners only if this object has been modified and debug mode is on, by calling
	 * <pre>
	 * setChanged()
	 * </pre>
	 * @param g the graph to display in the associated view
	 */
	public void updateGraph(LearnerGraph g)
	{
		setChanged();
		if (config.getDebugMode())
			notifyObservers(g);
	}
	

	final String questionPrefix="<html><font color=green>";
	
	protected List<String> getShortenedQuestion(List<String> question){
		List<String> questionList = new LinkedList<String>();
		assert(question.size()>=1);
		int counter=1;
		Iterator<String> questionIter = question.iterator();
		String lastQuestion = questionIter.next();
		
		while(questionIter.hasNext())
		{
				String current = questionIter.next();
				if(current.equals(lastQuestion))
					counter++;
				else{
					questionList.add(lastQuestion.concat( (counter>1)? "(*"+counter+")":""));// in the string case, I could add "\n" at the end
					counter = 1;lastQuestion = current;					
				}
		}
		questionList.add(lastQuestion.concat( (counter>1)? "(*"+counter+")":""));
		return questionList;
	}

	protected List<String> beautifyQuestionList(List<String> question)
	{
		List<String> questionList = new LinkedList<String>();
		int i=0;
		for(String q:question)
				questionList.add(questionPrefix+"("+i++ +") "+q);
		
		return questionList;
	}
	
	/** The dialog to be displayed to a user with questions to select. 
	 * This field should not really be public, but since different packages refer to it, 
	 * I chose to make it public for the time being. 
	 */
	public JDialog dialog = null;
	
	/** the option pane. */
	protected JOptionPane jop = null;	
	
	/** Cancels a dialog, if present. With no dialog, learner thread will terminate within a reasonable amount of time.
	 */
	public void terminateLearner()
	{
		assert(SwingUtilities.isEventDispatchThread());
		if (dialog != null && jop != null && dialog.isVisible())
			jop.setValue(new Integer(
                    JOptionPane.CLOSED_OPTION));// from http://java.sun.com/docs/books/tutorial/uiswing/components/examples/CustomDialog.java		}
		// The setting of the option above corresponds to hitting "close" or "ESC" on the dialogue, 
		// which is interpreted by the learner as a request to terminate learning, hence
		// the learner stops and the corresponding thread terminates. For this reason, 
		// it is appropriate to issue a .join() on the learner thread. 
	}
	
	/** Stores recorded answers. */
	protected AbstractOracle ans = null;
	
	/** Makes it possible to answer questions automatically.
	 *  
	 * @param a the class holding stored answers.
	 */
	public void setAnswers(AbstractOracle a)
	{
		ans = a;
	}
		
	public final static String QUESTION_AUTO = "<auto>"; 
	public final static String QUESTION_SPIN = "<spin>"; 
	public final static String QUESTION_USER = "<USER>"; 
	protected String howAnswerWasObtained = "";
	
	protected Pair<Integer,String> handleAutoAnswer(List<String> question)
	{
		howAnswerWasObtained = QUESTION_USER;
		Pair<Integer,String> AutoAnswer = ans == null? null:ans.getAnswer(question);
		if (AutoAnswer != null)
		{
			howAnswerWasObtained = QUESTION_AUTO;
			return AutoAnswer;
		}
		
		return null;
	}
	
	protected void setAutoOracle()
	{
		if (config.getAutoAnswerFileName().length() > 0)
		{
			ans = new StoredAnswers();
			try {
				((StoredAnswers)ans).setAnswers(new FileReader(config.getAutoAnswerFileName()));
			} catch (Exception e) {
				ans = null;
			}
		}
	}
	
	protected Pair<Integer,String> checkWithEndUser(LearnerGraph model,List<String> question, final Object [] moreOptions){
		Pair<Integer,String> autoAnswer = handleAutoAnswer(question);if (autoAnswer != null) return autoAnswer;

		final List<String> questionList = beautifyQuestionList(question);
		final AtomicInteger answer = new AtomicInteger(AbstractOracle.USER_WAITINGFORSELECTION);
		updateGraph(model);
		try {
			SwingUtilities.invokeAndWait(new Runnable() {
				public void run() {
					final Object[] options = new Object[1+moreOptions.length];
					//final JList nonrejectElements = new JList(new String[] { "<html><font color=gray>a","<html><font color=gray>b"});
					final JList rejectElements = new JList(questionList.toArray());
					options[0]="Accept";System.arraycopy(moreOptions, 0, options, 1, moreOptions.length);
					final JLabel label = new JLabel("<html><font color=red>Click on the first non-accepting element below", SwingConstants.CENTER);
					jop = new JOptionPane(new Object[] {label,rejectElements},
			                JOptionPane.QUESTION_MESSAGE,JOptionPane.YES_NO_CANCEL_OPTION,null,options, options[0]);
					dialog = new JDialog(parentFrame,"Valid input string?",false);
					dialog.setContentPane(jop);
					
					// the following chunk is partly from http://java.sun.com/docs/books/tutorial/uiswing/components/dialog.html
					dialog.setDefaultCloseOperation(
						    WindowConstants.DO_NOTHING_ON_CLOSE);
					dialog.addWindowListener(new WindowAdapter() {
					    public void windowClosing(@SuppressWarnings("unused") WindowEvent we) {
					    	jop.setValue(new Integer(
                                    JOptionPane.CLOSED_OPTION));// from http://java.sun.com/docs/books/tutorial/uiswing/components/examples/CustomDialog.java
					    }
					});
					jop.addPropertyChangeListener(new PropertyChangeListener() {
				        public void propertyChange(PropertyChangeEvent e) {
				            String prop = e.getPropertyName();
				            
							Object value = e.getNewValue();

							if (dialog.isVisible() && e.getSource() == jop
					            		 && (prop.equals(JOptionPane.VALUE_PROPERTY))) 
							{
								int i = 0;for(;i < options.length && options[i] != value;++i);
									if (i == options.length)
										i = AbstractOracle.USER_CANCELLED;// nothing was chosen
									else
										i = AbstractOracle.USER_ACCEPTED-i; // to ensure that zero translates into USER_ACCEPTED and other choices into lower numbers 
									
								// one of the choices was made, determine which one and close the window
								answer.getAndSet( i );
								synchronized(answer)
								{
									answer.notifyAll();
								}

								dialog.setVisible(false);dialog.dispose();
				            }
				        }
				    });
					rejectElements.addListSelectionListener(new ListSelectionListener() {

						public void valueChanged(ListSelectionEvent e) {
							if (dialog.isVisible() && e.getSource() == rejectElements &&
									!e.getValueIsAdjusting() && !rejectElements.isSelectionEmpty())
							{
								answer.getAndSet( rejectElements.getLeadSelectionIndex() );
								synchronized(answer)
								{
									answer.notifyAll();
								}
								
								dialog.setVisible(false);dialog.dispose();
							}
						}
						
					});				
					dialog.pack();
					//rejectElements.setListData(questionList.toArray());
					dialog.setVisible(true);
				}
			});
			synchronized (answer) {
				while(answer.get() == AbstractOracle.USER_WAITINGFORSELECTION)
						answer.wait();// wait for a user to make a response
			}
		} catch (InvocationTargetException e) {
			e.printStackTrace();
			// if we cannot make a call, return a negative number - nothing do not know what else to do about it.
		}
		catch (InterruptedException e) {
			
			// if we are interrupted, return a negative number - nothing do not know what else to do about it.
		}
		if (answer.get() == AbstractOracle.USER_WAITINGFORSELECTION) // this one if an exception was thrown
			answer.getAndSet(AbstractOracle.USER_CANCELLED);
		return new Pair<Integer,String>(answer.get(),null);
	}
	
	public int getQuestionCounter() {
		return questionCounter;
	}

	public void setQuestionCounter(int questionCnt) {
		this.questionCounter = questionCnt;
	}

	protected static void dumpSets(String output, Collection<List<String>> sPlus, Collection<List<String>> sMinus)
	{	
		try
		{
			System.out.println("dumping sets");
			XMLEncoder encoder = new XMLEncoder(new BufferedOutputStream(new FileOutputStream(output)));
			encoder.writeObject(sPlus);
			encoder.writeObject(sMinus);
			encoder.close();
			throw new IllegalArgumentException("finished");
		}
		catch(FileNotFoundException e)
		{
			IllegalArgumentException ex = new IllegalArgumentException("failed to write output file");
			ex.initCause(e);throw ex;
		}		
	}
}
