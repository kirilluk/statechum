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

package statechum.analysis.learning;

import java.awt.Frame;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.lang.reflect.InvocationTargetException;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;

import javax.swing.*;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import statechum.Configuration;
import statechum.Pair;
import statechum.analysis.learning.observers.Learner;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.model.testset.PTASequenceEngine;

public abstract class RPNILearner extends Observable implements Learner {
	protected final Configuration config;
	
	/** The frame in relation to which to pop dialog boxes. */
	protected Frame parentFrame;

	public RPNILearner(Frame parent, Configuration c) 
	{
		config = c;
		parentFrame = parent;
	}
	
	/** Retrieves the configuration used by this learner. */
	public Configuration getConfig()
	{
		return config;
	}
	
	/** Initialises this learner. */
	abstract public LearnerGraph init(Collection<List<String>> plus, Collection<List<String>> minus);

	/** Initialises this learner. */
	abstract public LearnerGraph init(PTASequenceEngine en, int plus, int minus);

	public LearnerGraph learnMachine(PTASequenceEngine en, int plusSize, int minusSize) 
	{
		init(en, plusSize, minusSize);return learnMachine();
	}
	
	public LearnerGraph learnMachine(Collection<List<String>> plus, Collection<List<String>> minus)
	{
		topLevelListener.init(plus,minus);return learnMachine();
	}

	protected Learner topLevelListener = this;
	
	/** Sets the highest listener to receive notification calls; 
	 * it is expected that listeners will propagate calls down the chain
	 * so that this learner eventually gets to execute its own methods.
	 * 
	 * @param top new top of the stack of listeners.
	 */
	public void setTopLevelListener(Learner top)
	{
		topLevelListener = top;
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
	

	final String questionPrefix="";
	
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
	public void terminateUserDialogueFrame()
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
	
	public final static String QUESTION_AUTO = "<auto>"; 
	public final static String QUESTION_SPIN = "<spin>"; 
	public final static String QUESTION_USER = "<USER>"; 
	
	public void Restart(@SuppressWarnings("unused") RestartLearningEnum mode) 
	{// this method is used to let observers know what is going on, the actual restarts are handled by the main learner routine.
	}
	
	/** Where we expect a specific value in order to proceed without a restart, the answer is
	 * highlighted in the user-displayed dialog box.
	 * 
	 * @param str string to augment
	 * @return resulting value
	 */
	static String addAnnotationExpected(String str)
	{
		if (str.length() < 1)
			return str;
		
		return "<html><font color=blue>"+str.charAt(0)+"</font>"+str.substring(1);
	}
	
	/** Displays a tentative graph and asks user a supplied question. 
	 * Options are to be shown as choices in addition to yes/element_not_accepted. 
	 */
	public Pair<Integer,String> CheckWithEndUser(LearnerGraph model,final List<String> question, final int expectedForNoRestart, 
			final int lengthInHardFacts, final Object [] moreOptions)
	{
		final List<String> questionList = beautifyQuestionList(question);
		final AtomicInteger answer = new AtomicInteger(AbstractOracle.USER_WAITINGFORSELECTION);
		updateGraph(model);
		try {
			SwingUtilities.invokeAndWait(new Runnable() {
				public void run() {
					final Object[] options = new Object[1+moreOptions.length];
					JList nonrejectElements = null;
					Iterator<String> inputIter = questionList.iterator();
					int inputCounter=0;
					if (lengthInHardFacts >= 0)
					{
						assert lengthInHardFacts < questionList.size();
						List<String> nonrejects = new ArrayList<String>(lengthInHardFacts+1);
						for(;inputCounter<lengthInHardFacts;++inputCounter) nonrejects.add("<html><font color=gray>"+inputIter.next());
						nonrejectElements = new JList(nonrejects.toArray());
					}
					final int RejectsStartFrom = inputCounter;
					List<String> rejects = new ArrayList<String>(questionList.size());
					assert expectedForNoRestart < 0 || expectedForNoRestart >= lengthInHardFacts: "expectedForNoRestart = "+expectedForNoRestart+" and lengthInHardFacts = "+lengthInHardFacts;
					for(;inputCounter<questionList.size();++inputCounter)
					{
						String inputText = "<html><font color=green>"+inputIter.next();
						if (inputCounter == expectedForNoRestart)
							rejects.add(addAnnotationExpected(inputText));
						else
							rejects.add(inputText);
					}
					final JList rejectElements = new JList(rejects.toArray());
					
					options[0]="Accept";if (expectedForNoRestart == AbstractOracle.USER_ACCEPTED) options[0] = addAnnotationExpected((String)options[0]);
					System.arraycopy(moreOptions, 0, options, 1, moreOptions.length);
					final JLabel label = new JLabel("<html><font color=red>Click on the first non-accepting element below", SwingConstants.CENTER);
					jop = new JOptionPane((nonrejectElements == null? new Object[] {label,rejectElements}:new Object[] {label,nonrejectElements,rejectElements}),
			                JOptionPane.QUESTION_MESSAGE,JOptionPane.YES_NO_CANCEL_OPTION,null,options, options[0]);
					dialog = new JDialog(parentFrame,"Valid input string?",false);
					dialog.setContentPane(jop);
					
					// the following chunk is partly from http://java.sun.com/docs/books/tutorial/uiswing/components/dialog.html
					dialog.setDefaultCloseOperation(
						    WindowConstants.DO_NOTHING_ON_CLOSE);
					dialog.addWindowListener(new WindowAdapter() {
					    @Override
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
								answer.getAndSet( RejectsStartFrom+rejectElements.getLeadSelectionIndex() );
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
}
