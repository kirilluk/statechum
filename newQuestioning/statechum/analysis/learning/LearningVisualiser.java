package statechum.analysis.learning;

import java.util.List;
import java.util.Set;

import statechum.analysis.learning.profileStringExtractor.SplitFrame;

public class LearningVisualiser extends Visualiser {
    /**
	 *  ID for serialization.
	 */
	private static final long serialVersionUID = 4842027206398108774L;
	
	//"Hypothesis Machine"
	public void construct(final Set<List<String>> sPlus, final Set<List<String>> sMinus, final SplitFrame split)
    {
	   	new Thread(new Runnable()
		{
			public void run()
			{
					RPNIBlueFringeLearner l = null;
		        	if (split != null) 
		        		l = new RPNIBlueFringeLearnerTestComponent(LearningVisualiser.this);
		        	else
		        		l = new RPNIBlueFringeLearner(LearningVisualiser.this);
		        		
		        	l.addObserver(LearningVisualiser.this);
		        	l.learnMachine(RPNIBlueFringeLearner.initialise(), sPlus, sMinus, 0);
			}
		},"RPNI learner thread").start();
    }
}
