package statechum.analysis.learning.spin;

import statechum.analysis.learning.RPNIBlueFringeLearnerTestComponentOpt;
import java.awt.Frame;
import java.util.*;

public class BlueFringeSpinLearner extends RPNIBlueFringeLearnerTestComponentOpt {
	
	public BlueFringeSpinLearner(Frame parentFrame, Set<String> ltlFormulae){
		super(parentFrame);
		scoreComputer = new ComputeSpinStateScores(0, ltlFormulae);
	}

}
