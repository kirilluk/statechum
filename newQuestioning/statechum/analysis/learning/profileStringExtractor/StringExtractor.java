package statechum.analysis.learning.profileStringExtractor;

import javax.swing.SwingUtilities;

public class StringExtractor {

	public static void main(String[] args){
		final String [] commandlineArgs = args; 
		final SplitFrame sFrame = new SplitFrame();
		
		if (args.length > 0)
			SwingUtilities.invokeLater(new Runnable() {
				public void run() {
					sFrame.runAnExperiment(commandlineArgs);
				}
			});
	}
	
}
