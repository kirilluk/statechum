package statechum.analysis.learning.profileStringExtractor;

import java.io.*;
import javax.swing.*;

public class StringExtractor {

	public static void main(String[] args){
		JFileChooser fc = new JFileChooser();
		fc.setMultiSelectionEnabled(true);
		fc.setFileSelectionMode(JFileChooser.FILES_ONLY);
		JFrame frame = new JFrame();
		frame.setVisible(true);
		int choice = fc.showDialog(frame, "Select XML files");
		if(choice == JFileChooser.APPROVE_OPTION){
			File[] file = fc.getSelectedFiles();
			Extractor e = new Extractor(file);
		}
		frame.dispose();
	}
	
}
