package statechum.analysis.learning.findloops;

import statechum.analysis.learning.profileStringExtractor.*;

import java.io.*;
import javax.xml.parsers.*;

import javax.xml.parsers.SAXParserFactory;


/**
 * This class is responsible for taking a series of tptp profile sequences and converting them to 
 * observations that can be read by findloops (Jourdan et al, Forte 2007).
 * @author Neil Walkinshaw
 *
 */

public class TptpToFindLoops {
	
	/**
	 * @param args
	 * first argument is the name of the tptp trace file (decompressed)
	 * second argument is the name of the findloops output file
	 */
	public static void main (String[] args){
		File tptpFile = new File(args[0]);
		File[] extractorArray = new File[1];
		extractorArray[0]=tptpFile;
		Extractor extractor = new Extractor(extractorArray);
		ClassMethodDefsHandler handler = extractor.getFileToHandler().get(tptpFile);
		TranslatorParser translator = new TranslatorParser(args[1],handler);
		SAXParserFactory factory = SAXParserFactory.newInstance();
		try{
			SAXParser parser = factory.newSAXParser();
			parser.parse(tptpFile, translator);
		}
		catch(Exception e){
			e.printStackTrace();
		}
	}
	
}
