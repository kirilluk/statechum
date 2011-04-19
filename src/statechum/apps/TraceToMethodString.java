package statechum.apps;

import java.io.File;
import java.io.IOException;

import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.xml.sax.SAXException;

import statechum.analysis.learning.profileStringExtractor.SimpleSequenceHandler;

public class TraceToMethodString {
	
	public static void main(String[] args){
		File trace = new File(args[0]);
		String thread = args[1];
		SAXParserFactory factory = SAXParserFactory.newInstance();
		try{
			SAXParser parser = factory.newSAXParser();
			//parser.setProperty("http://xml.org/sax/features/validation", false);

			process(parser, trace, thread);
		}
		catch(Exception e){
			e.printStackTrace();
			return;
		}
	}
	
	public static void process(SAXParser p, File trace, String thread) throws SAXException, IOException{
		SimpleSequenceHandler stackHandler = new SimpleSequenceHandler(trace, thread);
		p.parse(trace, stackHandler);
	}

}
