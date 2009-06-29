package statechum.analysis.learning.profileStringExtractor;

import java.io.File;
import java.io.IOException;

import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.xml.sax.SAXException;

public class TraceToMethodString {
	
	public static void main(String[] args){
		File trace = new File(args[0]);
		SAXParserFactory factory = SAXParserFactory.newInstance();
		try{
			SAXParser parser = factory.newSAXParser();
			//parser.setProperty("http://xml.org/sax/features/validation", false);

			process(parser, trace);
		}
		catch(Exception e){
			e.printStackTrace();
			return;
		}
	}
	
	public static void process(SAXParser p, File trace) throws SAXException, IOException{
		SimpleSequenceHandler stackHandler = new SimpleSequenceHandler(trace);
		p.parse(trace, stackHandler);
	}

}
