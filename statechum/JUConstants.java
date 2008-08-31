package statechum;

public interface JUConstants {
	final String LABEL = "label";
	final String ACCEPTED = "accepted";
	final String TITLE = "title";
	final String TRUE = "true";
	final String FALSE = "false";
	final String PROPERTY = "property";
	final String INIT = "init";
	final String STATS = "STATS";
	final String COLOUR = "COLOUR";
	final String RED = "RED",BLUE = "BLUE",AMBER = "AMBER",GRAY = "GRAY",
	JUNKVERTEX = "junk",// used for testing that searching for a property that does not exist returns a null vertex.
	EDGE = "edge",VERTEX = "vertex"; // used for labelling vertices
}
