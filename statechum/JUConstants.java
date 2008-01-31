package statechum;

public enum JUConstants {
	LABEL("label"),
	ACCEPTED("accepted"),
	TITLE("title"),
	INITIAL("init"),// whether a vertex is an initial state
	HIGHLIGHT("highlight"),
	STATS("STATS"),
	COLOUR("colour"),
	RED("red"),BLUE("blue"),
	JUNKVERTEX("junk");// used for testing that searching for a property that does not exist returns a null vertex.
	
	private String stringRepresentation;
	
	JUConstants(String textualName)
	{
		stringRepresentation = textualName;
	}
	
	public String toString()
	{
		return stringRepresentation;
	}
}
