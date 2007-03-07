package statechum.analysis.learning;

import java.util.*;
import edu.uci.ics.jung.visualization.*;
import edu.uci.ics.jung.visualization.contrib.*;
import edu.uci.ics.jung.visualization.control.DefaultModalGraphMouse;
import edu.uci.ics.jung.visualization.control.ModalGraphMouse;
import edu.uci.ics.jung.graph.*;
import edu.uci.ics.jung.graph.decorators.*;
import statechum.analysis.learning.profileStringExtractor.*;
import java.awt.*;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

import javax.swing.*;

public class Visualiser extends JFrame implements Observer  {
	
	
	
	public Visualiser(HashSet sPlus, HashSet sMinus, SplitFrame split){
		this.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
		this.addKeyListener(new KeyListener() {

			public void keyPressed(KeyEvent arg0) {
			}

			public void keyReleased(KeyEvent arg0) {
			}

			public void keyTyped(KeyEvent key) {
				if (key.getKeyChar() == KeyEvent.VK_ESCAPE)
					dispose();
			}
			
		});
        this.setTitle("Hypothesis Machine");
        setSize(new Dimension(800,600));
        setVisible(true);
    	RPNIBlueFringeLearnerTestComponent l = new RPNIBlueFringeLearnerTestComponent(split);
    	l.addObserver(this);
    	l.learnMachine(RPNIBlueFringeLearner.initialise(), sPlus, sMinus, 2);
	}
	
	public Visualiser(HashSet sPlus, HashSet sMinus){
		this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		this.addKeyListener(new KeyListener() {

			public void keyPressed(KeyEvent arg0) {
			}

			public void keyReleased(KeyEvent arg0) {
			}

			public void keyTyped(KeyEvent key) {
				if (key.getKeyChar() == KeyEvent.VK_ESCAPE)
					dispose();
			}
			
		});
        setSize(new Dimension(800,600));
        setVisible(true);
    	RPNIBlueFringeLearner l = new RPNIBlueFringeLearner();
    	l.addObserver(this);
    	l.learnMachine(RPNIBlueFringeLearner.initialise(), sPlus, sMinus);
	}
	
	public void update(Observable s, Object arg){
		Learner learner = (Learner)s;
		Graph g = learner.getGraph();
		Layout l = new KKLayout(g);
		PluggableRenderer r = new PluggableRenderer();
		r = labelEdges(r);
		r = labelVertices(r,g);
		VisualizationViewer v = new VisualizationViewer( l, r, new Dimension(800,600));
		v.setBackground(Color.WHITE);
		final DefaultModalGraphMouse graphMouse = new DefaultModalGraphMouse();
		graphMouse.setMode(ModalGraphMouse.Mode.PICKING);
        v.setGraphMouse(graphMouse);
		final GraphZoomScrollPane panel = new GraphZoomScrollPane(v);
		getContentPane().removeAll();
		getContentPane().add(panel);
		this.validate();
	}

	
	private  PluggableRenderer labelEdges(PluggableRenderer render){
		EdgeStringer stringer = new EdgeStringer(){
            public String getLabel(ArchetypeEdge e) {
            	if(e.containsUserDatumKey("label")){
            		HashSet<String> labels = (HashSet<String>)e.getUserDatum("label");
            		Iterator labelIt = labels.iterator();
            		String label = "[ ";
            		while(labelIt.hasNext()){
            			label = label.concat(labelIt.next()+" ");
            		}
            		return label+" ]";
            	}
            	else return "";
            }
        };
        render.setEdgeStringer(stringer);
        return render;
	}
	
	static class VertexShape extends AbstractVertexShapeFunction
	{

		public VertexShape() {
			super(  new ConstantVertexSizeFunction(25),
	                new ConstantVertexAspectRatioFunction(1.0f));
		}
		
		public Shape getShape(Vertex v) {
			if (v.getUserDatum("property") != null &&
					v.getUserDatum("property").equals("init"))
				return factory.getRegularStar(v, 7);
			else
				if ( !(new Boolean(v.getUserDatum("accepted").toString())).booleanValue() )
					return factory.getRectangle(v);
			return factory.getEllipse(v);
		}
	}
	
	static class VertexPaint implements VertexPaintFunction
	{
		protected final PickedInfo picked;
		
		public VertexPaint(PickedInfo p) {
			picked = p;
		}
		
		public Paint getDrawPaint(Vertex v) {
			if (v.getUserDatum("pair") != null)
				return Color.MAGENTA;
			
			return Color.BLACK;
		}

		public Paint getFillPaint(Vertex v) {
			Color col = Color.BLACK;
			
			if (picked.isPicked(v))
				col = Color.LIGHT_GRAY;
			else
			if (v.getUserDatum("colour") == null)
				col = Color.GREEN;
			else
			{
				String c = (String)v.getUserDatum("colour");
				if (c == "red")
					col = Color.PINK;
				else
					if (c == "blue")
						col = Color.BLUE;
			}		
			return col;
		}
		
	}
	
	private  PluggableRenderer labelVertices(PluggableRenderer r, Graph graph){
		StringLabeller labeller = StringLabeller.getLabeller(graph,"name");
		Iterator labelIt = graph.getVertices().iterator();
		while(labelIt.hasNext()){
			Vertex v = (Vertex)labelIt.next();
			try{
				String label = v.getUserDatum("label").toString();
				labeller.setLabel(v,label);
			}
			catch(Exception e){
				System.out.println(e);
			}
		}
		r.setVertexStringer(labeller);		
		r.setVertexShapeFunction(new VertexShape());
		r.setVertexPaintFunction(new VertexPaint(r));
		r.setVertexStrokeFunction(new ConstantVertexStrokeFunction(2.0f));
		return r;
	}
}
