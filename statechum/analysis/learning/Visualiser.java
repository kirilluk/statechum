package statechum.analysis.learning;

import java.util.*;
import edu.uci.ics.jung.visualization.*;
import edu.uci.ics.jung.visualization.contrib.*;
import edu.uci.ics.jung.visualization.control.DefaultModalGraphMouse;
import edu.uci.ics.jung.visualization.control.ModalGraphMouse;
import edu.uci.ics.jung.visualization.control.PickingGraphMousePlugin;
import edu.uci.ics.jung.graph.*;
import edu.uci.ics.jung.graph.decorators.*;
import statechum.JUConstants;
import java.awt.*;
import java.awt.event.*;

import javax.swing.*;

public class Visualiser extends JFrame implements Observer,Runnable, MouseListener  {
	
	/**
	 * The version ID for serialization.
	 */
	private static final long serialVersionUID = -6382530787840924374L;

	protected VisualizationViewer viewer = null;
	
	protected void construct(Graph g)
	{
		boolean assertsEnabled = false;
		assert assertsEnabled = true; // from http://java.sun.com/j2se/1.5.0/docs/guide/language/assert.html
		//if (!assertsEnabled)
			//System.err.println("Pass the -ea argument to JVM to enable assertions");

		this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		this.addKeyListener(new KeyListener() {

			public void keyPressed(KeyEvent arg0) {
			}

			public void keyReleased(KeyEvent arg0) {
			}

			public void keyTyped(KeyEvent key) {
				if (key.getKeyChar() == KeyEvent.VK_ESCAPE)
				{
					setVisible(false);dispose();
				}
			}
			
		});
        setSize(new Dimension(800,600));

		viewer = new VisualizationViewer( new DefaultVisualizationModel(new KKLayout(g)), constructRenderer(g) );
		viewer.setBackground(Color.WHITE);
		final DefaultModalGraphMouse graphMouse = new DefaultModalGraphMouse();
		graphMouse.setMode(ModalGraphMouse.Mode.PICKING);
		PickingGraphMousePlugin picker = new PickingGraphMousePlugin();
		graphMouse.add(new PickingGraphMousePlugin());
        viewer.setGraphMouse(graphMouse);
        viewer.setPickSupport(new ShapePickSupport());
        viewer.addMouseListener(this);
		final GraphZoomScrollPane panel = new GraphZoomScrollPane(viewer);
		//getContentPane().removeAll();
		getContentPane().add(panel);
        setVisible(true);
	}
	
	protected static PluggableRenderer constructRenderer(Graph g)
	{
		PluggableRenderer r = new PluggableRenderer();
		r = labelEdges(r);
		r = labelVertices(r,g);
		return r;
	}
	
	/** Until the first update, Jung window is not shown. */
	protected boolean wasInitialised = false;
	
	/** The graph currently being displayed, null if none is being displayed. */
	protected Graph graph = null;
	
	public void run()
	{
		assert(graph != null);
		if (!wasInitialised)
		{
			construct(graph);
			setTitle((String)graph.getUserDatum(JUConstants.TITLE));
			wasInitialised = true;
		}
		else
		{
			viewer.getModel().setGraphLayout( new KKLayout( graph ) );
			setTitle((String)graph.getUserDatum(JUConstants.TITLE));
			viewer.setRenderer(constructRenderer(graph));
		}
	}

	public void update(final Observable s, Object arg){
		graph = (Graph)((Graph)arg).copy();
		SwingUtilities.invokeLater(this);
	}

	
	private static PluggableRenderer labelEdges(PluggableRenderer render){
		EdgeStringer stringer = new EdgeStringer(){
            public String getLabel(ArchetypeEdge e) {
            	if(e.containsUserDatumKey(JUConstants.LABEL)){
            		HashSet<String> labels = (HashSet<String>)e.getUserDatum(JUConstants.LABEL);
            		Iterator<String> labelIt = labels.iterator();
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
				if ( !(new Boolean(v.getUserDatum(JUConstants.ACCEPTED).toString())).booleanValue() )
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
			if (v.getUserDatum(JUConstants.COLOUR) == null)
				col = Color.GREEN;
			else
			{
				String c = (String)v.getUserDatum(JUConstants.COLOUR);
				if (c == "red")
					col = Color.PINK;
				else
					if (c == "blue")
						col = Color.BLUE;
			}		
			return col;
		}
		
	}
	
	private static PluggableRenderer labelVertices(PluggableRenderer r, Graph graph){
		StringLabeller labeller = StringLabeller.getLabeller(graph,"name");
		labeller.clear();
		Iterator labelIt = graph.getVertices().iterator();
		while(labelIt.hasNext()){
			Vertex v = (Vertex)labelIt.next();
			try{
				Object label = v.getUserDatum(JUConstants.LABEL);
				if (label != null)
					labeller.setLabel(v,label.toString());
			}
			catch(Exception e){
				System.out.println(e);
				e.printStackTrace();
			}
		}
		r.setVertexStringer(labeller);		
		r.setVertexShapeFunction(new VertexShape());
		r.setVertexPaintFunction(new VertexPaint(r));
		r.setVertexStrokeFunction(new ConstantVertexStrokeFunction(2.0f));
		return r;
	}
	
	
	
	public void mouseClicked(MouseEvent e) {
		
	}

	public void mouseEntered(MouseEvent e) {

		
	}

	public void mouseExited(MouseEvent e) {

		
	}

	public void mousePressed(MouseEvent e) {

		
	}

	public void mouseReleased(MouseEvent e) {
	}
}
