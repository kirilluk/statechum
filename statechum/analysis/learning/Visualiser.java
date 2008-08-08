/*Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 
This file is part of StateChum

StateChum is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

StateChum is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with StateChum.  If not, see <http://www.gnu.org/licenses/>.
*/ 

package statechum.analysis.learning;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.lang.reflect.InvocationTargetException;
import java.util.*;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;

import edu.uci.ics.jung.visualization.*;
import edu.uci.ics.jung.visualization.contrib.*;
import edu.uci.ics.jung.visualization.control.DefaultModalGraphMouse;
import edu.uci.ics.jung.visualization.control.ModalGraphMouse;
import edu.uci.ics.jung.visualization.control.PickingGraphMousePlugin;
import edu.uci.ics.jung.visualization.control.ScalingGraphMousePlugin;
import edu.uci.ics.jung.graph.*;
import edu.uci.ics.jung.graph.decorators.*;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import statechum.DeterministicDirectedSparseGraph;
import statechum.JUConstants;
import statechum.analysis.learning.rpnicore.*;

import java.awt.*;
import java.awt.event.*;
import java.awt.geom.AffineTransform;
import java.beans.XMLDecoder;
import java.beans.XMLEncoder;

import javax.swing.*;

/* Graph layout loading/saving including most of XMLPersistingLayout is from Jung source code. 
 * 
 * Sample JVM arguments:
 * -ea -DVIZ_CONFIG=kirill_home -Dthreadnum=2 -Djava.library.path=linear/.libs -Xmx3500m
 * 
 Jung license included below,

 The license below is the BSD open-source license. See 
 http://www.opensource.org/licenses/bsd-license.php
 with:
 <OWNER> = Regents of the University of California and the JUNG Project
 <ORGANIZATION> = University of California
 <YEAR> = 2003 

 It allows redistribution of JUNG freely, albeit with acknowledgement of JUNG's being a component in the redistributed software. However, we would greatly appreciate if you can let us know what you are doing with JUNG.

 --
 THE JUNG LICENSE

 Copyright (c) 2003-2004,  Regents of the University of California and the JUNG Project 
 All rights reserved.

 Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
 
 * Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
 * Neither the name of the University of California nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

public class Visualiser extends JFrame implements Observer, Runnable,
		MouseListener {
	/**
	 * The version ID for serialisation.
	 */
	private static final long serialVersionUID = -6382530787840924374L;

	protected VisualizationViewer viewer = null;

	/** We'd like to store a number of graphs and switch between them, but 
	 * knowing the name (i.e. the layout) is not enough - we need to store
	 * graphs themselves, which is accomplished using this map.
	 */
	protected List<DirectedSparseGraph> graphs = new LinkedList<DirectedSparseGraph>();

	/** Current position in the above list. */
	protected int currentGraph;
	
	/**
	 * The name under which to store window information. The default value is
	 * null which inhibits saving of a layout.
	 */
	protected VIZ_PROPERTIES propName = null;

	/** A popup with Jung control choices. */
	JPopupMenu popupMenu;

	/**
	 * Public constructor
	 * 
	 * @param windowPropName
	 *            the name under which to store configuration information.
	 */
	public Visualiser(VIZ_PROPERTIES windowPropName) {
		super(GraphicsEnvironment.getLocalGraphicsEnvironment().getScreenDevices()
				[loadFrame(windowPropName).getScreenDevice()].getDefaultConfiguration());
		propName = windowPropName;
		
	}

	/**
	 * Public constructor, constructs a visualiser which cannot save or load a
	 * layout.
	 */
	public Visualiser() {
		propName = null;
	}

	/** Key bindings. */
	Map<Integer,Action> keyToActionMap = new TreeMap<Integer, Action>();
	
	/** Actions to switch to picking/transform mode. */
	Action pickAction, transformAction, persistAction;
	
	
	
	/** A kind action used by this interface. */
	public abstract class graphAction extends AbstractAction
	{
		public graphAction() {}
		public graphAction(String name, String description)
		{
			super(name);putValue(SHORT_DESCRIPTION, description);
		}
	}
	
	protected void setKeyBindings()
	{
		persistAction = new graphAction("saveLayout", "save the layout of the visible graph") {
			/** Serial number. */
			private static final long serialVersionUID = 1L;

			public void actionPerformed(@SuppressWarnings("unused") ActionEvent e) {
		        try {
		        	if (propName != null)
		        	{
		            	String fileName = getLayoutFileName(graphs.get(currentGraph));
		                XMLEncoder encoder = new XMLEncoder(new FileOutputStream(fileName));
		                Map<Integer,DoublePair> layout = ((XMLPersistingLayout) viewer.getModel().getGraphLayout()).persist();
		        		encoder.writeObject(layout);
		        		XMLAffineTransformSerialised trV = new XMLAffineTransformSerialised();
		        		trV.setFromAffineTransform(viewer.getViewTransformer().getTransform());encoder.writeObject(trV);
		        		XMLAffineTransformSerialised trL = new XMLAffineTransformSerialised();
		        		trL.setFromAffineTransform(viewer.getLayoutTransformer().getTransform());encoder.writeObject(trL);
		        		((XMLModalGraphMouse)viewer.getGraphMouse()).store(encoder);
		                encoder.close();
		        	}
		        } catch (Exception e1) {
		            e1.printStackTrace();
		        }		
			}
		};
		keyToActionMap.put(KeyEvent.VK_F2, persistAction);
		keyToActionMap.put(KeyEvent.VK_F3, new graphAction("loadLayout", "loads the previously saved layout the visible graph") {
			/** Serial number. */
			private static final long serialVersionUID = 2L;

			public void actionPerformed(@SuppressWarnings("unused") ActionEvent e) 
			{
		    		reloadLayout(false);
			}
		});
		keyToActionMap.put(KeyEvent.VK_F9, new graphAction("loadPreviousLayout", "loads the layout of the previous graph in the list") {
			/** Serial number. */
			private static final long serialVersionUID = 3L;

			public void actionPerformed(@SuppressWarnings("unused") ActionEvent e) 
			{
				if (currentGraph > 0)
					restoreLayout(false,currentGraph-1);
			}
		});
		keyToActionMap.put(KeyEvent.VK_F4, new graphAction("saveWindows", "save the current position/size of graph windows") {
			/** Serial number. */
			private static final long serialVersionUID = 4L;

			public void actionPerformed(@SuppressWarnings("unused") ActionEvent e) 
			{
				saveFrame(Visualiser.this, propName);
				saveConfiguration();
			}
		});
		keyToActionMap.put(KeyEvent.VK_ESCAPE, new graphAction("terminate", "terminates this program") {
			/** Serial number. */
			private static final long serialVersionUID = 5L;

			public void actionPerformed(@SuppressWarnings("unused")	ActionEvent e) 
			{
				setVisible(false);dispose();
				Visualiser.syncValue.set(true);
				synchronized (Visualiser.syncObject) {
					Visualiser.syncObject.notify();
				}
			}
		});
		keyToActionMap.put(KeyEvent.VK_SPACE, new graphAction("step", "exits the Visualiser.waitForKey() call") {
			/** Serial number. */
			private static final long serialVersionUID = 6L;

			public void actionPerformed(@SuppressWarnings("unused") ActionEvent e) 
			{
				Visualiser.syncValue.set(false);
				synchronized (Visualiser.syncObject) {
					Visualiser.syncObject.notify();
				}
			}
		});

		pickAction = new graphAction("pick", "Switches Jung into picking mode") {
			/** Serial number. */
			private static final long serialVersionUID = 7L;

			public void actionPerformed(@SuppressWarnings("unused") ActionEvent e) 
			{
				((XMLModalGraphMouse)viewer.getGraphMouse()).setMode(ModalGraphMouse.Mode.PICKING);
			}
		};
		keyToActionMap.put(KeyEvent.VK_F11,pickAction); 
		
		transformAction = new graphAction("transform", "Switches Jung into transformation mode") {
			/** Serial number. */
			private static final long serialVersionUID = 8L;

			public void actionPerformed(@SuppressWarnings("unused") ActionEvent e) 
			{
				((XMLModalGraphMouse)viewer.getGraphMouse()).setMode(ModalGraphMouse.Mode.TRANSFORMING);
			}
		};
		keyToActionMap.put(KeyEvent.VK_F12,transformAction); 

		keyToActionMap.put(KeyEvent.VK_UP, new graphAction("previous", "loads the previous graph") {
			/** Serial number. */
			private static final long serialVersionUID = 9L;

			public void actionPerformed(@SuppressWarnings("unused") ActionEvent e) 
			{
				if (currentGraph > 0)
				{
					--currentGraph;
					reloadLayout(false);
				}
			}
		});
		keyToActionMap.put(KeyEvent.VK_DOWN, new graphAction("next", "loads the next graph") {
			/** Serial number. */
			private static final long serialVersionUID = 10L;

			public void actionPerformed(@SuppressWarnings("unused") ActionEvent e) 
			{
				if (currentGraph < graphs.size()-1)
				{
					++currentGraph;
					reloadLayout(false);
				}
			}
		});
	}
	
	public void construct(Graph g) {
		boolean assertsEnabled = false;
		assert assertsEnabled = true; // from http://java.sun.com/j2se/1.5.0/docs/guide/language/assert.html
		if (!assertsEnabled && Boolean.getBoolean(getProperty(VIZ_PROPERTIES.ASSERT, "true")))
			System.err.println("Pass the -ea argument to JVM to enable assertions");

		this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		this.addKeyListener(new KeyListener() {

			public void keyPressed(KeyEvent arg0) {
				Action act = keyToActionMap.get(arg0.getKeyCode());
				if (act != null)
					act.actionPerformed(null);
			}

			public void keyReleased(@SuppressWarnings("unused") KeyEvent arg0) 
			{
			}

			public void keyTyped(@SuppressWarnings("unused") KeyEvent key) 
			{
			}
			
		});
        setSize(new Dimension(800,600));

		viewer = new VisualizationViewer( new DefaultVisualizationModel(new XMLPersistingLayout(
				propName != null? new FRLayout(g):new KKLayout(g))), constructRenderer(g) );
		viewer.setBackground(Color.WHITE);
		final DefaultModalGraphMouse graphMouse = new XMLModalGraphMouse();
		graphMouse.setMode(ModalGraphMouse.Mode.PICKING);
		graphMouse.add(new PickingGraphMousePlugin());
        viewer.setGraphMouse(graphMouse);
        viewer.setPickSupport(new ShapePickSupport());
        viewer.addMouseListener(this);
        popupMenu = new JPopupMenu();
        JMenuItem item = new JMenuItem("pick");item.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent e) {
				pickAction.actionPerformed(e);
			}
        });popupMenu.add(item);
        item = new JMenuItem("transform");item.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent e) {
				transformAction.actionPerformed(e);
			}
        });popupMenu.add(item);
        
		final GraphZoomScrollPane panel = new GraphZoomScrollPane(viewer);

		// Icon loading is from http://www.javaworld.com/javaworld/javaqa/2000-06/03-qa-0616-icon.html
		Image icon = Toolkit.getDefaultToolkit().getImage("resources"+System.getProperty("file.separator")+"icon.jpg");if (icon != null)	setIconImage(icon);

		setKeyBindings();
		//getContentPane().removeAll();
		getContentPane().add(panel);pack();
		restoreLayout(true,currentGraph);setBounds(loadFrame(propName).getRect());
        setVisible(true);
	}

	protected static class XMLModalGraphMouse extends DefaultModalGraphMouse
	{
		/** Restores parameters which are not recorderd in the view/layout transforms, from the supplied XML stream
	     * 
	     * @param decode XML decoder to use
	     */
		public void restore(XMLDecoder decoder)
		{
			((ScalingGraphMousePlugin)scalingPlugin).setIn( (Float)decoder.readObject());
			((ScalingGraphMousePlugin)scalingPlugin).setOut( (Float)decoder.readObject());
		}

		/** Stores parameters which are not recorderd in the view/layout transforms, to the supplied XML stream
	     * 
	     * @param encoder XML encoder to use
	     */
		public void store(XMLEncoder encoder)
		{
			encoder.writeObject( ((ScalingGraphMousePlugin)scalingPlugin).getIn());
			encoder.writeObject( ((ScalingGraphMousePlugin)scalingPlugin).getOut());
		}
	}
		
	/** Loads or reloads a graph. Used during initialisation on the Swing thread, 
	 * when graph changes or user hits F3 (reload).
	 * 
	 * @param ignoreErrors Whether to ignore loading errors - they are ignored 
	 * on auto-load, but honoured on user load.
	 */
	protected void reloadLayout(boolean ignoreErrors)
	{
		/** The graph currently being displayed. */
		final Graph graph = graphs.get(currentGraph);
		
		assert graph != null;
		String title = (String)graph.getUserDatum(JUConstants.TITLE)+" ("+(currentGraph+1)+"/"+graphs.size()+")"; 
		if (!wasInitialised)
		{
			construct(graph);
			setTitle(title);
			wasInitialised = true;
		}
		else
		{
			viewer.getModel().setGraphLayout( new XMLPersistingLayout(propName != null? new FRLayout(graph):new KKLayout(graph)) );
			setTitle(title);
			restoreLayout(ignoreErrors,currentGraph);
			viewer.setRenderer(constructRenderer(graph));
		}
	}
	
	/** Loads the layout of the specific graph in the list.
	 * 
	 *  @param whether to ignore loading errors.
	 *  @param graphNumber the number of the graph to load.
	 */
	protected void restoreLayout(boolean ignoreErrors, int graphNumber)
	{
        try {
        	String fileName = getLayoutFileName(graphs.get(graphNumber));
        	if (propName != null && (new File(fileName)).canRead())
        	{
    	    	XMLDecoder decoder = new XMLDecoder (new FileInputStream(fileName));
    	    	Map<Integer,DoublePair> map = (Map<Integer,DoublePair>)decoder.readObject();
        		((XMLPersistingLayout) viewer.getModel().getGraphLayout()).restore(map);
        		
        		// Most rotate/share/translate are stateless, so I only need to get the cumulative transform 
        		// for layout and view via getTransform() which should return AffineTransform 
        		// which I should be able to persist into XML.
        		// Only ScalingGraphMousePlugin has a state
        		// in the form of getIn()/setIn()/getOut()/setOut().
        		viewer.getViewTransformer().setToIdentity();viewer.getViewTransformer().concatenate( 
        				((XMLAffineTransformSerialised)decoder.readObject()).getAffineTransform() );
        		viewer.getLayoutTransformer().setToIdentity();viewer.getLayoutTransformer().concatenate( 
        				((XMLAffineTransformSerialised)decoder.readObject()).getAffineTransform() );
        		((XMLModalGraphMouse)viewer.getGraphMouse()).restore(decoder);
        		decoder.close();
        		
        		viewer.invalidate();
        	}
        } catch (Exception e1) {
        	if (!ignoreErrors)
        		e1.printStackTrace();
        }
	}
	
	/** Used to serialise graph layout information into XML where XMLEncoder requires a top-level class.
	 * 
	 * @author kirr
	 *
	 */
	public static class DoublePair {
		private double x,y;
		
		public DoublePair() 
		{
			x=0;y=0;
		}
		
		public DoublePair(double a,double b)
		{
			setX(a);setY(b);
		}
		
		public double getX()
		{
			return x;
		}
		
		public double getY()
		{
			return y;
		}
		
		public void setX(double a)
		{
			x=a;
		}
		
		public void setY(double b)
		{
			y=b;
		}
	}

	/** A serialised form of XMLAffineTransform
	 * 
	 * @author Kirill
	 *
	 */
	public static class XMLAffineTransformSerialised
	{
		protected double [] matrix;
		
		public double [] getM()
		{ 
			return matrix; 
		}
		
		public void setM(double []m)
		{
			matrix = m;
		}
		
		AffineTransform getAffineTransform()
		{
			return new AffineTransform(matrix);
		}
		
		void setFromAffineTransform(AffineTransform tr)
		{
			matrix=new double[6];
			tr.getMatrix(matrix);
		}
	}
		
	protected static PluggableRenderer constructRenderer(Graph g)
	{
		PluggableRenderer r = new PluggableRenderer();
		r = labelEdges(r);
		r = labelVertices(r,g);
		return r;
	}
	
	/** If the frame was not constructed, we have to construct instances of 
	 * all classes responsible for it; once it is build and only our 
	 * graph changed, it is enough to replace the layout to update the graph. */
	protected boolean wasInitialised = false;
		
	public void run()
	{
		reloadLayout(true);
	}

	public void update(final Observable s, Object arg){
		if(arg instanceof LearnerState){
			LearnerState lg = (LearnerState)arg;
			graphs.add( (DirectedSparseGraph)lg.getResult().paths.getGraph().copy() );
		}
		else{
			LearnerGraph lg = (LearnerGraph)arg;
			graphs.add( (DirectedSparseGraph)lg.paths.getGraph().copy() );
		}
		currentGraph = graphs.size()-1;
		SwingUtilities.invokeLater(this);
	}

	private static PluggableRenderer labelEdges(PluggableRenderer render){
		EdgeStringer stringer = new EdgeStringer(){
            public String getLabel(ArchetypeEdge e) {
            	String result = "";
            	
            	if(e.containsUserDatumKey(JUConstants.LABEL)){
            		HashSet<String> labels = (HashSet<String>)e.getUserDatum(JUConstants.LABEL);
            		Iterator<String> labelIt = labels.iterator();
            		String label = "[ ";
            		while(labelIt.hasNext()){
            			label = label.concat(labelIt.next()+" ");
            		}
            		result = label+" ]";
            	}
            	
            	return result;
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
			if (DeterministicDirectedSparseGraph.isInitial(v))
				return factory.getRegularStar(v, 7);
			else
				if ( !DeterministicDirectedSparseGraph.isAccept(v) )
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
			if (v.getUserDatum(JUConstants.HIGHLIGHT) != null)
				return Color.MAGENTA;
			
			return Color.BLACK;
		}

		public Paint getFillPaint(Vertex v) {
			Color col = Color.BLACK;
			
			if (picked.isPicked(v))
				col = Color.LIGHT_GRAY;
			else
			{
				JUConstants c = (JUConstants)v.getUserDatum(JUConstants.COLOUR);
				if (c == null)
					col = Color.GREEN;
				else
				{
					if (c == JUConstants.RED)
						col = Color.PINK;
					else
						if (c == JUConstants.BLUE)
							col = Color.BLUE;
						else
							if (c == JUConstants.AMBER)
								col = Color.YELLOW;
							else
								if (c == JUConstants.GRAY)
									col = Color.GRAY;
				}
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
	
	/** Holds the JFrame to see the graphs being dealt with. Usage:
	 * <pre>
	 * 		updateFrame(upper,lower);// a public method
	 * </pre>
	 * where <i>upper</i> and <i>lower</i> are the graphs to be displayed.
	 */
	static Visualiser upperGraphViz=null,lowerGraphViz=null;

	/** If a dialog box has to be displayed, it needs to know a reference frame. This method returns this frame, creating it if necessary.
	 * 
	 * @return a Visualiser frame.
	 */ 
	public static Visualiser getVisualiser()
	{
		if (upperGraphViz == null) upperGraphViz = new Visualiser(VIZ_PROPERTIES.UPPER);
		return upperGraphViz;
	}
	
	/** Removes the two windows displaying Jung graphs.
	 */
	public static void disposeFrame()
	{
		if (upperGraphViz != null && lowerGraphViz != null)
			try
			{
				SwingUtilities.invokeAndWait(new Runnable() 
				{
					public void run()
					{
						if (upperGraphViz != null)
						{
							upperGraphViz.setVisible(false);upperGraphViz.dispose();upperGraphViz=null;							
						}
						if (lowerGraphViz != null)
						{
							lowerGraphViz.setVisible(false);lowerGraphViz.dispose();lowerGraphViz=null;							
						}
					}
				});
			} catch (InterruptedException e) {
				// cannot do much about this
				e.printStackTrace();
			} catch (InvocationTargetException e) {
				// cannot do much about this
				e.printStackTrace();
			}
	}

	/** Displays twos graphs passed as arguments in the Jung window.
	 * This method is to be called for one (g != null) or two (g != null && lowerGraph != null) windows to be displayed;
	 * when both arguments are null, graphs are disposed of.
	 * 
	 * @param upperGraph the graph to display 
	 * @param lowerGraph the graph to display below it
	 */
	public static void updateFrame(final Object upperGraph,final Object lowerGraph)
	{
		if (upperGraph == null && lowerGraph == null)
		{// destruction of windows
			disposeFrame();
		}
		else
		{// construction of windows
			if (upperGraph == null)
				throw new IllegalArgumentException("the first graph to display cannot be null");
			
			if (upperGraphViz == null)
				upperGraphViz=new Visualiser(VIZ_PROPERTIES.UPPER);
	
			upperGraphViz.update(null, upperGraph);
			if (lowerGraph != null)
			{
				if (lowerGraphViz == null)
					lowerGraphViz = new Visualiser(VIZ_PROPERTIES.LOWER);
				lowerGraphViz.update(null, lowerGraph);
			}
		}
	}

	/** Used to make it possible to single-step through graph transforms. */
	final static Object syncObject = new Object();
	/** Value to return to a thread which is waiting. */
	final static AtomicBoolean syncValue = new AtomicBoolean();
	
	/** Waits for a user to hit space on any of the visualiser windows. 
	 * 
	 * @return true if user hit space, false if application is terminating.
	 */
	public static void waitForKey()
	{
		synchronized(syncObject)
		{
			try {
				syncObject.wait();
			} catch (InterruptedException e) {
				// assume we are terminating.
			}
		}
		
		if (syncValue.get())
			System.exit(1);
	}
	
	public enum VIZ_ENV_PROPERTIES {
		VIZ_DIR,// the path to visualisation-related information, such as graph layouts and configuration file.
		VIZ_CONFIG,// the configuration file containing window positions and whether to display an assert-related warning.
		VIZ_AUTOFILENAME // used to define a name of a file to load answers to questions
	}
	
	public enum VIZ_PROPERTIES { // internal properties
		ASSERT,// whether to display assert warning.
		LINEARWARNINGS,// whether to warn when external solver cannot be loaded and we have to fall back to the colt solver.
		BUILDGRAPH, // whether to break if the name of a graph to build is equal to a value of this property
		LOWER, UPPER // window positions, not real properties to be stored in a file.
		, STOP // used to stop execution - a walkaround re JUnit Eclipse bug on linux amd64.
		,GRAPHICS_MONITOR // the monitor to pop graphs on - useful when using multiple separate screens rather than xinerama or nview
		;
	}

	protected static Properties properties = null;
	
	protected static Map<String,WindowPosition> windowCoords = null;// if I index by VIZ_PROPERTIES, I cannot serialise into XML
	
	/** Default screen to use for any frames created. */
	public static final int DEFAULT_SCREEN = 0;
	
	/** Loads the location/size of a frame from the properties file and positions the frame as appropriate.
	 * 
	 * @param frame the frame to position.
	 * @param name the name of the property to load from
	 */   
	protected static WindowPosition loadFrame(VIZ_PROPERTIES name)
	{
		if (windowCoords == null)
			loadConfiguration();
		
		WindowPosition result = windowCoords.get(name.toString());
		
		if (result == null)
		{// invent default coordinates, using http://java.sun.com/j2se/1.5.0/docs/api/java/awt/GraphicsDevice.html#getDefaultConfiguration()

			GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
			GraphicsDevice[] gs = ge.getScreenDevices();
			int deviceToUse = Integer.valueOf(getProperty(VIZ_PROPERTIES.GRAPHICS_MONITOR, ""+DEFAULT_SCREEN));
			if (deviceToUse >= gs.length) deviceToUse =DEFAULT_SCREEN;// use the first one if cannot use the requested one.
			GraphicsConfiguration gc = gs[deviceToUse].getDefaultConfiguration();
			
			// from http://java.sun.com/j2se/1.4.2/docs/api/index.html
			Rectangle shape = gc.getBounds();
			Rectangle rect = new Rectangle(new Rectangle(shape.x, shape.y,400,300));
			if (name == VIZ_PROPERTIES.LOWER)
				rect.y+=rect.getHeight()+30;
			result = new WindowPosition(rect,deviceToUse);
			windowCoords.put(name.toString(),result);
		}
		
		return result;
	}
	
	/** Stores the current location/size of a frame to the properties file.
	 * 
	 * @param frame the frame to position.
	 * @param name the name under which to store the property
	 */   
	protected static void saveFrame(Frame frame,VIZ_PROPERTIES name)
	{
		WindowPosition windowPos = windowCoords.get(name.toString());if (windowPos == null) windowPos = new WindowPosition();
		Rectangle newRect = new Rectangle(frame.getSize());newRect.setLocation(frame.getX(), frame.getY());
		windowPos.setRect(newRect);
		windowCoords.put(name.toString(), windowPos);
	}

	/** Retrieves the name of the file to load a graph layout from/store layout to.
	 * 
	 * @param g the graph which name to extract
	 * @return the file name to load/store layout information, or throw IllegalArgumentException if it cannot be retrieved. 
	 */
	protected static String getLayoutFileName(Graph g)
	{
		String file = (String)g.getUserDatum(JUConstants.TITLE);
		String path = System.getProperty(VIZ_ENV_PROPERTIES.VIZ_DIR.toString());if (path == null) path="resources"+System.getProperty("file.separator")+"graphLayout";
		if (file == null)
			throw new IllegalArgumentException("cannot obtain graph name, the "+JUConstants.TITLE.toString()+" property has not been set on the graph");
		
		return path+System.getProperty("file.separator")+file.replace(' ', '_')+".xml";
	}
	
	/** Retrieves the name of the file to load configuration information from/store it to.
	 * 
	 * @return the file name to load/store configuration information information, or return null if it cannot be retrieved. 
	 */
	protected static String getConfigurationFileName()
	{
		String path = System.getProperty(VIZ_ENV_PROPERTIES.VIZ_DIR.toString());if (path == null) path="resources"+System.getProperty("file.separator")+"graphLayout";
		String file = System.getProperty(VIZ_ENV_PROPERTIES.VIZ_CONFIG.toString());
		String result = null;
		if (file != null)
			result = path+System.getProperty("file.separator")+file+".xml";
		
		return result;
	}	
	
	/** Retrieves the name of the property from the property file.
	 *  The first call to this method opens the property file.
	 *  
	 * @param name the name of the property.
	 * @param defaultValue the default value of the property
	 * @return property value, default value if not found
	 */
	public static String getProperty(VIZ_PROPERTIES name,String defaultValue)
	{
		String result = defaultValue;
		if (properties == null)
			loadConfiguration();
		result = properties.getProperty(name.toString(), defaultValue);
		return result;
	}

	/** Stores the details of the frame position. 
	 */
	public static class WindowPosition
	{
		private Rectangle rect = null;
		private int screenDevice;
		
		public WindowPosition() {}

		public WindowPosition(Rectangle r, int s)
		{
			rect = r;screenDevice = s;
		}
		
		public Rectangle getRect() {
			return rect;
		}

		public void setRect(Rectangle r) {
			this.rect = r;
		}

		public int getScreenDevice() {
			return screenDevice;
		}

		public void setScreenDevice(int screen) {
			this.screenDevice = screen;
		}

		
	}
	protected static void loadConfiguration()
	{
		String configFileName = getConfigurationFileName();
		if (configFileName == null || ! new File(configFileName).canRead())
		{
			System.err.println("Configuration file "+configFileName+" does not exist.");
		}
		else
		try 
		{
			XMLDecoder decoder = new XMLDecoder(new FileInputStream(configFileName));
			properties = (Properties) decoder.readObject();
			windowCoords = (HashMap<String, WindowPosition>) decoder.readObject();
			decoder.close();
		} catch (Exception e) 
		{// failed loading, (almost) ignore this.
			System.err.println("Failed to load "+configFileName);
			e.printStackTrace();
		}
		
		if (windowCoords == null)
			windowCoords = new HashMap<String, WindowPosition>();
		if (properties == null)
			properties = new Properties();
	}
	
	/** Saves all the current properties in the configuration file. */
	protected static void saveConfiguration()
	{
		String configFileName = getConfigurationFileName();
		if (windowCoords == null)
			windowCoords = new HashMap<String, WindowPosition>();
		if (properties == null)
			properties = new Properties();

		try {
			if (configFileName != null)
			{
				XMLEncoder encoder = new XMLEncoder(new FileOutputStream(configFileName));
				encoder.writeObject(properties);encoder.writeObject(windowCoords);encoder.close();
			}
		} catch (Exception e) 
		{// failed loading
			e.printStackTrace();
		}		
	}

	/** Returns true if the configuration file defines the name of the supplied graph as the one 
	 * transformation of which we should be looking at in detail.
	 * 
	 * @param graph the graph we might wish to look at in detail
	 * @return whether lots of debug output should be enable when this graph is being processed.
	 */
	public static boolean isGraphTransformationDebug(DirectedSparseGraph graph)
	{
		String name = graph == null? null:(String)graph.getUserDatum(JUConstants.TITLE);
		return name != null && name.length()>0 && 
			Visualiser.getProperty(Visualiser.VIZ_PROPERTIES.STOP, "").equals(name);
	}

	protected static class XMLPersistingLayout extends PersistentLayoutImpl
	{

		public XMLPersistingLayout(Layout layout) {
			super(layout);
		}
				
	    /** Almost verbatim from Jung source code.
	     * 
	     * Saves all the vertex locations to a map - this is enough to rebuild the layout 
	     * later since we'll know the connections later anyway.
	     * 
	     * @return map containing the layout.
	     */
	    public Map<Integer,DoublePair> persist() {
	    	if (sourceMap == null)
	    		sourceMap = new TreeMap<Integer,DoublePair>();
	        Set set = getGraph().getVertices();
	        for (Iterator iterator = set.iterator(); iterator.hasNext();) 
	        {
	            Vertex v = (Vertex) iterator.next();
	            DoublePair p = new DoublePair(getX(v), getY(v));
	            sourceMap.put(new Integer(v.hashCode()), p);
	        }
	        //encoder.writeObject(sourceMap);
	        return sourceMap;
	    }

	    /** Stores the layout loaded from a file. The idea is to merge it with new one before 
	     * storing it back. This permits storing positions of vertices not in the layout,
	     * thus permitting the same layout file to be used for different graphs. */
	    private Map<Integer,DoublePair> sourceMap = null;
	    
	    /** Almost verbatim from Jung source code.
	     * Restores all vertex locations from a file; does nothing if loading fails.
	     * 
	     * @param map to load from
	     */
	    public void restore(Map<Integer,DoublePair> loadedMap) 
	    {
	    	if (sourceMap == null)
	    		sourceMap = new TreeMap<Integer,DoublePair>();
	    	//Map<Integer,DoublePair> loadedMap = (Map<Integer,DoublePair>) decoder.readObject();
	    	sourceMap.putAll(loadedMap);
	        for(Iterator<Map.Entry<Integer,DoublePair> > mi=sourceMap.entrySet().iterator();mi.hasNext();)
	        {
	        	Map.Entry<Integer, DoublePair> e = mi.next();DoublePair p = e.getValue();
	        	map.put(e.getKey(), new PersistentLayout.Point(p.getX(),p.getY()));
	        }
	        initializeLocations();
	        locked = true;
	    }
	}


	public void mouseClicked(@SuppressWarnings("unused") MouseEvent e) {
	}

	public void mouseEntered(@SuppressWarnings("unused") MouseEvent e) {
	}

	public void mouseExited(@SuppressWarnings("unused") MouseEvent e) {
	}

	// The following is from http://java.sun.com/docs/books/tutorial/uiswing/components/menu.html#popup
	
	public void mousePressed(MouseEvent e) {
		maybeShowPopup(e);
	}

	public void mouseReleased(MouseEvent e) 
	{
		maybeShowPopup(e);
	}
	private void maybeShowPopup(MouseEvent e) {
        if (e.isPopupTrigger()) {
            popupMenu.show(e.getComponent(),
                       e.getX(), e.getY());
        }
    }
}
