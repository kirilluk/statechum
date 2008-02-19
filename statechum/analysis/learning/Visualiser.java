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
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.*;
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
import statechum.JUConstants;
import java.awt.*;
import java.awt.event.*;
import java.awt.geom.AffineTransform;
import java.beans.XMLDecoder;
import java.beans.XMLEncoder;

import javax.swing.*;

/* Graph layout loading/saving including most of XMLPersistingLayout is from Jung source code. 
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
	 * The version ID for serialization.
	 */
	private static final long serialVersionUID = -6382530787840924374L;

	protected VisualizationViewer viewer = null;

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
		propName = windowPropName;
	}

	/**
	 * Public constructor, constructs a visualiser which cannot save or load a
	 * layout.
	 */
	public Visualiser() {
		propName = null;
	}

	protected void construct(Graph g) {
		boolean assertsEnabled = false;
		assert assertsEnabled = true; // from
										// http://java.sun.com/j2se/1.5.0/docs/guide/language/assert.html
		if (!assertsEnabled
				&& Boolean
						.getBoolean(getProperty(VIZ_PROPERTIES.ASSERT, "true")))
			System.err
					.println("Pass the -ea argument to JVM to enable assertions");

		this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		this.addKeyListener(new KeyListener() {

			public void keyPressed(KeyEvent arg0) {
				switch (arg0.getKeyCode()) {
				case KeyEvent.VK_F2:// save the current layout
					storeLayout();
					break;
				case KeyEvent.VK_F3:// load a layout
					reloadLayout(false);
					break;
				case KeyEvent.VK_F4:// save the current window position
					saveFrame(Visualiser.this, propName);
					saveConfiguration();
					break;
				}
			}

			public void keyReleased(KeyEvent arg0) {
			}

			public void keyTyped(KeyEvent key) {
				switch (key.getKeyChar()) {
				case KeyEvent.VK_ESCAPE:// terminate
					setVisible(false);
					dispose();
					Visualiser.syncValue.set(true);
					synchronized (Visualiser.syncObject) {
						Visualiser.syncObject.notify();
					}
					break;
				case KeyEvent.VK_SPACE:// single-step
					Visualiser.syncValue.set(false);
					synchronized (Visualiser.syncObject) {
						Visualiser.syncObject.notify();
					}
					break;
				}
			}

		});
		setSize(new Dimension(800, 600));

		viewer = new VisualizationViewer(new DefaultVisualizationModel(
				new XMLPersistingLayout(propName != null ? new FRLayout(g)
						: new KKLayout(g))), constructRenderer(g));
		viewer.setBackground(Color.WHITE);
		final DefaultModalGraphMouse graphMouse = new XMLModalGraphMouse();
		graphMouse.setMode(ModalGraphMouse.Mode.PICKING);
		graphMouse.add(new PickingGraphMousePlugin());
		viewer.setGraphMouse(graphMouse);
		viewer.setPickSupport(new ShapePickSupport());
		viewer.addMouseListener(this);
		popupMenu = new JPopupMenu();
		JMenuItem item = new JMenuItem("pick");
		item.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				((XMLModalGraphMouse) viewer.getGraphMouse())
						.setMode(ModalGraphMouse.Mode.PICKING);
			}
		});
		popupMenu.add(item);
		item = new JMenuItem("transform");
		item.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				((XMLModalGraphMouse) viewer.getGraphMouse())
						.setMode(ModalGraphMouse.Mode.TRANSFORMING);
			}
		});
		popupMenu.add(item);

		final GraphZoomScrollPane panel = new GraphZoomScrollPane(viewer);

		// Icon loading is from
		// http://www.javaworld.com/javaworld/javaqa/2000-06/03-qa-0616-icon.html
		Image icon = Toolkit.getDefaultToolkit()
				.getImage(
						"resources" + System.getProperty("file.separator")
								+ "icon.jpg");
		if (icon != null)
			setIconImage(icon);

		// getContentPane().removeAll();
		getContentPane().add(panel);
		pack();
		restoreLayout(true);
		loadFrame(this, propName);
		setVisible(true);
	}

	protected static class XMLModalGraphMouse extends DefaultModalGraphMouse {
		/**
		 * Restores parameters which are not recorderd in the view/layout
		 * transforms, from the supplied XML stream
		 * 
		 * @param decode
		 *            XML decoder to use
		 */
		public void restore(XMLDecoder decoder) {
			((ScalingGraphMousePlugin) scalingPlugin).setIn((Float) decoder
					.readObject());
			((ScalingGraphMousePlugin) scalingPlugin).setOut((Float) decoder
					.readObject());
		}

		/**
		 * Stores parameters which are not recorderd in the view/layout
		 * transforms, to the supplied XML stream
		 * 
		 * @param encoder
		 *            XML encoder to use
		 */
		public void store(XMLEncoder encoder) {
			encoder.writeObject(((ScalingGraphMousePlugin) scalingPlugin)
					.getIn());
			encoder.writeObject(((ScalingGraphMousePlugin) scalingPlugin)
					.getOut());
		}
	}

	/**
	 * Loads or reloads a graph. Used during initialisation on the Swing thread,
	 * when graph changes or user hits F3 (reload).
	 * 
	 * @param ignoreErrors
	 *            Whether to ignore loading errors - they are ignored on
	 *            auto-load, but honoured on user load.
	 */
	protected void reloadLayout(boolean ignoreErrors) {
		assert graph != null;
		if (!wasInitialised) {
			construct(graph);
			setTitle((String) graph.getUserDatum(JUConstants.TITLE));
			wasInitialised = true;
		} else {
			viewer.getModel().setGraphLayout(
					new XMLPersistingLayout(propName != null ? new FRLayout(
							graph) : new KKLayout(graph)));
			setTitle((String) graph.getUserDatum(JUConstants.TITLE));
			restoreLayout(ignoreErrors);
			viewer.setRenderer(constructRenderer(graph));
		}
	}

	protected void restoreLayout(boolean ignoreErrors) {
		try {
			String fileName = getLayoutFileName(graph);
			if (propName != null && (new File(fileName)).canRead()) {
				XMLDecoder decoder = new XMLDecoder(new FileInputStream(
						fileName));
				((XMLPersistingLayout) viewer.getModel().getGraphLayout())
						.restore(decoder);

				// most rotate/share/translate are stateless, so I only need to
				// get the cumulative transform
				// for layout and view via getTransform() which should return
				// AffineTransform
				// which I should be able to persist into XML.
				// Only ScalingGraphMousePlugin has a state
				// in the form of getIn()/setIn()/getOut()/setOut().
				viewer.getViewTransformer().setToIdentity();
				viewer.getViewTransformer().concatenate(
						((XMLAffineTransformSerialised) decoder.readObject())
								.getAffineTransform());
				viewer.getLayoutTransformer().setToIdentity();
				viewer.getLayoutTransformer().concatenate(
						((XMLAffineTransformSerialised) decoder.readObject())
								.getAffineTransform());
				((XMLModalGraphMouse) viewer.getGraphMouse()).restore(decoder);
				decoder.close();

				viewer.repaint();
			}
		} catch (Exception e1) {
			if (!ignoreErrors)
				e1.printStackTrace();
		}
	}

	/**
	 * Used to serialise graph layout information into XML where XMLEncoder
	 * requires a top-level class.
	 * 
	 * @author kirr
	 * 
	 */
	public static class DoublePair {
		private double x, y;

		public DoublePair() {
			x = 0;
			y = 0;
		}

		public DoublePair(double a, double b) {
			setX(a);
			setY(b);
		}

		public double getX() {
			return x;
		}

		public double getY() {
			return y;
		}

		public void setX(double a) {
			x = a;
		}

		public void setY(double b) {
			y = b;
		}
	}

	/**
	 * A serialised form of XMLAffineTransform
	 * 
	 * @author kirr
	 * 
	 */
	public static class XMLAffineTransformSerialised {
		protected double[] matrix;

		public double[] getM() {
			return matrix;
		}

		public void setM(double[] m) {
			matrix = m;
		}

		AffineTransform getAffineTransform() {
			return new AffineTransform(matrix);
		}

		void setFromAffineTransform(AffineTransform tr) {
			matrix = new double[6];
			tr.getMatrix(matrix);
		}
	}

	protected void storeLayout() {
		try {
			if (propName != null) {
				String fileName = getLayoutFileName(graph);
				XMLEncoder encoder = new XMLEncoder(new FileOutputStream(
						fileName));
				((XMLPersistingLayout) viewer.getModel().getGraphLayout())
						.persist(encoder);

				XMLAffineTransformSerialised trV = new XMLAffineTransformSerialised();
				trV.setFromAffineTransform(viewer.getViewTransformer()
						.getTransform());
				encoder.writeObject(trV);
				XMLAffineTransformSerialised trL = new XMLAffineTransformSerialised();
				trL.setFromAffineTransform(viewer.getLayoutTransformer()
						.getTransform());
				encoder.writeObject(trL);
				((XMLModalGraphMouse) viewer.getGraphMouse()).store(encoder);
				encoder.close();
			}
		} catch (Exception e1) {
			e1.printStackTrace();
		}
	}

	protected static PluggableRenderer constructRenderer(Graph g) {
		PluggableRenderer r = new PluggableRenderer();
		r = labelEdges(r);
		r = labelVertices(r, g);
		return r;
	}

	/**
	 * If the frame was not constructed, we have to construct instances of all
	 * classes responsible for it; once it is build and only our graph changed,
	 * it is enough to replace the layout to update the graph.
	 */
	protected boolean wasInitialised = false;

	/** The graph currently being displayed, null if none is being displayed. */
	protected Graph graph = null;

	public void run() {
		reloadLayout(true);
	}

	public void update(final Observable s, Object arg) {
		graph = (Graph) ((Graph) arg).copy();
		SwingUtilities.invokeLater(this);
	}

	private static PluggableRenderer labelEdges(PluggableRenderer render) {
		EdgeStringer stringer = new EdgeStringer() {
			public String getLabel(ArchetypeEdge e) {
				if (e.containsUserDatumKey(JUConstants.LABEL)) {
					HashSet<String> labels = (HashSet<String>) e
							.getUserDatum(JUConstants.LABEL);
					Iterator<String> labelIt = labels.iterator();
					String label = "[ ";
					while (labelIt.hasNext()) {
						label = label.concat(labelIt.next() + " ");
					}
					return label + " ]";
				} else
					return "";
			}
		};
		render.setEdgeStringer(stringer);
		return render;
	}

	static class VertexShape extends AbstractVertexShapeFunction {
		public VertexShape() {
			super(new ConstantVertexSizeFunction(25),
					new ConstantVertexAspectRatioFunction(1.0f));
		}

		public Shape getShape(Vertex v) {
			if (RPNIBlueFringeLearner.isInitial(v))
				return factory.getRegularStar(v, 7);
			else if (!RPNIBlueFringeLearner.isAccept(v))
				return factory.getRectangle(v);
			return factory.getEllipse(v);
		}
	}

	static class VertexPaint implements VertexPaintFunction {
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
			else {
				JUConstants c = (JUConstants) v
						.getUserDatum(JUConstants.COLOUR);
				if (c == null)
					col = Color.GREEN;
				else {
					if (c == JUConstants.RED)
						col = Color.PINK;
					else if (c == JUConstants.BLUE)
						col = Color.BLUE;
				}
			}
			return col;
		}

	}

	private static PluggableRenderer labelVertices(PluggableRenderer r,
			Graph graph) {
		StringLabeller labeller = StringLabeller.getLabeller(graph, "name");
		labeller.clear();
		Iterator labelIt = graph.getVertices().iterator();
		while (labelIt.hasNext()) {
			Vertex v = (Vertex) labelIt.next();
			try {
				Object label = v.getUserDatum(JUConstants.LABEL);
				if (label != null)
					labeller.setLabel(v, label.toString());
			} catch (Exception e) {
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

	/**
	 * Holds the JFrame to see the graphs being dealt with. Usage:
	 * 
	 * <pre>
	 * updateFrame(upper, lower);// a public method
	 * </pre>
	 * 
	 * where <i>upper</i> and <i>lower</i> are the graphs to be displayed.
	 */
	static Visualiser upperGraphViz = null, lowerGraphViz = null;

	/**
	 * If a dialog box has to be displayed, it needs to know a reference frame.
	 * This method returns this frame, creating it if necessary.
	 * 
	 * @return a Visualiser frame.
	 */
	public static Visualiser getVisualiser() {
		if (upperGraphViz == null)
			upperGraphViz = new Visualiser(VIZ_PROPERTIES.UPPER);
		return upperGraphViz;
	}

	/**
	 * Removes the two windows displaying Jung graphs.
	 */
	public static void disposeFrame() {
		try {
			SwingUtilities.invokeAndWait(new Runnable() {
				public void run() {
					if (upperGraphViz != null) {
						upperGraphViz.setVisible(false);
						upperGraphViz.dispose();
						upperGraphViz = null;
					}
					if (lowerGraphViz != null) {
						lowerGraphViz.setVisible(false);
						lowerGraphViz.dispose();
						lowerGraphViz = null;
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

	/**
	 * Displays twos graphs passed as arguments in the Jung window. This method
	 * is to be called for one (g != null) or two (g != null && lowerGraph !=
	 * null) windows to be displayed; when both arguments are null, graphs are
	 * disposed of.
	 * 
	 * @param upperGraph
	 *            the graph to display
	 * @param lowerGraph
	 *            the graph to display below it
	 */
	public static void updateFrame(final DirectedSparseGraph upperGraph,
			final DirectedSparseGraph lowerGraph) {
		if (upperGraph == null && lowerGraph == null) {// destruction of
														// windows
			disposeFrame();
		} else {// construction of windows
			if (upperGraph == null)
				throw new IllegalArgumentException(
						"the first graph to display cannot be null");

			if (upperGraphViz == null)
				upperGraphViz = new Visualiser(VIZ_PROPERTIES.UPPER);

			upperGraphViz.update(null, upperGraph);
			if (lowerGraph != null) {
				if (lowerGraphViz == null)
					lowerGraphViz = new Visualiser(VIZ_PROPERTIES.LOWER);
				lowerGraphViz.update(null, lowerGraph);
			}
		}
	}

	/** Used to make it possible to single-step through graph transforms. */
	private final static Object syncObject = new Object();

	/** Value to return to a thread which is waiting. */
	private final static AtomicBoolean syncValue = new AtomicBoolean();

	/**
	 * Waits for a user to hit space on any of the visualiser windows.
	 * 
	 * @return true if user hit space, false if application is terminating.
	 */
	public static void waitForKey() {
		synchronized (syncObject) {
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
		VIZ_DIR, // the path to visualisation-related information, such as
					// graph layouts and configuration file.
		VIZ_CONFIG, // the configuration file containing window positions and
					// whether to display an assert-related warning.
	}

	public enum VIZ_PROPERTIES { // internal properties
		ASSERT, // whether to display assert warning.
		BUILDGRAPH, // whether to break if the name of a graph to build is equal
					// to a value of this property
		LOWER, UPPER // window positions, not real properties to be stored in
						// a file.
		, STOP // used to stop execution - a walkaround re JUnit Eclipse bug on
				// linux amd64.
		;
	}

	protected static Properties properties = null;

	protected static Map<String, Rectangle> windowCoords = null;// if I index by
																// VIZ_PROPERTIES,
																// I cannot
																// serialise
																// into XML

	/**
	 * Loads the location/size of a frame from the properties file and positions
	 * the frame as appropriate.
	 * 
	 * @param frame
	 *            the frame to position.
	 * @param name
	 *            the name of the property to load from
	 */
	protected static void loadFrame(Frame frame, VIZ_PROPERTIES name) {
		if (windowCoords == null)
			loadConfiguration();

		Rectangle result = windowCoords.get(name.toString());

		if (result == null) {// invent default coordinates, using
								// http://java.sun.com/j2se/1.5.0/docs/api/java/awt/GraphicsDevice.html#getDefaultConfiguration()

			GraphicsEnvironment ge = GraphicsEnvironment
					.getLocalGraphicsEnvironment();
			GraphicsDevice[] gs = ge.getScreenDevices();
			GraphicsConfiguration gc = gs[0].getDefaultConfiguration();

			// from http://java.sun.com/j2se/1.4.2/docs/api/index.html
			Rectangle shape = gc.getBounds();
			result = new Rectangle(shape.x, shape.y, 400, 300);
			if (name == VIZ_PROPERTIES.LOWER)
				result.y += result.getHeight() + 30;

			// no need to store these values, if a user hits the "save" button,
			// current ones will be saved.
		}

		frame.setBounds(result);
	}

	/**
	 * Stores the current location/size of a frame to the properties file.
	 * 
	 * @param frame
	 *            the frame to position.
	 * @param name
	 *            the name under which to store the property
	 */
	protected static void saveFrame(Frame frame, VIZ_PROPERTIES name) {
		Rectangle windowPos = new Rectangle(frame.getSize());
		windowPos.setLocation(frame.getX(), frame.getY());
		windowCoords.put(name.toString(), windowPos);
	}

	/**
	 * Retrieves the name of the file to load a graph layout from/store layout
	 * to.
	 * 
	 * @param g
	 *            the graph which name to extract
	 * @return the file name to load/store layout information, or throw
	 *         IllegalArgumentException if it cannot be retrieved.
	 */
	protected static String getLayoutFileName(Graph g) {
		String file = (String) g.getUserDatum(JUConstants.TITLE);
		String path = System.getenv(VIZ_ENV_PROPERTIES.VIZ_DIR.toString());
		if (path == null)
			path = "resources" + System.getProperty("file.separator")
					+ "graphLayout";
		if (file == null)
			throw new IllegalArgumentException("cannot obtain graph name, the "
					+ JUConstants.TITLE.toString()
					+ " property has not been set on the graph");

		return path + System.getProperty("file.separator")
				+ file.replace(' ', '_') + ".xml";
	}

	/**
	 * Retrieves the name of the file to load configuration information
	 * from/store it to.
	 * 
	 * @return the file name to load/store configuration information
	 *         information, or return null if it cannot be retrieved.
	 */
	protected static String getConfigurationFileName() {
		String path = System.getenv(VIZ_ENV_PROPERTIES.VIZ_DIR.toString());
		if (path == null)
			path = "resources" + System.getProperty("file.separator")
					+ "graphLayout";
		String file = System.getenv(VIZ_ENV_PROPERTIES.VIZ_CONFIG.toString());
		String result = null;
		if (file != null)
			result = path + System.getProperty("file.separator") + file
					+ ".xml";

		return result;
	}

	/**
	 * Retrieves the name of the property from the property file. The first call
	 * to this method opens the property file.
	 * 
	 * @param name
	 *            the name of the property.
	 * @param defaultValue
	 *            the default value of the property
	 * @return property value, default value if not found
	 */
	public static String getProperty(VIZ_PROPERTIES name, String defaultValue) {
		String result = defaultValue;
		if (properties == null)
			loadConfiguration();
		result = properties.getProperty(name.toString(), defaultValue);
		return result;
	}

	protected static void loadConfiguration() {
		String configFileName = getConfigurationFileName();
		if (configFileName == null || !new File(configFileName).canRead()) {
			System.err.println("Configuration file " + configFileName
					+ " does not exist.");
		} else
			try {
				if (configFileName != null) {
					XMLDecoder decoder = new XMLDecoder(new FileInputStream(
							configFileName));
					properties = (Properties) decoder.readObject();
					windowCoords = (HashMap<String, Rectangle>) decoder
							.readObject();
					decoder.close();
				}
			} catch (Exception e) {// failed loading, (almost) ignore this.
				System.err.println("Failed to load " + configFileName);
				e.printStackTrace();
			}

		if (windowCoords == null)
			windowCoords = new HashMap<String, Rectangle>();
		if (properties == null)
			properties = new Properties();
	}

	/** Saves all the current properties in the configuration file. */
	protected static void saveConfiguration() {
		String configFileName = getConfigurationFileName();
		if (windowCoords == null)
			windowCoords = new HashMap<String, Rectangle>();
		if (properties == null)
			properties = new Properties();

		try {
			if (configFileName != null) {
				XMLEncoder encoder = new XMLEncoder(new FileOutputStream(
						configFileName));
				encoder.writeObject(properties);
				encoder.writeObject(windowCoords);
				encoder.close();
			}
		} catch (Exception e) {// failed loading
			e.printStackTrace();
		}
	}

	/**
	 * Returns true if the configuration file defines the name of the supplied
	 * graph as the one transformation of which we should be looking at in
	 * detail.
	 * 
	 * @param graph
	 *            the graph we might wish to look at in detail
	 * @return whether lots of debug output should be enable when this graph is
	 *         being processed.
	 */
	public static boolean isGraphTransformationDebug(DirectedSparseGraph graph) {
		String name = graph == null ? null : (String) graph
				.getUserDatum(JUConstants.TITLE);
		return name != null
				&& name.length() > 0
				&& Visualiser.getProperty(Visualiser.VIZ_PROPERTIES.STOP, "")
						.equals(name);
	}

	protected static class XMLPersistingLayout extends PersistentLayoutImpl {

		public XMLPersistingLayout(Layout layout) {
			super(layout);
		}

		/**
		 * Almost verbatim from Jung source code.
		 * 
		 * Saves all the vertex locations to a file - this is enough to rebuild
		 * the layout later since we'll know the connections later anyway.
		 * 
		 * @param encoder
		 *            XML encoder to use
		 * @throws an
		 *             IOException if the file cannot be used
		 */
		public void persist(XMLEncoder encoder) {
			Set set = getGraph().getVertices();
			for (Iterator iterator = set.iterator(); iterator.hasNext();) {
				Vertex v = (Vertex) iterator.next();
				DoublePair p = new DoublePair(getX(v), getY(v));
				map.put(new Integer(v.hashCode()), p);
			}
			encoder.writeObject(map);
		}

		/**
		 * Almost verbatim from Jung source code. Restores all vertex locations
		 * from a file; does nothing if loading fails.
		 * 
		 * @param decode
		 *            XML decoder to use
		 */
		public void restore(XMLDecoder decoder) throws IOException,
				ClassNotFoundException {
			Map<Integer, DoublePair> sourceMap = (Map<Integer, DoublePair>) decoder
					.readObject();
			for (Iterator<Map.Entry<Integer, DoublePair>> mi = sourceMap
					.entrySet().iterator(); mi.hasNext();) {
				Map.Entry<Integer, DoublePair> e = mi.next();
				DoublePair p = e.getValue();
				map.put(e.getKey(), new PersistentLayout.Point(p.getX(), p
						.getY()));
			}
			initializeLocations();
			locked = true;
		}
	}

	public void mouseClicked(MouseEvent e) {
	}

	public void mouseEntered(MouseEvent e) {
	}

	public void mouseExited(MouseEvent e) {
	}

	// The following is from
	// http://java.sun.com/docs/books/tutorial/uiswing/components/menu.html#popup

	public void mousePressed(MouseEvent e) {
		maybeShowPopup(e);
	}

	public void mouseReleased(MouseEvent e) {
		maybeShowPopup(e);
	}

	private void maybeShowPopup(MouseEvent e) {
		if (e.isPopupTrigger()) {
			popupMenu.show(e.getComponent(), e.getX(), e.getY());
		}
	}
}
