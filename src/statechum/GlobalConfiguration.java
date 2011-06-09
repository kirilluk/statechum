/* Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 * 
 * This file is part of StateChum.
 * 
 * StateChum is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * StateChum is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with StateChum.  If not, see <http://www.gnu.org/licenses/>.
 */

package statechum;

import java.awt.Frame;
import java.awt.GraphicsConfiguration;
import java.awt.GraphicsDevice;
import java.awt.GraphicsEnvironment;
import java.awt.Rectangle;
import java.beans.XMLDecoder;
import java.beans.XMLEncoder;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.TreeMap;

import statechum.GlobalConfiguration.G_PROPERTIES;

import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;

/**
 * Unlike <em>Configuration</em> which stores a set of variables for a specific learning-process, an instance of this
 * class reflects configuration parameters for the whole of Statechum.
 * 
 * @author kirill
 *
 */
public class GlobalConfiguration {
	public enum G_PROPERTIES { // internal properties
		ASSERT// whether to display assert warnings. If this is set, a number of additional consistency checks are enabled.
		,TEMP // temporary directory to use
		,LINEARWARNINGS// whether to warn when external solver cannot be loaded and we have to fall back to the colt solver.
		,SMTWARNINGS// whether we should provide warnings when some SMT-related operations do not make sense or cannot be completed.
		,BUILDGRAPH // whether to break if the name of a graph to build is equal to a value of this property
		,STOP // used to stop execution - a workaround re JUnit Eclipse bug on linux amd64.
		,GRAPHICS_MONITOR // the monitor to pop graphs on - useful when using multiple separate screens rather than xinerama or nview
		,TIMEBETWEENHEARTBEATS // How often to check i/o streams and send heartbeat data.
		,ASSERT_ENABLED // whether assertions are enabled - in this case some additional checks are carried out outside of assert statements
		,VIZ_DIR// the path to visualisation-related information, such as graph layouts and configuration file.
		,VIZ_CONFIG// the configuration file containing window positions and whether to display an assert-related warning.
		,VIZ_AUTOFILENAME // used to define a name of a file to load answers to questions.
		,LTL2BA // path to LTL2BA executable, if null the default path will be used.
		,ERLANGHOME // path to the Erlang distribution directory 
		,ERLANGOUTPUT_ENABLED // whether to relay any output from Erlang to the console - there could be quite a lot of it.
		;
	}

	/**
	 * Default screen to use for any frames created. 
	 */
	public static final int DEFAULT_SCREEN = 0;
	
	/**
	 * Default values of Statechum-wide attributes. 
	 */
	protected static final Map<G_PROPERTIES, String> defaultValues = new TreeMap<G_PROPERTIES, String>();
	
	private static boolean assertionsEnabled = false;// this has to be executed above the static block which assigns a new value if appropriate.
	
	static
	{
		defaultValues.put(G_PROPERTIES.LINEARWARNINGS, "false");
		defaultValues.put(G_PROPERTIES.SMTWARNINGS, "false");
		defaultValues.put(G_PROPERTIES.ASSERT, "false");
		defaultValues.put(G_PROPERTIES.GRAPHICS_MONITOR, ""+DEFAULT_SCREEN);
		defaultValues.put(G_PROPERTIES.STOP, "");
		defaultValues.put(G_PROPERTIES.TEMP, "tmp");
		defaultValues.put(G_PROPERTIES.TIMEBETWEENHEARTBEATS, "3000");
		defaultValues.put(G_PROPERTIES.ASSERT_ENABLED, "false");
		defaultValues.put(G_PROPERTIES.ERLANGOUTPUT_ENABLED, "true");
		assert assertionsEnabled = true;// from http://java.sun.com/j2se/1.5.0/docs/guide/language/assert.html
	}
	
	
	public boolean isAssertEnabled()
	{
		return assertionsEnabled || Boolean.valueOf(GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.ASSERT_ENABLED));
	}
	
	protected GlobalConfiguration() {
		// the initial values are provided for each variable inline.
	}
	
	private static final GlobalConfiguration globalConfiguration = new GlobalConfiguration();
	
	public static GlobalConfiguration getConfiguration()
	{
		return globalConfiguration;
	}
	
	protected Properties properties = null;
	protected Map<Integer, WindowPosition> windowCoords = null;

	/** Retrieves the name of the property from the property file.
	 *  The first call to this method opens the property file.
	 *  
	 * @param name the name of the property.
	 * @param defaultValue the default value of the property
	 * @return property value, default value if not found
	 */
	public String getProperty(G_PROPERTIES name)
	{
		if (properties == null)
			loadConfiguration();
		return properties.getProperty(name.name(), defaultValues.get(name));
	}

	protected void loadConfiguration()
	{
		String configFileName = getConfigurationFileName();
		if (configFileName != null && new File(configFileName).canRead())
		try 
		{
			System.out.println("Loaded configuration file "+configFileName);
			XMLDecoder decoder = new XMLDecoder(new FileInputStream(configFileName));
			properties = (Properties) decoder.readObject();
			windowCoords = (HashMap<Integer, WindowPosition>) decoder.readObject();
			decoder.close();
		} catch (Exception e) 
		{// failed loading, (almost) ignore this.
			System.err.println("Failed to load "+configFileName);
			e.printStackTrace();
		}
		
		if (windowCoords == null)
			windowCoords = new HashMap<Integer, WindowPosition>();
		if (properties == null)
			properties = new Properties();
		boolean valuesSet = false,firstValue=true;
		for(G_PROPERTIES prop:G_PROPERTIES.values())
		{
			String name = prop.name(),value=null;try { value=System.getProperty(name); } catch (Exception ex) {}// ignore exception if cannot extract a value
			if (value != null)
			{// new value set via command-line
				if (firstValue) firstValue=false;else System.out.print(',');
				System.out.print(name+"="+value);
				properties.setProperty(name,value);valuesSet=true;
			}
		}
		if (valuesSet) System.out.println();
	}

	/** Retrieves the name of the file to load configuration information from/store it to.
	 * 
	 * @return the file name to load/store configuration information information, or return null if it cannot be retrieved. 
	 */
	protected static String getConfigurationFileName()
	{
		String path = System.getProperty(G_PROPERTIES.VIZ_DIR.name());if (path == null) path="resources"+File.separator+"graphLayout";
		String file = System.getProperty(G_PROPERTIES.VIZ_CONFIG.name());
		String result = null;
		if (file != null)
			result = path+File.separator+file+".xml";
		
		return result;
	}

	/** Loads the location/size of a frame from the properties file and positions the frame as appropriate.
	 * 
	 * @param frame the frame to position.
	 * @param name the name of the property to load from
	 */   
	public WindowPosition loadFrame(int windowID)
	{
		if (windowCoords == null)
			loadConfiguration();
		
		WindowPosition result = windowCoords.get(windowID);
		
		if (result == null)
		{// invent default coordinates, using http://java.sun.com/j2se/1.5.0/docs/api/java/awt/GraphicsDevice.html#getDefaultConfiguration()

			GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
			GraphicsDevice[] gs = ge.getScreenDevices();
			int deviceToUse = Integer.valueOf(getProperty(G_PROPERTIES.GRAPHICS_MONITOR));
			if (deviceToUse >= gs.length) deviceToUse =statechum.GlobalConfiguration.DEFAULT_SCREEN;// use the first one if cannot use the requested one.
			GraphicsConfiguration gc = gs[deviceToUse].getDefaultConfiguration();
			
			// from http://java.sun.com/j2se/1.4.2/docs/api/index.html
			Rectangle shape = gc.getBounds();
			Rectangle rect = new Rectangle(new Rectangle(shape.x, shape.y,400,300));
			if (rect.height > shape.height) rect.height=shape.height;if (rect.width > shape.width) rect.width=shape.width;
			rect.y+=windowID*(rect.getHeight()+30);
			int yLimit = shape.height-rect.height;if (rect.y>yLimit) rect.y=yLimit;
			int xLimit = shape.width -rect.width ;if (rect.x>xLimit) rect.x=xLimit;
			result = new WindowPosition(rect,deviceToUse);
			windowCoords.put(windowID,result);
		}
		
		return result;
	}
	
	/** Stores the current location/size of a frame to the properties file.
	 * 
	 * @param frame the frame to position.
	 * @param name the name under which to store the property
	 */   
	public void saveFrame(Frame frame,int windowID)
	{
		WindowPosition windowPos = windowCoords.get(windowID);if (windowPos == null) windowPos = new WindowPosition();
		Rectangle newRect = new Rectangle(frame.getSize());
		newRect.setLocation(frame.getX(), frame.getY());
		windowPos.setRect(newRect);
		windowCoords.put(windowID, windowPos);
	}

	/** Stores the details of the frame position. 
	 */
	public static class WindowPosition
	{
		private Rectangle rect = null;
		private int screenDevice;
		
		public WindowPosition() {
			// need a default constructor.
		}

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
	
	/** Saves all the current properties in the configuration file. */
	public void saveConfiguration()
	{
		String configFileName = getConfigurationFileName();
		if (windowCoords == null)
			windowCoords = new HashMap<Integer, WindowPosition>();
		if (properties == null)
			properties = new Properties();

		try {
			if (configFileName != null)
			{
				XMLEncoder encoder = new XMLEncoder(new FileOutputStream(configFileName));
				encoder.writeObject(properties);
				encoder.writeObject(windowCoords);encoder.close();
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
	public boolean isGraphTransformationDebug(DirectedSparseGraph graph)
	{
		String name = graph == null? null:(String)graph.getUserDatum(JUConstants.TITLE);
		return name != null && name.length()>0 && 
			getProperty(G_PROPERTIES.STOP).equals(name);
	}

	public void setProperty(G_PROPERTIES key, String string) {
		if(properties == null) {
			properties = new Properties();
		}
		properties.setProperty(key.name(), string);
	}
}
