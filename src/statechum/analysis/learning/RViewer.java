// Mostly from http://www.algorithm-forge.com/techblog/2010/07/using-javagd-in-java-applications/

package statechum.analysis.learning;

import java.awt.GraphicsEnvironment;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.util.Map;
import java.util.TreeMap;

import javax.swing.Action;
import javax.swing.JFrame;
import javax.swing.JPopupMenu;
import javax.swing.WindowConstants;

import org.rosuda.javaGD.GDInterface;
import org.rosuda.javaGD.JGDBufferedPanel;

public class RViewer extends GDInterface implements WindowListener {
	
	/** The frame hosting each window. */
    protected JFrame frame;

    /** A popup with Jung control choices. */
    JPopupMenu popupMenu;

    
	/** Maps graph names to windows. */
	protected static Map<String,RViewer> nameToGraph = new TreeMap<String,RViewer>();
	
	/** Given a name returns a corresponding graph. */
	public static RViewer getGraph(String name)
	{
		return nameToGraph.get(name);
	}

	/** Obtains a device number used to switch the output to this graph. Device numbers start from zero. */
	@Override
	public int getDeviceNumber()
	{
		return c.getDeviceNumber();
	}

    /** The name to give to the next graph to be created. */
    protected static String newGraphName =null;

	public static final int graphWindowNumber = 100;// used to serialise the coordinates.

	/** Every graph has a size associated with it, this one contains the one for the next graph to be created. */
	protected static int currentWindowNumber = graphWindowNumber;

	protected int thisWindowNumber = -1;

    /** Assigns a graph name for the creation of the next graph, it is expected that the next call will go to gdOpen via JNI. */
    public static void setNewGraphName(String name)
    {
    	assert newGraphName == null;
    	newGraphName = name;
    }

    protected String graphName = null;

    public String getGraphName()
    {
    	return graphName;
    }

    /** Key bindings. */
    protected Map<Integer, Action> keyToActionMap = new TreeMap<Integer, Action>();
   
    @Override
    public void gdOpen(double w, double h)
	{
    	thisWindowNumber=currentWindowNumber++;
    	graphName = newGraphName;
		frame = new JFrame(GraphicsEnvironment.getLocalGraphicsEnvironment().getScreenDevices()[Visualiser.globalConfig.loadFrame(thisWindowNumber).getScreenDevice()].getDefaultConfiguration());
		frame.setTitle("Graph "+getGraphName());
		frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		frame.addWindowListener(this);
		Visualiser.setStateChumKeyBindings(frame, thisWindowNumber, keyToActionMap);
        frame.addKeyListener(new KeyListener() {

            @Override
            public void keyPressed(KeyEvent arg0) {
                Action act = keyToActionMap.get(arg0.getKeyCode());
                if (act != null) {
                    act.actionPerformed(null);
                }
            }

            @Override
            public void keyReleased(@SuppressWarnings("unused") KeyEvent arg0) {// this method is intentionally left blank - keypresses/releases are handled by the keyPressed method.
            }

            @Override
            public void keyTyped(@SuppressWarnings("unused") KeyEvent key) {// this method is intentionally left blank - keypresses/releases are handled by the keyPressed method.
            }
        });
        
        popupMenu = new JPopupMenu();
 
        Visualiser.updatePopupMenu(popupMenu, keyToActionMap);
		c = new JGDBufferedPanel(w, h){ 
			/**
			 * ID for serialisation
			 */
			private static final long serialVersionUID = -3603199728828001813L;

				// The following is from http://java.sun.com/docs/books/tutorial/uiswing/components/menu.html#popup
			    @Override
			    public void mousePressed(MouseEvent e) {
			    	super.mousePressed(e);
			        maybeShowPopup(e);
			    }

			    @Override
			    public void mouseReleased(MouseEvent e) {
			    	super.mouseReleased(e);
			        maybeShowPopup(e);
			    }

			    private void maybeShowPopup(MouseEvent e) {
			        if (e.isPopupTrigger()) {
			            popupMenu.show(e.getComponent(),
			                    e.getX(), e.getY());
			        }
			    }
			};
	        
		frame.getContentPane().add((JGDBufferedPanel)c);
    	nameToGraph.put(getGraphName(), this);newGraphName = null;
    	
    	// We should not manipulate sizes of components here because changes to a size of a frame 
    	// cases an rniEval to be executed. Size changes are managed via a repaint on Swing thread 
    	// and this happens immediately after we try to change size here in the worker thread
    	// from within rniEval caused by a call to interactive box plot in DrawGraphs. 
    	// As such, we are waiting for resize to complete and resize is waiting for rniEval to finish
    	// leading to a deadlock. Solution - run all this on Swing thread so that updates do not happen
    	// until we are finished - this also avoids deadlock if an update happens when I'm plotting
    	// a graph from a subsequent rniEval
        frame.pack();
		frame.setBounds(Visualiser.globalConfig.loadFrame(thisWindowNumber).getRect());
        frame.setVisible(true);
        super.gdOpen(w, h);
	}
    
    /** Post-open initialisation. 
     * 
     */
    public void init(int dev)
    {
		c.setDeviceNumber(dev);
    }
    
    @Override
    public void gdClose()
    {
    	super.gdClose();
    	if (frame!=null)
    	{
    		frame.getContentPane().remove((JGDBufferedPanel)c);
    		frame.dispose();frame = null;
    	}
    }
    
	@Override
	public void windowOpened(@SuppressWarnings("unused") WindowEvent e) {
		// dummy
	}

	@Override
	public void executeDevOff()
	{
		DrawGraphs.eval("try({ dev.set("+(getDeviceNumber()+1)+"); dev.off()},silent=TRUE)","failed to do devoff");
	}
	
	@Override
	public void windowClosing(@SuppressWarnings("unused") WindowEvent e) {
		if (c!=null)
		{
			if (DrawGraphs.engine != null)
				executeDevOff();
		}
	}

	@Override
	public void windowClosed(@SuppressWarnings("unused") WindowEvent e) {
		// dummy
	}

	@Override
	public void windowIconified(@SuppressWarnings("unused") WindowEvent e) {
		// dummy
	}

	@Override
	public void windowDeiconified(@SuppressWarnings("unused") WindowEvent e) {
		// dummy
	}

	@Override
	public void windowActivated(@SuppressWarnings("unused") WindowEvent e) {
		// dummy
	}

	@Override
	public void windowDeactivated(@SuppressWarnings("unused") WindowEvent e) {
		// dummy
	}

}
