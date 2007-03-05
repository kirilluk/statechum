package statechum.analysis.learning;

import java.util.*;
import edu.uci.ics.jung.visualization.*;
import edu.uci.ics.jung.visualization.contrib.*;
import edu.uci.ics.jung.visualization.control.DefaultModalGraphMouse;
import edu.uci.ics.jung.visualization.control.ModalGraphMouse;
import edu.uci.ics.jung.graph.*;
import edu.uci.ics.jung.graph.decorators.*;

import java.awt.*;
import javax.swing.*;

public class Visualiser extends JFrame implements Observer  {
	
	
	
	public Visualiser(HashSet sPlus, HashSet sMinus, boolean blueFringe){
		this.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
        setSize(new Dimension(800,600));
        setVisible(true);
        if(blueFringe){
        	RPNIBlueFringeLearner l = new RPNIBlueFringeLearner();
        	l.addObserver(this);
        	l.learnMachine(l.initialise(), sPlus, sMinus);
        }
        else{
        	RPNILearner l = new RPNILearner(sPlus, sMinus);
        	l.addObserver(this);
        	l.learnMachine();
        }
		
	}
	
	public void update(Subject s){
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
		//this.getContentPane().update(this.getGraphics());
		//repaint();
		//JOptionPane.showMessageDialog(this, "click OK to continue");
	}

	
	private  PluggableRenderer labelEdges(PluggableRenderer render){
		EdgeStringer stringer = new EdgeStringer(){
            public String getLabel(ArchetypeEdge e) {
            	if(e.containsUserDatumKey("label")){
            		HashSet<String> labels = (HashSet<String>)e.getUserDatum("label");
            		Iterator labelIt = labels.iterator();
            		String label = new String();
            		while(labelIt.hasNext()){
            			label = label.concat(labelIt.next()+" ");
            		}
            		return label;
            	}
            	else return "";
            }
        };
        render.setEdgeStringer(stringer);
        return render;
	}
	
	private  PluggableRenderer labelVertices(PluggableRenderer r, Graph graph){
		StringLabeller labeller = StringLabeller.getLabeller(graph,"name");
		Iterator labelIt = graph.getVertices().iterator();
		while(labelIt.hasNext()){
			Vertex v = (Vertex)labelIt.next();
			try{
				String label = v.getUserDatum("label").toString();
				if(v.getUserDatum("colour")!=null)
					label = label.concat(" "+ v.getUserDatum("colour").toString().substring(0,1).toUpperCase());
				labeller.setLabel(v,label);
			}
			catch(Exception e){
				System.out.println(e);
			}
		}
		r.setVertexStringer(labeller);
		return r;
	}

}
