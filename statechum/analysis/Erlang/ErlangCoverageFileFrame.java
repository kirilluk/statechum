/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package statechum.analysis.Erlang;

import java.awt.Dimension;
import java.io.File;
import java.io.IOException;
import javax.swing.JEditorPane;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.ScrollPaneConstants;

/**
 *
 * @author ramsay
 */
public class ErlangCoverageFileFrame extends JFrame {

    /**
	 * ID for serialization
	 */
	private static final long serialVersionUID = -236003125488674238L;

	public ErlangCoverageFileFrame(final String f, String name) {
        super(name);
        try {
            JEditorPane htmlPane = new JEditorPane(f);
            htmlPane.setEditable(false);
            JScrollPane scroll = new JScrollPane(htmlPane, ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_ALWAYS);
            scroll.setPreferredSize(new Dimension(1024, 768));
            this.setContentPane(scroll);
        } catch (IOException e) {
            JLabel c = new JLabel("There was an error opening " + f);
            this.setContentPane(c);
        }
        this.pack();
        this.setVisible(true);
        (new File(f)).delete();
    }
}
