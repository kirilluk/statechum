/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package statechum.analysis.CodeCoverage;

import java.awt.Dimension;
import java.io.File;
import javax.swing.JEditorPane;
import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.ScrollPaneConstants;

/**
 *
 * @author ramsay
 */
public class CodeCoverageStringFrame extends JFrame {

    /**
     * ID for serialization
     */
    private static final long serialVersionUID = -236003125488674238L;

    public CodeCoverageStringFrame(final String f, String name) {
        super(name);
        JEditorPane htmlPane = new JEditorPane("text/html", f);
        htmlPane.setEditable(false);
        JScrollPane scroll = new JScrollPane(htmlPane, ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_ALWAYS);
        scroll.setPreferredSize(new Dimension(1024, 768));
        this.setContentPane(scroll);
        this.pack();
        this.setVisible(true);
        (new File(f)).delete();
    }
}
