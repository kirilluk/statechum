/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package analysis.Erlang;

import java.awt.Dimension;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.IOException;
import javax.swing.JEditorPane;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JScrollPane;

/**
 *
 * @author ramsay
 */
public class ErlangCoverageFileFrame extends JFrame {

    public ErlangCoverageFileFrame(final String f, String name) {
        super(name);
        try {
            JEditorPane htmlPane = new JEditorPane(f);
            htmlPane.setEditable(false);
            JScrollPane scroll = new JScrollPane(htmlPane, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS, JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
            scroll.setPreferredSize(new Dimension(1024, 768));
            this.setContentPane(scroll);
        } catch (IOException e) {
            JLabel c = new JLabel("There was an error opening " + f);
            this.setContentPane(c);
        }
        addWindowListener(new WindowAdapter() {

            @Override
            public void windowClosing(WindowEvent e) {
                (new File(f)).delete();
            }
        });
        this.pack();
        this.setVisible(true);
    }
}
