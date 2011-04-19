/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package statechum;

/**
 *
 * @author ramsay
 */
public interface Label extends Comparable<Label> {

    abstract Label replaceAll(String from, String to);

}
