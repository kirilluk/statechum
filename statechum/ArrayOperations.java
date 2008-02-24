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

package statechum;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

/** A number of static methods to manipulate nested sequences of objects. 
 * Implemented as sequences of arrays because Java provides a simple syntax 
 * for array initialisation which is missing for "real" sequences. 
 * 
 * @author kirr
 *
 */
public class ArrayOperations {
    /** Appends two arrays and returns a result. None of the two
     * arrays passed as paramenters are affected.
     * @param to the array to append to
     * @param what the array to append
     * @return the result of appending what to to
     */
    public static byte[] arrayappend(byte[] to, byte[] what)
    {
		if (to == null)
			return what;
		if (what == null)
			return to;

		byte[] data = new byte[to.length + what.length];
		System.arraycopy(to, 0, data, 0, to.length);
		System.arraycopy(what, 0, data, to.length, what.length);
		return data;
    }

    /** appends a number provided to an array
     * @param to the array to append to
     * @param what the number to append
     * @return the array with the number appended.
     */
    public static byte [] arrayappend(byte [] to, byte what)
    {
      byte whatToAppend[]=new byte[1];whatToAppend[0]=what;
      return arrayappend(to,whatToAppend);
    }

    /** An instance of this object can be used at the end of test sequences. */
    public static class EndOfSequence
    {
     /** Compares two end-of-test objects for equality.
       * Returns true only if the two are the same instance.
       *
       * @see java.lang.Object#equals(java.lang.Object)
       */
      public boolean equals(Object obj)
      {
        return obj == this;
      }

      private static EndOfSequence instance = new EndOfSequence();
      
      /** An (empty) default constructor. */
      protected EndOfSequence() 
      {// does not do anything.
      }

	/** Provides a `printable' name of this test.
	 *
	 * @return test name.
	 */
      public String getName() { return "End"; }

      /** The test class <code>testIO</code> holds an instance of EndOfSequence which is very
       * useful to end tests with; there is no need to create a new instance of EndOfSequence
       * whenever such an instance is needed.
       *
       * @return an instance of the EndOfSequence class.
       */
      public static EndOfSequence End() {
        return instance;
      }

    }
    
    /** This function cuts the array of objects beyond the instance of the EndOfSequence class.
     * An instance of <code>testIO.classEnd</code> class is a marker that
     * sequences should not be appended beyond this marker.
     * @param what the array on which to operate.
     * @return the array with everything after (and including) the first EndOfSequence removed.
     */
    public static Object [] arrayTrunc(Object [] what)
    {
      if (what == null)
        return what;

      int i=0;
      for(;i<what.length && !(what[i] instanceof EndOfSequence);++i)
      {// intentionally empty.
      }

      if (i == what.length)
        return what;

      Object result [] = new Object [i+1];// ensure that End will be appended.
      System.arraycopy(what,0,result,0,i+1);
      return result;
    }

    /** Multiplies elements of the given array, i.e.
     * considers an array to consist of arrays and multiplies these arrays.
     * Null-elements are ignored.
     * 
     * @param what the array to process.
     * @return An array of test sequences; each of test sequences contain test elements.
     */
    public static Object [][] flatten(Object [] what)
    {
      if (what == null) return null;

      Object tmp [] = null;
      if (what.length == 0) return new Object[][]{};

      tmp=flatten(what,0);
      List<Object> outcome = new LinkedList<Object>();

      // now remove all nulls (both null sequences and null elements from sequence) and remove repeated sequences.
      for(int i=0;i<tmp.length;++i)
      {
        if (tmp[i] != null)
        {
          List<Object> res = new LinkedList<Object>();
          Object []testSeq=(Object[])tmp[i];
          for(int j=0; j<testSeq.length;++j)
            if (testSeq[j] != null) res.add(testSeq[j]);

          if (!res.isEmpty())
          {// now we remove duplicates
            if (!outcome.contains(res))
              outcome.add(res);
          }
        }
      }

      Object [][] result = new Object[outcome.size()][];
      int i=0;
      for(Iterator<Object> iter=outcome.iterator();iter.hasNext();++i)
        result[i]=((List<Object>)iter.next()).toArray();

      return result;
    }

	/** Used to separate elements of flags associated with each sequence corresponding to nodes. Used in <em>getDebugDataSet</em>.
	 */
	public static final char separator=','; 

	/** Converts a supplied sequence of strings into a textual representation. Similar to <em>toString</em> but does not include spaces or brackets.
	 * 
	 * @param seq sequence to convert
	 * @return conversion outcome.
	 */
	public static String seqToString(Collection<String> seq)
	{//TODO: to test seqToString
		boolean first = true;
		StringBuffer result = new StringBuffer();for(String str:seq) { if (first) first=false;else result.append(separator);result.append(str); }
		return result.toString();
	}

    
    /** Multiplies elements of the given array, i.e.
     * every it considers an array to consist of arrays and multiplies these arrays.
     * null-elements are ignored.
     * @param what the array to process.
     * @param position the element to start with.
     * @return the outcome.
     */
    private static Object [] flatten(Object [] what, int position)
    {
      Object current[]=null;
      if (what[position] == null)
        current = new Object[] { null };
      else
        if (what[position] instanceof Object[]) // an array of elements
        {
          Object [] theArray = (Object[])what[position];
          current = new Object [0];
          for(int i=0;i < theArray.length;++i)
          {
            if (theArray[i] instanceof Object[])
              current=arrayappend(current,flatten((Object [])theArray[i]));// flatten this sequence
            else
              current = arrayappend(current,(Object)(new Object[] { theArray[i] }));// a single element
          }
        }
        else
          current = new Object []{ new Object[] { what[position]} };
      // at this point, we've converted the current element into an array of sequences.

      Object [] result = null;

      if (position == what.length-1)
        return current;

      Object [] prev = flatten(what,position+1);
      result = new Object[current.length*prev.length];

      for(int i=0;i<current.length;++i)
        for(int j=0;j<prev.length;++j)
          result[i*prev.length+j] = arrayTrunc(arrayappend( (Object [])current[i],(Object [])prev[j]));

      return result;
    }

    /** Appends two arrays and returns a result. None of the two
     * arrays passed as paramenters are affected.
     * @param to the array to append to
     * @param what the array to append
     * @return the result of appending what to to
     */
    public static Object [] arrayappend(Object [] to, Object [] what)
    {
        if (to == null) return what;
        if (what == null) return to;

        Object [] data=new Object[to.length+what.length];
        System.arraycopy(to,0,data,0,to.length);
        System.arraycopy(what,0,data,to.length,what.length);
        return data;
    }

    /** appends a number provided to an array
     * @param to the array to append to
     * @param what the number to append
     * @return the array with the number appended.
     */
    public static Object [] arrayappend(Object [] to, Object what)
    {
        Object whatToAppend[]=new Object[1];whatToAppend[0]=what;
        return arrayappend(to,whatToAppend);
    }

    /** Appends an array supplied to it, to itself, a number of times.
     * @param what an array of objects to raise to power
     * @param number the power to raise to. If zero, <code>power</code> returns null.
     * @return the outcome.
     */
    public static Object [] power(Object []what, int number)
    {
      Object [] outcome = null;
      for(int i=0;i<number;++i) outcome = arrayappend(outcome,what);
      return outcome;
    }

    /** Given an array  A, builds a sequence containing
     * null, A, A*A, A*A*A ... up to the power of A requested.
     * @param what array of objects to raise to power. If null, returns null.
     * @param number the power to raise to. If zero, returns null.
     * @return the outcome.
     */
    public static Object [] series(Object [] what, int number)
    {
      if (number == 0 || what == null) return null;

      Object [] outcome = new Object[]{null}, currentPower = new Object[0];
      for(int i=0;i<number;++i)
      {
        currentPower = arrayappend(currentPower,what);
        outcome = arrayappend(outcome,(Object)currentPower);
      }
      return outcome;
    }    

    /** Compares two arrays and returns true if they are equal.
     * The message describing what is different between them
     * is stored in the <code>errorMessage</code> variable.
     *
     * @param what the first array to compare
     * @param with the second one
     * @param comparisonResult the description of what went wrong in the comparison,
     * if this function returns <code>false</code>.
     * @return true if the two contain equal elements (according to their <code>equals</code> function).
     */
    protected static boolean cmp(Object [] what, Object [] with, StringBuffer comparisonResult)
    {
      if ( (what == null && with != null) || (what != null && with == null))
      {
        if (comparisonResult != null)
          comparisonResult.append("wrong result of appending : null v.s. non-null");
        return false;
      }

      if (what == null && with == null)
        return true;

      if (what.length != with.length)
      {
        if (comparisonResult != null)
          comparisonResult.append("Wrong number of elements in the arrays to compare");
        return false;
      }

      for(int i=0;i<what.length;++i)
      {
        if  ((what[i] != null && with[i] == null) || (what[i] == null && with[i] != null))
        {
          if (comparisonResult != null)
            comparisonResult.append("wrong result of appending : null v.s. non-null at position " + i);
          return false;
        }

        if (what[i] == null)
          continue;

        // at this point, both what[i] and with[i] are non-null.

        if ( !what[i].getClass().equals(with[i].getClass()))
        {
          if (comparisonResult != null)
            comparisonResult.append("Elements at position "+i+" are of different types");
          return false;
        }

        if (what[i] instanceof Object[])// both elements are arrays, compare them.
        {
          if ( !cmp( ( Object[] ) what[i], ( Object[] ) with[i], comparisonResult ))
            return false;
        }
        else
        if (!what[i].equals(with[i]))// elements are ordinary object, compare them directly.
        {
          if (comparisonResult != null)
            comparisonResult.append("Elements at position "+i+" differ: expected "+what[i]+", got "+with[i]);
          return false;
        }

      }

      return true;
    }

}
