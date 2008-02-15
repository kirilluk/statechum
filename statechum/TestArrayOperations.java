package statechum;

import java.util.Arrays;
import java.util.Comparator;
import java.util.TreeSet;

import org.junit.Assert;
import org.junit.Test;


import junit.framework.AssertionFailedError;

import static statechum.ArrayOperations.arrayappend;

public class TestArrayOperations {
	/** Fails the currently running test with the supplied error message.
	 * 
	 * @param ErrorMessage the error message to return.
	 */
	public static void fail(String ErrorMessage)
	{
		throw new AssertionFailedError(ErrorMessage);
	}
	
	  /** A generally useful function, comparing floating-point values.
	   * @param errMsg the error to report with an <code>AssertionFailedError</code> if not equal.
	   * @param a what to compare
	   * @param b with what.
	   */
	  public static void checkFloatsEqual( String errMsg, double a, double b )
	  {
	    if ( Math.abs( a - b ) > 1e-5 )
	      fail( errMsg + ": " + a + " v.s. " + b );
	  }

	  // Testing the byte part

	  /** Compares two arrays of bytes.
	   * @param what what to compare
	   * @param with with what.
	   */
	  public static void compare(byte [] what, byte [] with)
	  {
	    if ( (what == null && with != null) || (what != null && with == null))
	      fail("wrong result of appending : null v.s. non-null");
	    if (what == null && with == null)
	      return;

	    if (what.length != with.length)
	      fail("Wrong number of elements in the arrays to compare");

	    for(int i=0;i<what.length;++i)
	      if (what[i] != with[i])
	        fail("Elements at position "+i+" differ: expected "+what[i]+", got "+with[i]);
	  }

	  /** Tests appending of arrays with a number of elements. */
	  @Test
	  public void testArrayAppendOK()
	  {
	    byte a[] = new byte[] { 1,2,3,4,5 }, b[] = new byte[] { 6,7,8,9 };
	    compare(new byte[]{1,2,3,4,5,6,7,8,9},arrayappend(a,b));
	  }

	  /** Test appending of an array with a single element and the one with many. */
	  @Test
	  public void testArrayAppend1OK()
	  {
	    byte a[] = new byte[] { 1 }, b[] = new byte[] { 6,7,8,9 };
	    compare(new byte[]{1,6,7,8,9},arrayappend(a,b));
	  }

	  /** Test appending of an array with a single element to the one with many. */
	  @Test
	  public void testArrayAppend2OK()
	  {
	    byte a[] = new byte[] { 1,2,3,4,5 }, b[] = new byte[] { 6 };
	    compare(new byte[]{1,2,3,4,5,6},arrayappend(a,b));
	  }

	  /** Tests appending of an array to an empty array. */
	  @Test
	  public void testArrayAppendEmptyA()
	  {
	    byte a[] = new byte[] {}, b[] = new byte[] { 6,7,8,9 };
	    compare(new byte[]{6,7,8,9},arrayappend(a,b));
	  }

	  /** Tests appending of an empty array to the one with some elements. */
	  @Test
	  public void testArrayAppendEmptyB()
	  {
	    byte a[] = new byte[] { 1,2,3,4,5 }, b[] = new byte[] { };
	    compare(new byte[]{1,2,3,4,5},arrayappend(a,b));
	  }

	  /** Tests appending to the null array. */
	  @Test
	  public void testArrayAppendNullA()
	  {
	    byte a[] = null, b[] = new byte[] { 6,7,8,9 };
	    compare(b,arrayappend(a,b));
	  }

	  /** Tests appending of the null array. */
	  @Test
	  public void testArrayAppendNullB()
	  {
	    byte a[] = new byte[] { 1,2,3,4,5 }, b[] = null;
	    compare(a,arrayappend(a,b));
	  }

	  /** Tests appending of a single byte, to a non-empty non-null array. */
	  @Test
	  public void testArrayAppendByte()
	  {
	    byte a[] = new byte[] { 1,2,3,4,5 };
	    compare(new byte[]{1,2,3,4,5,6},arrayappend(a,(byte)6));
	  }

	  /** Tests appending of a single byte to a null array. */
	  @Test
	  public void testArrayAppendByteToNull()
	  {
	    compare(new byte[]{6},arrayappend(null,(byte)6));
	  }

	  /** Tests appending of a byte to an empty non-null array. */
	  @Test
	  public void testArrayAppendToEmpty()
	  {
	    compare(new byte[]{6},
	                        arrayappend(new byte[0],(byte)6));
	  }

	  /** Tests appending of an array of bytes to an empty array. */
	  @Test
	  public void testArrayAppendToEmpty2()
	  {
	    compare(new byte[]{6},
	                        arrayappend(new byte[0],new byte[]{6}));
	  }

	  /** Tests appending of an empty array to a non-empty one. */
	  @Test
	  public void testArrayAppendEmptyToNonEmpty()
	  {
	    compare(new byte[]{6},
	                        arrayappend(new byte[]{6},new byte[0]));
	  }

	  /** Tests appending of a null array to the empty non-null array. */
	  @Test
	  public void testArrayAppendNullToEmpty()
	  {
	    compare(new byte[]{},
	                        arrayappend(new byte[0],null));
	  }

	  /** Tests appending of two empty non-null arrays. */
	  @Test
	  public void testArrayAppendEmptyToEmpty()
	  {
	    compare(new byte[0],
	                        arrayappend(new byte[0],new byte[0]));
	  }
	  
	  ////////////////////////////////////////////////////////////////////////
	  // Testing the routines which append sequences of objects

	   /** Compares two arrays and does a <code>fail</code> if
	    * they do not contain equal elements
	    * (according to their <code>equals</code> function).
	    * @param what the first array to compare
	    * @param with the second one
	    */
	   protected void compare(Object [] what, Object [] with)
	   {
	     StringBuffer errMsg = new StringBuffer();
	     if (!ArrayOperations.cmp(what,with,errMsg))
	       fail("The comparison of arrays failed: "+errMsg);
	   }

	  /** Checks that <code>cmp(what,with) == cmp(with,what)</code>.
	   *
	   * @param what the first array to compare
	   * @param with the second one
	   * @return true if the two contain equal elements (according to their <code>equals</code> function).
	   */
	  public boolean testcmp(Object [] what, Object [] with)
	  {
	    boolean resultA = ArrayOperations.cmp(what,with,null), resultB = ArrayOperations.cmp(with,what,null);
	    Assert.assertEquals(resultA,resultB);

	    return resultA;
	  }

	  /** Tests arrays without nesting. */
	  @Test
	  public void testCompareA()
	  {
	    Assert.assertTrue(testcmp(new Object[0],new Object[0]));
	    Assert.assertTrue(testcmp(null,null));
	    Assert.assertFalse(testcmp(null,new Object[0]));
	    Assert.assertFalse(testcmp(new Object[0],new Object[]{"a"}));

	    Assert.assertTrue(testcmp(new Object[] { "a","b" }, new Object[] { "a","b" }));
	    Assert.assertTrue(testcmp(new Object[] { "a",null, "b" }, new Object[] { "a",null, "b" }));
	    Assert.assertTrue(testcmp(new Object[] { "a",null }, new Object[] { "a",null }));
	    Assert.assertTrue(testcmp(new Object[] { null }, new Object[] { null }));

	    Assert.assertFalse(testcmp(new Object[] { "a",null }, new Object[] { "a","b" }));
	    Assert.assertFalse(testcmp(new Object[] { null }, new Object[] { new Object() }));
	    Assert.assertFalse(testcmp(new Object[] { "a","c" }, new Object[] { "a","b" }));
	    Assert.assertFalse(testcmp(new Object[] { "a" }, new Object[] { "a","b" }));
	  }

	  /** Tests nested arrays. */
	  @Test
	  public void testCompareB()
	  {
	    Assert.assertTrue(testcmp(new Object[] {
	                              new Object[] { "a",null, "b" }, "a","b",new Object[] { "a",null, "b" } },
	                              new Object[] {
	                              new Object[] { "a",null, "b" }, "a","b",new Object[] { "a",null, "b" } }));

	    Assert.assertTrue(testcmp(new Object[] {
	                              new Object[] { "a",null, new Object[] { "a",null, "b" }, "b" }, "a","b",new Object[] { "a",null, "b" } },
	                              new Object[] {
	                              new Object[] { "a",null, new Object[] { "a",null, "b" }, "b" }, "a","b",new Object[] { "a",null, "b" } }));

	    Assert.assertFalse(testcmp(new Object[] {
	                             new Object[] { "a",null, new Object[] { "a",null }, "b" }, "a","b",new Object[] { "a",null, "b" } },
	                             new Object[] {
	                             new Object[] { "a",null, new Object[] { "a",null, "b" }, "b" }, "a","b",new Object[] { "a",null, "b" } }));

	    Assert.assertFalse(testcmp(new Object[] {
	                            new Object[] { "a",null, new Object[] { "a", "b" }, "b" }, "a","b",new Object[] { "a",null, "b" } },
	                            new Object[] {
	                            new Object[] { "a",null, new Object[] { "a",null, "b" }, "b" }, "a","b",new Object[] { "a",null, "b" } }));

	    Assert.assertFalse(testcmp(new Object[] {
	                         new Object[] { "a",null, new Object[] { "a",null, "b" }, "c" }, "a","b",new Object[] { "a",null, "b" } },
	                         new Object[] {
	                         new Object[] { "a",null, new Object[] { "a",null, "b" }, "b" }, "a","b",new Object[] { "a",null, "b" } }));

	    Assert.assertFalse(testcmp(new Object[] {
	                            new Object[] { "a",null, new Object[] { "a",null, "b" }, "b" }, "b",new Object[] { "a",null, "b" } },
	                            new Object[] {
	                            new Object[] { "a",null, new Object[] { "a",null, "b" }, "b" }, "a","b",new Object[] { "a",null, "b" } }));

	    Assert.assertFalse(testcmp(new Object[] {
	                         new Object[] { "a",null, new Object[] { "a",null, "b" }, "b" }, "a","b",new Object[] { "a",null } },
	                         new Object[] {
	                         new Object[] { "a",null, new Object[] { "a",null, "b" }, "b" }, "a","b",new Object[] { "a",null, "b" } }));
	 }

	  ////////////////////////////////////////////////////////////////////////
	  // Testing the routines which append sequences of objects


	  /** Test appending of non-empty non-null sequences. */
	  @Test
	  public void testObjArrayAppendOK()
	  {
	    Object a[] = new Object[] { "1","2","3","4","5" }, b[] = new Object[] { "6","7","8","9" };
	    compare(new Object[]{"1","2","3","4","5","6","7","8","9"},
	                        ArrayOperations.arrayappend(a,b));
	  }

	  /** Tests appending of non-empty non-null sequences where one of them contains a null object. */
	  @Test
	  public void testObjArrayAppendOK2()
	  {
	    Object a[] = new Object[] { "1","2","3",null,"5" }, b[] = new Object[] { "6","7","8","9" };
	    compare(new Object[]{"1","2","3",null,"5","6","7","8","9"},
	                        ArrayOperations.arrayappend(a,b));
	  }

	  /** Tests appending of a non-empty non-null sequence to the one containing a single element. */
	  @Test
	  public void testArrayObjAppend1OK()
	  {
	    Object a[] = new Object[] { "1" }, b[] = new Object[] { "6","7","8","9" };
	    compare(new Object[]{"1","6","7","8","9"},
	                        ArrayOperations.arrayappend(a,b));
	  }

	  /** Tests appending of a singleton sequence to a non-empty non-null sequence. */
	  @Test
	  public void testArrayObjAppend2OK()
	  {
	    Object a[] = new Object[] { "1","2","3","4","5" }, b[] = new Object[] { "6" };
	    compare(new Object[]{"1","2","3","4","5","6"},
	                        ArrayOperations.arrayappend(a,b));
	  }

	  /** Tests appending of a non-empty non-null sequence, to an empty sequence. */
	  @Test
	  public void testArrayObjAppendEmptyA()
	  {
	    Object a[] = new Object[] { }, b[] = new Object[] { "6","7","8","9" };
	    compare(new Object[]{"6","7","8","9"},
	                        ArrayOperations.arrayappend(a,b));
	  }


	  /** Tests appending of an empty sequence to a non-empty non-null sequence. */
	  @Test
	  public void testArrayObjAppendEmptyB()
	  {
	    Object a[] = new Object[] { "1","2","3","4","5" }, b[] = new Object[] {  };
	    compare(new Object[]{"1","2","3","4","5"},
	                        ArrayOperations.arrayappend(a,b));
	  }

	  /** Test appending of an empty sequence to a null sequence. */
	  @Test
	  public void testArrayObjAppendNullA()
	  {
	    Object b[] = new Object[] { "6","7","8","9" };
	    compare(b,
	                        ArrayOperations.arrayappend(null,b));
	  }

	  /** Tests appending of a null sequence. */
	  @Test
	  public void testArrayObjAppendNullB()
	  {
	    Object a[] = new Object[] { "1","2","3","4","5" };
	    compare(a,
	                        ArrayOperations.arrayappend(a,null));
	  }

	  /** Tests appending of a single element to a non-empty non-null sequence. */
	  @Test
	  public void testArrayObjAppendObject()
	  {
	    Object a[] = new Object[] { "1","2","3","4","5" };
	    compare(new Object[]{"1","2","3","4","5","6"},
	                        ArrayOperations.arrayappend(a,"6"));
	  }

	  /** Test appending of a single element to a null sequence. */
	  @Test
	  public void testArrayObjAppendToNull()
	  {
	    compare(new Object[]{"6"},
	                        ArrayOperations.arrayappend(null,"6"));
	  }

	  /** Test appending of a null element to a null sequence. */
	  @Test
	  public void testArrayObjAppendNullToNull()
	  {
	    compare(new Object[]{null},
	                        ArrayOperations.arrayappend(null,(Object)null));
	  }

	  /** Test appending of a null element to a singleton sequence. */
	  @Test
	  public void testArrayObjAppendNullToNonNull()
	  {
	    compare(new Object[]{"1",null},
	                        ArrayOperations.arrayappend(new Object[]{"1"},(Object)null));
	  }

	  /** Test appending of a single element to an empty sequence. */
	  @Test
	  public void testArrayObjAppendToEmpty()
	  {
	    compare(new Object[]{"6"},
	                        ArrayOperations.arrayappend(new Object[0],"6"));
	  }

	  /** Test appending of a singleton sequence to a null sequence. */
	  @Test
	  public void testArrayObjAppendToEmpty2()
	  {
	    compare(new Object[]{"6"},
	                        ArrayOperations.arrayappend(new Object[0],new Object[]{"6"}));
	  }

	  /** Test appending of an empty sequence to a singleton sequence. */
	  @Test
	  public void testArrayObjAppendEmptyToNonEmpty()
	  {
	    compare(new Object[]{"6"},
	                        ArrayOperations.arrayappend(new Object[]{"6"},new Object[0]));
	  }

	  /** Test appending of a null element to an emtpy sequence. */
	  @Test
	  public void testArrayObjAppendNullToEmpty()
	  {
	    compare(new Object[]{null},
	                        ArrayOperations.arrayappend(new Object[0],(Object)null));
	  }

	  /** Test appending of an empty sequence, to an emtpy sequence. */
	  @Test
	  public void testArrayObjAppendEmptyToEmpty()
	  {
	    compare(new Object[0],
	                        ArrayOperations.arrayappend(new Object[0],new Object[0]));
	  }

	  ////////////////////////////////////////////////////////////////////////////
	  // testing of the power function

	  /** Tests a raise of a singleton sequence to the power of 0. */
	  @Test
	  public void testPower0()
	  {
	    Assert.assertNull("the outcome is not null",ArrayOperations.power(new Object[]{"hello"},0));
	  }

	  /** Tests a raise of a null element. */
	  @Test
	  public void testPowerNull()
	  {
	    Assert.assertNull("the outcome is not null",ArrayOperations.power(null,1));
	  }

	  /** Tests a raise of a singleton sequence to the power of 1. */
	  @Test
	  public void testPower1()
	  {
	    compare(new Object[] {"hello"},ArrayOperations.power(new Object[]{"hello"},1));
	  }

	  /** Tests a raise of a singleton sequence to the power of 2. */
	  @Test
	  public void testPower2()
	  {
	    compare(new Object[] {"hello","hello"},ArrayOperations.power(new Object[]{"hello"},2));
	  }

	  /** Tests a raise of a sequence with more than one element, to the power of 2. */
	  @Test
	  public void testPowerA()
	  {
	    compare(new Object[] {"A","B","A","B"},ArrayOperations.power(new Object[]{"A","B"},2));
	  }

	  ////////////////////////////////////////////////////////////////////////////
	  // testing of the powerSeries function

	  /** Tests a series of sequence, with no elements in it. */
	  @Test
	  public void testSeries0()
	  {
	    Assert.assertNull("the outcome is not null",ArrayOperations.series(new Object[]{"hello"},0));
	  }

	  /** Tests a series of a null sequence. */
	  @Test
	  public void testSeriesNull()
	  {
	    Assert.assertNull("the outcome is not null",ArrayOperations.series(null,2));
	  }

	  /** Tests a 1-series of a singleton sequence. */
	  @Test
	  public void testSeries1()
	  {
	    compare(new Object[] {null,new Object[]{"hello"}},ArrayOperations.series(new Object[]{"hello"},1));
	  }

	  /** Tests a 2-series of a singleton sequence. */
	  @Test
	  public void testSeries2()
	  {
	    compare(new Object[] {null,new Object[]{"hello"},new Object[]{"hello", "hello"}},ArrayOperations.series(new Object[]{"hello"},2));
	  }

	  ////////////////////////////////////////////////////////
	  // Testing flatten

	  /** Compares two sets of sequences.
	   * @param what a sequence of objects to compare.
	   * @param with the sequence to compare it with.
	   */
	  protected void setseqCompare(Object [][]what, Object [][]with)
	  {
	    class comp implements Comparator
	    {
	        public int compare( Object cmp_what, Object cmp_with )
	        {
	          if ( cmp_what == null || cmp_with == null ||
	               ! ( cmp_what instanceof Object[] ) || ! ( cmp_with instanceof Object[] ) )
	            fail( "wrong arguments passed to Comparator" );

	          Object a[] = ( Object[] ) cmp_what, b[] = ( Object[] ) cmp_with;
	          if ( a.length < b.length )
	            return -1;
	          else
	          if ( a.length > b.length )
	            return 1;
	          else {
	            for ( int i = 0; i < a.length; ++i )
	            {
	              if ( a[i] == null || b[i] == null )
	                fail( "Element " + i + " of a sequence is null" );

	              if (a[i] != ArrayOperations.EndOfSequence.End() || b[i] != ArrayOperations.EndOfSequence.End())
	              {
	                if ( ! ( a[i] instanceof String ) || ! ( b[i] instanceof String ) )
	                  fail( "Element " + i + " of a sequence is not a string" );

	                int result = ( ( String ) a[i] ).compareTo( ( String ) b[i] );
	                if ( result != 0 )
	                  return result;
	              }
	            }

	            return 0;
	          }
	        }

	        public boolean equals( Object obj )
	        {
	          return this == obj;
	        }
	      } // class comp

	    TreeSet<Object[]> first = new TreeSet<Object[]>(new comp()), second = new TreeSet<Object[]>(new comp());
	    first.addAll(Arrays.asList(what));second.addAll(Arrays.asList(with));

	    if (!first.equals(second))
	    {
	      //log.debug("the result received is :");
	      StringBuffer str = new StringBuffer();str.append("Expected: ");
	      for(Object[] elem:first)
	      {
	        for(Object seq:elem)
	        {
	          str.append(seq.toString());str.append(ArrayOperations.separator);
	        }
	      }
	      str.append("; Received: ");
	      for(Object[] elem:second)
	      {
	        for(Object seq:elem)
	        {
	          str.append(seq.toString());str.append(ArrayOperations.separator);
	        }
	      }
	      fail("flatten returned the wrong result");
	    }
	  }

  /** Tests flattening of an array with no nested arrays. */
  public void testFlatten_simple()
  {
    setseqCompare(new Object[][] { new Object[]{"a","b","c"} }, ArrayOperations.flatten(new Object[] { "a","b","c" }));
  }
	
  /** Tests flatteinig of an array with some nested arrays. */
  @Test
  public void testFlatten_simple2()
  {
	setseqCompare(new Object[][] { new Object[]{"a","b","c"} },
	                 ArrayOperations.flatten(new Object[] { new Object[]{"a"},"b",new Object[]{"c"} }));
  }
	
  /** Tests flattening of an array with nested singleton arrays and null elements. */
  @Test
  public void testFlatten_simple3()
  {
	  setseqCompare(new Object[][] { new Object[]{"a","b","c"} },
	                ArrayOperations.flatten(new Object[] { new Object[]{"a"},"b",new Object[]{"c"}, null,null,null }));
  }
	
  /** Tests flattening of an array with a null at the beginning and some singleton arrays. */
  @Test
  public void testFlatten_simple4()
  {
	 setseqCompare(new Object[][] { new Object[]{"a","b","c"} },
	               ArrayOperations.flatten(new Object[] { null, new Object[]{"a"},"b",new Object[]{"c"} }));
  }
	
  /** Tests flattening of an array with nested sequences, containing more than one element. */
  @Test
  public void testFlatten_A()
  {
	  setseqCompare(new Object[][] { new Object[]{"a","bA","c"},new Object[]{"a","bB","c"} },
	          ArrayOperations.flatten(new Object[] { "a",new Object[] {"bA", "bB"} ,"c" }));
  }
	
  /** Tests that if there are two non-singleton arrays included in the array to flatten,
   * all combinations of all elements of these arrays are built.
   */
  @Test
  public void testFlatten_B()
  {
	setseqCompare(new Object[][] { new Object[]{"a","bA","c","dA"},new Object[]{"a","bB","c","dA"},
          new Object[]{"a","bA","c","dB"},new Object[]{"a","bB","c","dB"} },
          ArrayOperations.flatten(new Object[] { "a",new Object[] {"bA", "bB"} ,"c",new Object[] {"dA", "dB"} }));
  }

  /** Tests that if there are two non-singleton arrays included in the array to flatten,
   * all combinations of all elements of these arrays are built.
   */
  @Test
  public void testFlatten_C()
  {
    setseqCompare(new Object[][] { new Object[]{"a","bA","dA"},new Object[]{"a","bA","dB"},
	          new Object[]{"a","bB","dA"},new Object[]{"a","bB","dB"} },
	          ArrayOperations.flatten(new Object[] { "a",new Object[] {"bA", "bB"} ,null,new Object[] {"dA", "dB"} }));
  }
	
  /** Tests that if there are two non-singleton arrays included in the array to flatten,
   * all combinations of all elements of these arrays are built.
   */
  @Test
  public void testFlatten_D()
  {
    setseqCompare(new Object[][] {
              new Object[]{"a","bA","cA","dA"},new Object[]{"a","bB","cA","dA"},
	          new Object[]{"a","bA","cA","dB"},new Object[]{"a","bB","cA","dB"},
	          new Object[]{"a","bA","cB","dA"},new Object[]{"a","bB","cB","dA"},
	          new Object[]{"a","bA","cB","dB"},new Object[]{"a","bB","cB","dB"},
	          new Object[]{"a","bA","cC","dA"},new Object[]{"a","bB","cC","dA"},
	          new Object[]{"a","bA","cC","dB"},new Object[]{"a","bB","cC","dB"} },
	          ArrayOperations.flatten(new Object[] { "a",new Object[] {"bA", "bB"} ,
	                              new Object[] {"cA", "cB", "cC"},new Object[] {"dA", "dB"} }));
  }
	
  /** Tests that if there are two non-singleton arrays included in the array to flatten,
   * all combinations of all elements of these arrays are built.
   */
  @Test
  public void testFlatten_E()
  {
    setseqCompare(new Object[][] { new Object[]{"a","bA","cA", "dA"},new Object[]{"a","bB","cA", "dA"},
	          new Object[]{"a","bA","cA", "dB"},new Object[]{"a","bB","cA", "dB"} },
	          ArrayOperations.flatten(new Object[] { "a",new Object[] {"bA", "bB"} ,
	                              new Object[] {"cA" },new Object[] {"dA", "dB"} }));
  }
	
  /** This one tests alternation between `sequence' and `choice'. Initially, everything is a sequence of
   * choices. Within a choice, one can have another sequence of choices etc.
   */
  @Test
  public void testFlatten_F()
  {
    setseqCompare(new Object[][] { new Object[]{"a","bA","c"},new Object []{"a","s","e","q","c"},new Object[]{"a","bB","c"},new Object[]{"a","c"} },
	          ArrayOperations.flatten(new Object[] { "a",null,new Object[] {"bA", new Object [] {"s","e","q"},null,"bB"} ,"c" }));
  }
	
  /** This one tests alternation between `sequence' and `choice'. Initially, everything is a sequence of
   * choices. Within a choice, one can have another sequence of choices etc.
   */
  @Test
  public void testFlatten_G()
  {
    setseqCompare(new Object[][] { new Object[]{"a","bA","c"},new Object []{"a","s","e","q","c"},new Object []{"a","s","f","q","c"},new Object[]{"a","bB","c"} },
          ArrayOperations.flatten(new Object[] { "a",null,new Object[] {"bA", new Object [] {"s",new Object[]{"e","f"},"q"},"bB"} ,"c" }));
  }
	
  /** Tests flattening of an empty array. */
  @Test
  public void testFlatten_empty()
  {
    setseqCompare(new Object[0][],ArrayOperations.flatten(new Object[0]));
  }
	
  /** Test flattening of an array containing a nested empty array. */
  @Test
  public void testFlatten_empty2()
  {
    setseqCompare(new Object[0][],ArrayOperations.flatten(new Object[]{new Object[0]}));
  }
	
  /** Tests flattening of a null array. */
  @Test
  public void testFlatten_null0()
  {
    Assert.assertNull(ArrayOperations.flatten(null));
  }
	
  /** Tests flattening of an array containing nulls at different levels of nestedness. */
  @Test
  public void testFlatten_null1()
  {
    setseqCompare(new Object[][] { new Object[]{"a","bA","c"},new Object[]{"a","bB","c"},new Object [] {"a","c"} },
	          ArrayOperations.flatten(new Object[] { "a",null,new Object[] {"bA", null,null,"bB"} ,"c" }));

  }
	
  /** Tests flattening of an array with many nulls, at different levels. */
  public void testFlatten_null2()
  {
    setseqCompare(new Object[][] { },
                  ArrayOperations.flatten(new Object[] { null,null,new Object[] {null, null,null,null} ,null }));
  }
	
  /** Tests flattening of a singleton array with containing a null element. */
  public void testFlatten_null3()
  {
    setseqCompare(new Object[][] { },
                  ArrayOperations.flatten(new Object[] { null }));
  }
	
  /** Tests that End() actually terminates sequences. */
  public void testFlatten_End()
  {
    setseqCompare(new Object[][] { new Object[]{"a","bA", ArrayOperations.EndOfSequence.End()},new Object[]{"a","bB","c"} },
        ArrayOperations.flatten(new Object[] { "a",new Object[] {new Object [] { "bA", ArrayOperations.EndOfSequence.End() }, "bB"} ,"c" }));
  }
	
}
