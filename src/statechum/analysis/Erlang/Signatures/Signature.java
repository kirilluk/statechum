/* Copyright (c) 2011 The University of Sheffield.
 * 
 * This file is part of StateChum
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
 * 
 */
package statechum.analysis.Erlang.Signatures;

import java.lang.reflect.Constructor;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import statechum.Helper;
import statechum.Label;
import statechum.analysis.Erlang.ErlangLabel;
import statechum.analysis.Erlang.ErlangLabel.ErlangBitString;
import statechum.analysis.Erlang.ErlangLabel.ErlangBoolean;
import statechum.analysis.Erlang.ErlangLabel.ErlangDouble;
import statechum.analysis.Erlang.ErlangLabel.ErlangInt;
import statechum.analysis.Erlang.ErlangLabel.ErlangList;
import statechum.analysis.Erlang.ErlangLabel.ErlangLong;
import statechum.analysis.Erlang.ErlangLabel.ErlangParserComponent;
import statechum.analysis.Erlang.ErlangLabel.ErlangQuotedAtom;
import statechum.analysis.Erlang.ErlangLabel.ErlangString;
import statechum.analysis.Erlang.ErlangLabel.ErlangTuple;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBitstr;
import com.ericsson.otp.erlang.OtpErlangBoolean;
import com.ericsson.otp.erlang.OtpErlangDouble;
import com.ericsson.otp.erlang.OtpErlangFloat;
import com.ericsson.otp.erlang.OtpErlangFun;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangPort;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

/** Describes the type signature of a function and provides some useful(?) methods for manipulating them.
 *
 * This is deliberately polymorphic with subclasses to support progressive inference of signatures.
 *
 * @author ramsay
 */
public abstract class Signature implements Label
{
    /** This method should provide a valid (if pointless) instance of the type signature it represents.
     *
     * Currently it will always use 1 for integers and wibble for atoms, and corresponding useful values like [1] and [wibble] for lists.
     *
     */
    public abstract OtpErlangObject instantiate();

    /** This method should provide a collection with one possible instantiation for each
     *  available alternate signature in the parse.
     *
     * Most signatures will just use instantiate() but should provide one instance for 
     * each of their possible children's instances.
     *
     * This should be overridden as necessary by subclasses.
     */
    public List<OtpErlangObject> instantiateAllAlts() {
        List<OtpErlangObject> result = new LinkedList<OtpErlangObject>();
        if (this.instantiate() != null)
        	result.add(this.instantiate());
        return result;
    }


    /** Used to instantiate a list of "any" elements. */
    protected static final AtomSignature wibbleSignature = new AtomSignature(new OtpErlangList(),new OtpErlangList(new OtpErlangObject[]{new OtpErlangAtom("wibble")}));

    /** Given an Erlang type encoded as an object, constructs an instance of a corresponding type. */
	public static Signature buildFromType(OtpErlangObject elementAt) 
	{
		Signature result = null;
		
		try
		{
			if (elementAt instanceof OtpErlangTuple)
			{// multi-arg constructor, turn elements of the tuple into arguments for the constructor. 
				OtpErlangTuple argTuple = (OtpErlangTuple)elementAt;
				if (argTuple.arity() < 2) throw new IllegalArgumentException("invalid type argument: a list with arity of less than two");
				final int argumentNumber = argTuple.arity()-1;
				Class<OtpErlangList> []argTypes = new Class[argumentNumber];
				for(int i=0;i<argumentNumber;++i) argTypes[i]=OtpErlangList.class;
				@SuppressWarnings("unchecked")
				Class<Signature> sigClass = (Class<Signature>)Class.forName("statechum.analysis.Erlang.Signatures."+((OtpErlangAtom)argTuple.elementAt(0)).atomValue()+"Signature");
				Constructor<Signature> constructor = sigClass.getConstructor(argTypes);
				Object []values = new OtpErlangList[argumentNumber];
				System.arraycopy(argTuple.elements(), 1, values, 0, argumentNumber);
				result = constructor.newInstance(values);
			}
			else
				throw new IllegalArgumentException("invalid type argument "+elementAt+" : it has to be either an atom or a list"); 
		}
		catch(IllegalArgumentException ex)
		{// if one of the constructors or some of the consistency checks failed, re-throw the exception.
			throw ex;
		}
		catch(Exception ex)
		{// failed to create instance
			Helper.throwUnchecked("Failed to create a type instance corresponding to "+elementAt, ex);
		}
		return result;
	}

	/** Returns a shortened class name which is the first atom in a type signature returned by the
	 * modified typer.
	 * @return reduced class name.
	 */
	public static String getSigName(Object obj)
	{
		String name = obj.getClass().getName();
		name = name.substring(name.lastIndexOf('.')+1);name = name.substring(0,name.indexOf("Signature"));
		return name;
	}
	
	/** Used by subclasses of Signature to build string representation. */
   public String erlangTypeToString(OtpErlangList listA, OtpErlangList listB)
   {
		List<OtpErlangObject> details = new LinkedList<OtpErlangObject>();
		details.add(new OtpErlangAtom(getSigName(this)));
		if (listA != null)
		{
			details.add(listA);
			if (listB != null)
				details.add(listB);
		}
		else 
			throw new IllegalArgumentException("listA is null but listB is not");
		
		return ErlangLabel.dumpErlangObject(new OtpErlangTuple(details.toArray(new OtpErlangObject[0])));
   }
   
   public static List<List<OtpErlangObject>> computeCrossProduct(List<Signature> listOfArgs) 
   {
    	assert !listOfArgs.isEmpty();
        LinkedList<List<OtpErlangObject>> res = new LinkedList<List<OtpErlangObject>>();
    	List<Signature> tail = new LinkedList<Signature>(listOfArgs);
        Signature head = tail.remove(0);
        List<OtpErlangObject> headVals = head.instantiateAllAlts();

        if (!tail.isEmpty()) 
        {
            List<List<OtpErlangObject>> tailVals = computeCrossProduct(tail);
            assert !tailVals.isEmpty();
            for (OtpErlangObject h : headVals) {
                for (List<OtpErlangObject> t : tailVals)
                {
                	List<OtpErlangObject> product = new LinkedList<OtpErlangObject>();product.add(h);
                	product.addAll(t);
                	res.add(product);
                }
            }
        }
        else
        	// tail is empty
            for (OtpErlangObject h : headVals)
            {
            	List<OtpErlangObject> product = new LinkedList<OtpErlangObject>();product.add(h);
            	res.add(product);
            }
        
        return res;
    }

   
   /** Represents an Erlang term from which details of this signature can be reconstructed - this is 
    * currently only used to compare labels due to their immutability but can easily be 
    * used for serialisation.
    */
   protected String erlangTermForThisType = null;

	@Override
	public int compareTo(Label o) {
		return toErlangTerm().compareTo(((Signature)o).toErlangTerm());
	}
	
	@Override
	public String toErlangTerm() {
		return erlangTermForThisType;
	}

	@Override
	public boolean equals(Object o)
	{
		return toErlangTerm().equals(((Label)o).toErlangTerm());
	}
	
	@Override
	public int hashCode()
	{
		return toErlangTerm().hashCode();
	}
}
