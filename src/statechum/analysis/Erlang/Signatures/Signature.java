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
import java.util.LinkedList;
import java.util.List;

import statechum.Helper;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

/** Describes the type signature of a function and provides some useful(?) methods for manipulating them.
 *
 * This is deliberately polymorphic with subclasses to support progressive inference of signatures.
 *
 * @author ramsay
 */
public abstract class Signature {

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

    /** Used to instantiate a list of any elements. */
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

   public static List<List<OtpErlangObject>> computeCrossProduct(List<Signature> listOfArgs) {
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

}
