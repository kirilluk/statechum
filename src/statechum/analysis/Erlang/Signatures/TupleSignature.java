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

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 *
 * @author ramsay
 */
public class TupleSignature extends Signature {

    protected final List<Signature> elems;

    /** Used by the old parser. */
    public TupleSignature(List<Signature> e)
    {
    	elems = e;
    }
    
    /** A tuple with an arbitrary number of elements of unknown types. */
    public TupleSignature(OtpErlangList attributes) {
        super();
		if (attributes.arity() != 0) throw new IllegalArgumentException("TupleSignature does not accept attributes");
		int arity = 2;// arbitrary value
        elems = new ArrayList<Signature>(arity);
        for(int i=0;i<arity;++i) elems.add(wibbleSignature);
		erlangTermForThisType = erlangTypeToString(attributes,null);
    }

    /** A tuple with elements of known types. */
    public TupleSignature(OtpErlangList attributes,OtpErlangList values) {
        super();
		if (attributes.arity() != 0) throw new IllegalArgumentException("TupleSignature does not accept attributes");
/*		
       int arity = 0;
        for(OtpErlangObject obj:attributes.elements())
        	if (obj instanceof OtpErlangInt) 
        	{
        		try {
					arity = ((OtpErlangInt)obj).intValue();
				} catch (OtpErlangRangeException e) {
					Helper.throwUnchecked("Failed to convert the supplied value to integer", e);
				}
        	}
        	else
  				throw new IllegalArgumentException("Unknown attribute "+obj+" in the list of attributes for TupleSignature");
*/      
       	int arity = values.arity();
        elems = new ArrayList<Signature>(arity);
        for(int i=0;i<arity;++i) elems.add(Signature.buildFromType(values.elementAt(i)));
		erlangTermForThisType = erlangTypeToString(attributes,values);
   }

    @Override
	public OtpErlangObject instantiate() {
    	OtpErlangObject elements[] = new OtpErlangObject[elems.size()];
    	int i=0;
        for (Signature e : elems) {
        	elements[i++] = e.instantiate();
        }
        return new OtpErlangTuple(elements);
   }

    @Override
    public List<OtpErlangObject> instantiateAllAlts() {
        List<OtpErlangObject> result = new LinkedList<OtpErlangObject>();
        for(List<OtpErlangObject> listOfValues:Signature.computeCrossProduct(elems))
        	result.add(new OtpErlangTuple(listOfValues.toArray(new OtpErlangObject[0])));
        
/* this was originally written to generate tuples, but I have since generalised it into computeCrossProduct
    	List<Signature> tailElems = new LinkedList<Signature>(elems);
        Signature head = tailElems.remove(0);
        ListSignature tail = new ListSignature(tailElems);
        List<OtpErlangObject> headVals = head.instantiateAllAlts();
        List<OtpErlangObject> tailVals = tail.instantiateAllAlts();
        assert !tailVals.isEmpty();
        for (OtpErlangObject h : headVals) {
            for (OtpErlangObject t : tailVals)
            {
            	OtpErlangObject tails[] = ((OtpErlangTuple)t).elements();
            	OtpErlangObject product [] = new OtpErlangObject[1+tails.length];
            	product[0]=h;System.arraycopy(tails, 0, product, 1, tails.length);
            	result.add(new OtpErlangTuple(product));
            }
        }

*/
        return result;

    }
}
