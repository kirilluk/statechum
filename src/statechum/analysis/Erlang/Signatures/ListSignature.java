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

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;

/**
 *
 * @author ramsay
 */
public class ListSignature extends Signature {

	/** This determines possible elements in the list, any combination of them is allowed.
	 * terminator is the type of the last element in the list.
	 */
    final public Signature elems,terminator;
    final public boolean nonempty, maybeimproper,improper,any;

	public static final OtpErlangAtom NonEmptyAtom = new OtpErlangAtom("nonempty"),
		ImproperAtom = new OtpErlangAtom("improper"),MaybeImproperAtom = new OtpErlangAtom("maybeimproper"),
		AnyAtom = new OtpErlangAtom("any");
    
	/** Constructs this signature
	 * 
	 * @param attributes if it is non-empty or possibly improper
	 * @param values possible values, ignored if attribute any is defined.
	 */
    public ListSignature(OtpErlangList attributes,OtpErlangList values) {
        super();
        boolean nonemptyValue = false, maybeimproperValue = false,improperValue = false, anyValue = false;
        for(OtpErlangObject obj:attributes.elements())
        	if (obj.equals(NonEmptyAtom)) nonemptyValue = false;
        	else
        		if (obj.equals(ImproperAtom)) improperValue = false;
        		else
            		if (obj.equals(MaybeImproperAtom)) maybeimproperValue = false;
            		else
        			if (obj.equals(AnyAtom)) anyValue = true;
        			else
        				throw new IllegalArgumentException("Unknown attribute "+obj+" in the list of attributes for ListSignature");
        
        nonempty = nonemptyValue;improper = improperValue;
        if (improper) maybeimproper = true;else maybeimproper = maybeimproperValue;
        any = anyValue;
        if (values.arity() == 0) 
        {
        	elems = null;terminator = null;
        }
        else
    	   if (values.arity() == 1)
    	   {
    		   elems = Signature.buildFromType(values.elementAt(0));
    		   terminator = null;
    	   }
    	   else
    		   if (values.arity() == 2)
    		   {// first value - type of elements, second one - type of terminator
    			   elems = Signature.buildFromType(values.elementAt(0));
    			   terminator = Signature.buildFromType(values.elementAt(1));
    		   }
    		   else
    		   throw new IllegalArgumentException("More than a two types of elements of a list - a collection of types should be encoded as a ?union");
        
        if (improper)
        	throw new IllegalArgumentException("improper lists are not currently supported");
    }

    @Override
	public OtpErlangObject instantiate() {
    	if (!nonempty) return new OtpErlangList();
    	
    	Signature elemSig = elems==null?wibbleSignature:elems;
    	return elemSig.instantiate();
    }
    
    @Override
    public List<OtpErlangObject> instantiateAllAlts() {
    	List<OtpErlangObject> result = new LinkedList<OtpErlangObject>();
    	Signature elemSig = elems==null?wibbleSignature:elems;
    	if (!nonempty) result.add(new OtpErlangList());// empty list if permitted 
    	
    	int length = 2;// arbitrary length of a list
    	List<Signature> s = new ArrayList<Signature>(length);
    	for(int i=0;i<length;++i) s.add(elemSig);
    	for(List<OtpErlangObject> listOfValues:Signature.computeCrossProduct(s))
    		result.add(new OtpErlangList(listOfValues.toArray(new OtpErlangObject[0])));
/*    		
    	{// This one computes a cross-product using ListSignatures - not currently used. 
        	List<Signature> tailElems = new LinkedList<Signature>(elems);
            Signature head = tailElems.remove(0);
            ListSignature tail = new ListSignature(tailElems);
            List<OtpErlangObject> headVals = head.instantiateAllAlts();
            List<OtpErlangObject> tailVals = tail.instantiateAllAlts();
            assert !tailVals.isEmpty();
            for (OtpErlangObject h : headVals) {
                for (OtpErlangObject t : tailVals)
                {
                	OtpErlangObject tails[] = ((OtpErlangList)t).elements();
                	OtpErlangObject product [] = new OtpErlangObject[1+tails.length];
                	product[0]=h;System.arraycopy(tails, 0, product, 1, tails.length);
                	result.add(new OtpErlangList(product));
                }
            }
        }
*/
        return result;
    }
}

