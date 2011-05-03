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
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
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
     
       	int arity = values.arity();
        elems = new ArrayList<Signature>(arity);
        for(int i=0;i<arity;++i) elems.add(Signature.buildFromType(values.elementAt(i)));
		erlangTermForThisType = erlangTypeToString(attributes,values);
   }

    @Override
    public List<OtpErlangObject> instantiateAllAlts() {
        List<OtpErlangObject> result = new LinkedList<OtpErlangObject>();
        for(List<OtpErlangObject> listOfValues:Signature.computeCrossProduct(elems))
        	result.add(new OtpErlangTuple(listOfValues.toArray(new OtpErlangObject[0])));
        return result;
    }

    @Override
	public boolean typeCompatible(OtpErlangObject term) 
	{
		if (!(term instanceof OtpErlangTuple)) return false;
		OtpErlangTuple tuple = (OtpErlangTuple)term;
		
		if (tuple.arity() != elems.size()) return false;
		
		Iterator<Signature> sigIterator = elems.iterator();
		for(int i=0;i<elems.size();++i)
		{
			OtpErlangObject listTerm = tuple.elementAt(i);
			Signature sig = sigIterator.next();
			if (!sig.typeCompatible(tuple.elementAt(i)))
			{
				if (!(listTerm instanceof OtpErlangString) || !sig.typeCompatible(Signature.stringToList(listTerm)))
					return false;
			}
		}
		return true;
	}
}
