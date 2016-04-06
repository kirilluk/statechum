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

import statechum.Configuration;

import com.ericsson.otp.erlang.OtpErlangAtom;
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
    public TupleSignature(Configuration config, OtpErlangList attributes) {
        super();
		if (attributes.arity() != 0) throw new IllegalArgumentException("TupleSignature does not accept attributes");
		int arity = 2;// arbitrary value
        elems = new ArrayList<Signature>(arity);
        final AtomSignature wibbleTupleSignature = new AtomSignature(config,new OtpErlangList(),
    			new OtpErlangList(new OtpErlangObject[] { new OtpErlangAtom("Awibble") }));
        for(int i=0;i<arity;++i) elems.add(wibbleTupleSignature);
		erlangTermForThisType = erlangTypeToString(attributes,null);
    }

    /** A tuple with elements of known types. */
    public TupleSignature(Configuration config, OtpErlangList attributes,OtpErlangList values) {
        super();
		if (attributes.arity() != 0) throw new IllegalArgumentException("TupleSignature does not accept attributes");
     
       	int arity = values.arity();
        elems = new ArrayList<Signature>(arity);
        for(int i=0;i<arity;++i) elems.add(Signature.buildFromType(config, values.elementAt(i)));
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
			Signature sig = sigIterator.next();
			if (!sig.typeCompatible(tuple.elementAt(i)))
				return false;
		}
		return true;
	}
}
