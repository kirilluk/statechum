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

/** This represents a set of alternative signatures.
 *
 * This is usually the parse of a BNF signature. So [ 58 | 59 , ... ]
 * is a ListSignature with the first elem an AltSignature with two LiteralSignature elements.
 *
 * @author ramsay
 */
public class AltSignature extends Signature {

    public List<Signature> elems;

    /** Used by the old parser. */
    public AltSignature(List<Signature> e) {
    	elems = e;
    	
    	StringBuffer resultHolder = new StringBuffer();resultHolder.append("{'");
    	resultHolder.append(getSigName(this));
   		resultHolder.append("',[],[");
		boolean innerFirst = true;
		for(Signature sig:elems)
		{
			if (!innerFirst) resultHolder.append(',');else innerFirst = false;
			resultHolder.append(sig.toErlangTerm());
		}
		resultHolder.append("]}");
    }
    
    /** A tuple with elements of known types. */
    public AltSignature(OtpErlangList attributes,OtpErlangList values) {
        super();
		if (attributes.arity() != 0) throw new IllegalArgumentException("AltSignature does not accept attributes");
       	int arity = values.arity();
        elems = new ArrayList<Signature>(arity);
        for(int i=0;i<arity;++i) elems.add(Signature.buildFromType(values.elementAt(i)));
		erlangTermForThisType = erlangTypeToString(attributes,values);
    }

    // Pick the first one...
    @Override
	public OtpErlangObject instantiate() {
        if (elems.isEmpty()) return null;

        return elems.get(0).instantiateAllAlts().get(0);
    }

    @Override
    public List<OtpErlangObject> instantiateAllAlts() {
    	LinkedList<OtpErlangObject> result = new LinkedList<OtpErlangObject>();
        for(Signature s : elems) {
            result.addAll(s.instantiateAllAlts());
        }
        return result;
    }
}
