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
import java.util.Map;
import java.util.TreeMap;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 *
 * @author ramsay
 */
public class RecordSignature extends Signature {

	protected final OtpErlangAtom nameTag;
    protected final String name;
    protected final Map<String, Signature> fields;
    protected final ArrayList<Signature> orderedSignatures;
    
    public RecordSignature(OtpErlangList attributes,OtpErlangList fieldDetails) {
    	if (attributes.arity() != 1) throw new IllegalArgumentException("RecordSignature expects a single attribute containing its name tag");
    	orderedSignatures = new ArrayList<Signature>(fieldDetails.arity());
    	nameTag = (OtpErlangAtom)attributes.elementAt(0);
    	name = nameTag.atomValue();
        fields = new TreeMap<String, Signature>();
        for(OtpErlangObject obj:fieldDetails)
        {
        	OtpErlangTuple nameValue = (OtpErlangTuple)obj;
        	if (nameValue.arity() != 2) throw new IllegalArgumentException("Invalid name-type field "+obj+" passed to RecordSignature");
        	Signature sig = Signature.buildFromType(nameValue.elementAt(1));
        	orderedSignatures.add(sig);
        	fields.put(((OtpErlangAtom)nameValue.elementAt(0)).atomValue(),sig);
        }
    }

    @Override
	public OtpErlangObject instantiate() {
    	OtpErlangObject elements[] = new OtpErlangObject[fields.size()+1];
    	int i=0;
    	elements[i++]=nameTag;
        for (Signature e : orderedSignatures) {
        	elements[i++] = e.instantiate();
        }
        return new OtpErlangTuple(elements);
    }

    @Override
    public List<OtpErlangObject> instantiateAllAlts() {
        List<OtpErlangObject> result = new LinkedList<OtpErlangObject>();
        for(List<OtpErlangObject> listOfValues:Signature.computeCrossProduct(orderedSignatures))
        {
        	List<OtpErlangObject> tuple = new ArrayList<OtpErlangObject>(orderedSignatures.size()+1);
        	tuple.add(nameTag);tuple.addAll(listOfValues);
        	result.add(new OtpErlangTuple(tuple.toArray(new OtpErlangObject[0])));
        }
        return result;
    }

}
