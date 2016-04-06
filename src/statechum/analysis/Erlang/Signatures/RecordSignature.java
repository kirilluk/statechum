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
import java.util.Map;
import java.util.TreeMap;

import statechum.Configuration;

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
    
    public RecordSignature(Configuration config, OtpErlangList attributes,OtpErlangList fieldDetails) {
    	if (attributes.arity() != 1) throw new IllegalArgumentException("RecordSignature expects a single attribute containing its name tag");
    	orderedSignatures = new ArrayList<Signature>(fieldDetails.arity());
    	nameTag = (OtpErlangAtom)attributes.elementAt(0);
    	orderedSignatures.add(new AtomSignature(config,new OtpErlangList(),new OtpErlangList(new OtpErlangObject[]{
    			nameTag
    	})));
    	name = nameTag.atomValue();
        fields = new TreeMap<String, Signature>();
        for(OtpErlangObject obj:fieldDetails)
        {
        	OtpErlangTuple nameValue = (OtpErlangTuple)obj;
        	if (nameValue.arity() != 2) throw new IllegalArgumentException("Invalid name-type field "+obj+" passed to RecordSignature");
        	Signature sig = Signature.buildFromType(config, nameValue.elementAt(1));
        	orderedSignatures.add(sig);
        	fields.put(((OtpErlangAtom)nameValue.elementAt(0)).atomValue(),sig);
        }
        erlangTermForThisType = erlangTypeToString(attributes,fieldDetails);
    }

    @Override
    public List<OtpErlangObject> instantiateAllAlts() {
        List<OtpErlangObject> result = new LinkedList<OtpErlangObject>();
        for(List<OtpErlangObject> listOfValues:Signature.computeCrossProduct(orderedSignatures))
        	result.add(new OtpErlangTuple(listOfValues.toArray(new OtpErlangObject[0])));
        return result;
    }
    
    /** Given that orderedSignatures starts with an atom reflecting the tag of this record, 
     * checking is identical to that for the tuple.
     */
    @Override
	public boolean typeCompatible(OtpErlangObject term) 
	{
		if (!(term instanceof OtpErlangTuple)) return false;
		OtpErlangTuple tuple = (OtpErlangTuple)term;
		
		if (tuple.arity() != orderedSignatures.size()) return false;
		
		Iterator<Signature> sigIterator = orderedSignatures.iterator();
		for(int i=0;i<orderedSignatures.size();++i)
			if (!sigIterator.next().typeCompatible(tuple.elementAt(i)))
				return false;
		
		return true;
	}

}
