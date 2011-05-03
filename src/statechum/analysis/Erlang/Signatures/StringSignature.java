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
import java.util.Collections;
import java.util.List;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;

/**
 *
 * @author ramsay
 */
public class StringSignature extends Signature {

	protected final List<OtpErlangObject> values;
	protected final boolean nonEmpty;
		
	public StringSignature(OtpErlangList attributes)
	{
		if (attributes.arity() > 0) 
			nonEmpty = attributes.elementAt(0).equals(ListSignature.NonEmptyAtom);
		else 
			nonEmpty = false;
		
		values = null;
		erlangTermForThisType = erlangTypeToString(attributes,null);
	}
	
	public StringSignature(OtpErlangList attributes, OtpErlangList valuesArg)
	{
		if (attributes.arity() > 0) throw new IllegalArgumentException("no attributes supported for a string with an explicit collection of values");
		values = new ArrayList<OtpErlangObject>(valuesArg.arity());
		boolean nonEmptyValue = true;
		for(int i=0;i<valuesArg.arity();++i)
		{
			if (valuesArg.elementAt(i) instanceof OtpErlangList)
			{
				if ( ((OtpErlangList)valuesArg.elementAt(i)).arity() > 0)
					throw new IllegalArgumentException("Cannot build a string from a non-string list "+ valuesArg);
				values.add(new OtpErlangString(""));// empty string
				nonEmptyValue = false;
			}
			else
			{
				if (!(valuesArg.elementAt(i) instanceof OtpErlangString))
					throw new IllegalArgumentException("Cannot build a string from a list "+ valuesArg + " with non-string values");
				if  (((OtpErlangString)valuesArg.elementAt(i)).stringValue().isEmpty())
					nonEmptyValue = false;

				values.add(valuesArg.elementAt(i));
			}
		}
		nonEmpty = nonEmptyValue;
		erlangTermForThisType = erlangTypeToString(attributes,valuesArg);
	}

    @Override
	public List<OtpErlangObject> instantiateAllAlts() {
    	List<OtpErlangObject> outcome = null;
    	if (values == null)
    	{// values are not constrained
    		if (nonEmpty) outcome = Collections.singletonList((OtpErlangObject)new OtpErlangString("wibble"));
    		else outcome = Collections.singletonList((OtpErlangObject)new OtpErlangString(""));
    	}
    	else
    		outcome = values;
    	
    	return outcome;
    }

	@Override
	public boolean typeCompatible(OtpErlangObject term) {
		if (term instanceof OtpErlangList)
		{
			return !nonEmpty && ( ((OtpErlangList)term).arity() == 0);
		}
		if (!(term instanceof OtpErlangString)) return false;
		String str = ((OtpErlangString) term).stringValue();
		if (str.isEmpty() && nonEmpty) return false;
		
		if (values != null && !values.contains(term)) return false;
		return true;
	}
}
