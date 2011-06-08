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

import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import statechum.Helper;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;

/**
 *
 * @author ramsay
 */
public class IntSignature extends Signature {

    public final long lower;
    public final long upper;
    
    // Collection of specific values, if not null.
    // If null, any number between lower and upper is a valid number.
    public final Set<Long> values;
    
	public static final OtpErlangAtom PositiveAtom = new OtpErlangAtom("positive"),
	ValuesAtom = new OtpErlangAtom("values"),
	BoundariesAtom = new OtpErlangAtom("boundaries"),
	NegativeAtom = new OtpErlangAtom("negative"),
	NonNegativeAtom = new OtpErlangAtom("nonnegative");

    public IntSignature(OtpErlangList attributes,OtpErlangList boundariesOrValues) 
    {
    	long lowerValue = Long.MIN_VALUE,upperValue = Long.MAX_VALUE;Set<Long> valuesValue = null;
    	try
    	{
	        for(OtpErlangObject obj:attributes.elements())
	        	if (obj.equals(BoundariesAtom))
	        	{
	    			lowerValue = ((OtpErlangLong)boundariesOrValues.elementAt(0)).intValue();
	    			upperValue = ((OtpErlangLong)boundariesOrValues.elementAt(1)).intValue();
	        	}
	        	else
	        		if (obj.equals(ValuesAtom))
	        		{
	        			valuesValue = new TreeSet<Long>();
	        			for(OtpErlangObject number:boundariesOrValues.elements())
	        				valuesValue.add(((OtpErlangLong)number).longValue());
	        		}
	        		else
	    				throw new IllegalArgumentException("Unknown attribute "+obj+" in the list of attributes for IntSignature");
		}
		catch(OtpErlangRangeException ex)
		{
			Helper.throwUnchecked("failed to convert the supplied boundaries to Java integers", ex);
		}
		finally
		{
			lower = lowerValue;upper = upperValue;
			values = valuesValue==null?null:Collections.unmodifiableSet(valuesValue);
		}
		erlangTermForThisType = erlangTypeToString(attributes,boundariesOrValues);
	}
    
    public IntSignature(OtpErlangList attributes)
    {
    	long lowerValue = Long.MIN_VALUE, upperValue = Long.MAX_VALUE;values = null;
        for(OtpErlangObject obj:attributes.elements())
        	if (obj.equals(PositiveAtom))
        	{
        		lowerValue = 1;upperValue = Long.MAX_VALUE;
        	}
        	else
        		if (obj.equals(NegativeAtom))
        		{
        			lowerValue=Long.MIN_VALUE;upperValue=-1;
        		}
        		else
            		if (obj.equals(NonNegativeAtom))
            		{
            			lowerValue=0;upperValue = Long.MAX_VALUE;
            		}
        			else
        				throw new IllegalArgumentException("Unknown attribute "+obj+" in the list of attributes for IntSignature");

        lower=lowerValue;upper=upperValue;
		erlangTermForThisType = erlangTypeToString(attributes, null);
   }
    
	@Override
	public boolean typeCompatible(OtpErlangObject term) 
	{
		long value = 0;
		if (term instanceof OtpErlangInt) value = ((OtpErlangInt)term).longValue();
		else
		if (term instanceof OtpErlangLong) value =  ((OtpErlangLong)term).longValue();
		else
			return false;// wrong type
		if (value < lower || value > upper) return false;
		if (values != null && !values.contains(value)) return false;
		
		return true;
	}

	@Override
	public List<OtpErlangObject> instantiateAllAlts() 
	{
		OtpErlangObject value = null;
	   	if (values == null || values.isEmpty())
	   		value = new OtpErlangLong((upper+lower)/2);
	   	else
	   		value = new OtpErlangLong(values.iterator().next());
	   	
	   	return Collections.singletonList(value);
 	}

}
