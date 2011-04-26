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

import statechum.Helper;
import statechum.Label;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;

/**
 *
 * @author ramsay
 */
public class IntSignature extends Signature {

    public final int lower;
    public final int upper;

	public static final OtpErlangAtom PositiveAtom = new OtpErlangAtom("positive"),
	NegativeAtom = new OtpErlangAtom("negative"),
	NonNegativeAtom = new OtpErlangAtom("nonnegative"),
	AntiStringAtom = new OtpErlangAtom("atom");

    public IntSignature(OtpErlangList attributes,OtpErlangList boundaries) 
    {
		if (attributes.arity() != 0) throw new IllegalArgumentException("IntSignature does not accept attributes when boundaries are also provided");
		if (boundaries.arity() != 3)  throw new IllegalArgumentException("IntSignature requires two boundary values instead of those provided, "+boundaries);

		int lowerValue = 0,upperValue = 0;
		try
		{
			lowerValue = ((OtpErlangLong)boundaries.elementAt(0)).intValue();
			upperValue = ((OtpErlangLong)boundaries.elementAt(1)).intValue();
			if (!AntiStringAtom.equals(boundaries.elementAt(2))) throw new IllegalArgumentException("The third element of list "+boundaries+" should be an atom to stop it from becoming a string");
		}
		catch(OtpErlangRangeException ex)
		{
			Helper.throwUnchecked("failed to convert the supplied boundaries to Java integers", ex);
		}
		finally
		{
			lower = lowerValue;upper = upperValue;
		}
		erlangTermForThisType = erlangTypeToString(attributes,boundaries);
	}
    
    public IntSignature(OtpErlangList attributes)
    {
    	int lowerValue = 0, upperValue = 0;
        for(OtpErlangObject obj:attributes.elements())
        	if (obj.equals(PositiveAtom))
        	{
        		lowerValue = 1;upperValue = Integer.MAX_VALUE;
        	}
        	else
        		if (obj.equals(NegativeAtom))
        		{
        			lowerValue=Integer.MIN_VALUE;upperValue=-1;
        		}
        		else
            		if (obj.equals(NonNegativeAtom))
            		{
            			lowerValue=1;upperValue = Integer.MAX_VALUE;
            		}
        			else
        				throw new IllegalArgumentException("Unknown attribute "+obj+" in the list of attributes for IntSignature");

        if (lowerValue == 0 || upperValue == 0)
        	// unassigned
        	lowerValue = upperValue = 42;
        
        lower=lowerValue;upper=upperValue;
		erlangTermForThisType = erlangTypeToString(attributes, null);
   }
    
    @Override
	public OtpErlangObject instantiate() {
        return new OtpErlangLong((upper-lower)/2);
    }

}
