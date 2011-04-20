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

import com.ericsson.otp.erlang.OtpErlangByte;
import com.ericsson.otp.erlang.OtpErlangList;

/**
 *
 * @author ramsay
 */
public class ByteSignature extends Signature {

	public ByteSignature(OtpErlangList attributes)
	{
		super();
		if (attributes.arity() != 0) throw new IllegalArgumentException("ByteSignature does not accept attributes");
	}
	
    @Override
	public OtpErlangByte instantiate() {
        return new OtpErlangByte((byte)42);
    }
}
