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

import statechum.Configuration;
import statechum.Helper;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;

/**
 * 
 * @author ramsay
 */
public class ListSignature extends Signature {

	/**
	 * This determines possible elements in the list, any combination of them is
	 * allowed. terminator is the type of the last element in the list.
	 */
	final public Signature elems, terminator;
	final public boolean nonempty, maybeimproper, improper, any;
	final protected int listExpansionLength;

	/** Used to instantiate a list of "any" elements. */
	protected final AtomSignature wibbleListSignature;
	
	public static final OtpErlangAtom NonEmptyAtom = new OtpErlangAtom(
			"nonempty"), ImproperAtom = new OtpErlangAtom("improper"),
			MaybeImproperAtom = new OtpErlangAtom("maybeimproper"),
			AnyAtom = new OtpErlangAtom("any");

	/**
	 * Constructs this signature
	 * 
	 * @param attributes
	 *            if it is non-empty or possibly improper
	 * @param values
	 *            possible values, ignored if attribute any is defined.
	 */
	public ListSignature(Configuration config, OtpErlangList attributes, OtpErlangList values) {
		super();
		boolean nonemptyValue = false, maybeimproperValue = false, improperValue = false, anyValue = false;
		for (OtpErlangObject obj : attributes.elements())
			if (obj.equals(NonEmptyAtom))
				nonemptyValue = true;
			else if (obj.equals(ImproperAtom))
				improperValue = true;
			else if (obj.equals(MaybeImproperAtom))
				maybeimproperValue = true;
			else if (obj.equals(AnyAtom))
				anyValue = true;
			else
				throw new IllegalArgumentException("Unknown attribute " + obj
						+ " in the list of attributes for ListSignature");

		nonempty = nonemptyValue;
		improper = improperValue;
		if (improper)
			maybeimproper = true;
		else
			maybeimproper = maybeimproperValue;
		
		if (values.arity() == 0) {
			elems = null;
			anyValue = true;
			terminator = null;
		} else if (values.arity() == 1) {
			elems = Signature.buildFromType(config, values.elementAt(0));
			terminator = null;
			if (elems instanceof AnySignature)
				anyValue = true;
		} else if (values.arity() == 2) {
			// first value - type of elements,
			// second one - type of terminator
			elems = Signature.buildFromType(config, values.elementAt(0));
			terminator = Signature.buildFromType(config, values.elementAt(1));
		} else
			throw new IllegalArgumentException(
					"More than a two types of elements of a list - a collection of types should be encoded as a ?union");

		any = anyValue;

		if (any)
			listExpansionLength = config.getErlangAlphabetAnyListLength();
		else
			listExpansionLength = config.getErlangAlphabetListLength();
		
		wibbleListSignature = new AtomSignature(config,new OtpErlangList(),
				new OtpErlangList(new OtpErlangObject[] { new OtpErlangAtom("AnyListElemWibble")}));
		
		erlangTermForThisType = erlangTypeToString(attributes, values);
	}

	@Override
	public List<OtpErlangObject> instantiateAllAlts() {
		List<OtpErlangObject> result = new LinkedList<OtpErlangObject>();
		if (!nonempty)
			result.add(new OtpErlangList());// empty list if permitted

		for (int length = 1; length <= listExpansionLength; length++) {
			Signature elemSig = any ? wibbleListSignature : elems;
			List<Signature> s = new ArrayList<Signature>(length);
			for (int i = 0; i < length; ++i)
				s.add(elemSig);
			if (!improper)
				for (List<OtpErlangObject> listOfValues : Signature.computeCrossProduct(s))
					result.add(new OtpErlangList(listOfValues.toArray(new OtpErlangObject[0])));
			if (maybeimproper) {
				Signature termSig = terminator == null ? wibbleListSignature: terminator;
				for (List<OtpErlangObject> listOfValues : Signature.computeCrossProduct(s))

					for (OtpErlangObject term : termSig.instantiateAllAlts())
						if (!checkEmptyList(term)) {
							try {
								result.add(new OtpErlangList(listOfValues
										.toArray(new OtpErlangObject[0]), term));
							} catch (OtpErlangException e) {
								Helper.throwUnchecked("failed to create an improper list", e);
							}
						}

			}
		}
		return result;
	}

	/** This method is used to check a tail of an improper list. 
	 * It returns true if a term can be used to make an improper list, false
	 * otherwise and throws an exception if the supplied term represents a non-empty list. */
	public static boolean checkEmptyList(OtpErlangObject term) {
		boolean outcome = false;
		if (term instanceof OtpErlangList) {
			if (((OtpErlangList) term).arity() > 0)
				throw new IllegalArgumentException(
						"the tail of an improper list cannot be a non-empty list");
			outcome = true;
		} else if (term instanceof OtpErlangString) {
			if (!((OtpErlangString) term).stringValue().isEmpty())
				throw new IllegalArgumentException(
						"the tail of an improper list cannot be a non-empty string (which is a list)");
			outcome = true;
		}

		return outcome;
	}

	@Override
	public boolean typeCompatible(OtpErlangObject term) {
		if (term instanceof OtpErlangList)
			return typeCompatibleInternal(term);

		if (term instanceof OtpErlangString)
			return typeCompatibleInternal(Signature.stringToList(term));

		return false;
	}

	public boolean typeCompatibleInternal(OtpErlangObject term) {
		if (!(term instanceof OtpErlangList))
			return false;
		OtpErlangList list = (OtpErlangList) term;

		if (list.isProper() && improper)
			return false;
		if (!list.isProper() && !maybeimproper && !improper)
			return false;
		if (list.arity() == 0 && nonempty)
			return false;

		if (any)
			return true;

		if (elems != null)
			for (int i = 0; i < list.arity(); ++i) {
				OtpErlangObject listTerm = list.elementAt(i);
				if (!elems.typeCompatible(listTerm))
					return false;
			}

		if (list.arity() == 0)
			return true;
		OtpErlangObject lastElement = list.getLastTail();
		if (lastElement == null && improper)
			return false;
		// where lastElement == null but we are not certain to be improper,
		// cannot really do anything.
		// where lastElement != null, term is improper; if we are not allowed to
		// be improper, we would've returned failure already.
		if (terminator != null && lastElement != null)
			return terminator.typeCompatible(lastElement);

		return true;
	}
}
