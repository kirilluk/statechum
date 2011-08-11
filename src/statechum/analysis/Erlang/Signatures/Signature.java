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

import java.lang.reflect.Constructor;
import java.util.LinkedList;
import java.util.List;

import statechum.Helper;
import statechum.Label;
import statechum.analysis.Erlang.ErlangLabel;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * Describes the type signature of a function and provides some useful(?)
 * methods for manipulating them.
 * 
 * This is deliberately polymorphic with subclasses to support progressive
 * inference of signatures.
 * 
 * @author ramsay
 */
public abstract class Signature implements Label {
	/**
	 * This method should provide a collection with one possible instantiation
	 * for each available alternate signature in the parse.
	 * 
	 * Most signatures will just use instantiate() but should provide one
	 * instance for each of their possible children's instances.
	 * 
	 * This should be overridden as necessary by subclasses.
	 */
	abstract public List<OtpErlangObject> instantiateAllAlts();

	/**
	 * Verifies that the supplied term is type-compatible to the type
	 * represented by this signature, returns false if not.
	 * 
	 * @param term
	 *            which is supposed to be type-compatible to this signature.
	 */
	abstract public boolean typeCompatible(OtpErlangObject term);

	/** Used to instantiate a list of "any" elements. */
	protected static final AtomSignature wibbleSignature = new AtomSignature(new OtpErlangList(),
			new OtpErlangList(new OtpErlangObject[] { new OtpErlangAtom("Awibble") }));

	/**
	 * Given an Erlang type encoded as an object, constructs an instance of a
	 * corresponding type.
	 */
	public static Signature buildFromType(OtpErlangObject elementAt) {
		Signature result = null;

		try {
			if (elementAt instanceof OtpErlangTuple) {// multi-arg constructor,
														// turn elements of the
														// tuple into arguments
														// for the constructor.
				OtpErlangTuple argTuple = (OtpErlangTuple) elementAt;
				if (argTuple.arity() < 2)
					throw new IllegalArgumentException(
							"invalid type argument: a list with arity of less than two");
				if (argTuple.arity() > 3)
					throw new IllegalArgumentException(
							"invalid type argument: a list with arity of over three");
				final int argumentNumber = argTuple.arity() - 1;
				Class<OtpErlangList>[] argTypes = new Class[argumentNumber];
				for (int i = 0; i < argumentNumber; ++i)
					argTypes[i] = OtpErlangList.class;
				@SuppressWarnings("unchecked")
				Class<Signature> sigClass = (Class<Signature>) Class
						.forName("statechum.analysis.Erlang.Signatures."
								+ ((OtpErlangAtom) argTuple.elementAt(0)).atomValue() + "Signature");
				Constructor<Signature> constructor = sigClass.getConstructor(argTypes);
				Object[] values = new OtpErlangList[argumentNumber];
				for (int i = 0; i < argumentNumber; ++i) {
					OtpErlangObject obj = argTuple.elementAt(i + 1);
					if (obj instanceof OtpErlangString)
						obj = stringToList(obj);
					else if (!(obj instanceof OtpErlangList))
						throw new IllegalArgumentException("got " + obj + " where expected a list");
					values[i] = obj;
				}
				result = constructor.newInstance(values);
			} else
				throw new IllegalArgumentException("invalid type argument " + elementAt
						+ " : it has to be a tuple");
		} catch (IllegalArgumentException ex) {// if one of the constructors or
												// some of the consistency
												// checks failed, re-throw the
												// exception.
			throw ex;
		} catch (Exception ex) {// failed to create instance
			Helper.throwUnchecked("Failed to create a type instance corresponding to " + elementAt, ex);
		}
		return result;
	}

	/**
	 * OTPErlang has an uncanny habit to turn lists into strings if possible,
	 * since this has apparently happened here, convert the string back to list.
	 */
	public static OtpErlangObject stringToList(OtpErlangObject obj) {
		String str = ((OtpErlangString) obj).stringValue();
		OtpErlangObject[] listOfNumbers = new OtpErlangObject[str.length()];
		for (int num = 0; num < str.length(); ++num)
			listOfNumbers[num] = new OtpErlangLong(str.charAt(num));
		return new OtpErlangList(listOfNumbers);
	}

	/**
	 * Otp servers return tuples but we do not get to see them - only middle
	 * elements. This method converts a type for such a response into a
	 * collection of possible values for the middle argument.
	 */
	public static Signature extractElement(Signature someSig, int elemNumber) {
		Signature outcome = null;
		if (someSig instanceof AnySignature || someSig instanceof NoneSignature)
			outcome = someSig;
		else if (someSig instanceof AltSignature) {
			AltSignature altSig = (AltSignature) someSig;
			List<Signature> sigs = new LinkedList<Signature>();
			for (Signature s : altSig.elems) {
				Signature extracted = extractElement(s, elemNumber);
				if (extracted instanceof AltSignature)
					sigs.addAll(((AltSignature) extracted).elems);
				else
					sigs.add(extracted);
			}
			outcome = new AltSignature(sigs);
		} else if (someSig instanceof TupleSignature) {
			TupleSignature tupleSig = (TupleSignature) someSig;
			if (tupleSig.elems.size() < elemNumber + 1)
				throw new IllegalArgumentException("tuple too short, expected to extract element "
						+ elemNumber + " from " + tupleSig);
			outcome = extractElementFromAlt(tupleSig.elems.get(elemNumber));
		} else
			throw new IllegalArgumentException("invalid type " + someSig + " expected a tuple or an alt");

		return outcome;
	}

	/**
	 * A tuple may contain Alts which are arbitrarily deeply nested, extract
	 * them all.
	 */
	private static Signature extractElementFromAlt(Signature someSig) {
		Signature outcome = someSig;
		if (someSig instanceof AltSignature) {
			AltSignature altSig = (AltSignature) someSig;
			List<Signature> sigs = new LinkedList<Signature>();
			for (Signature s : altSig.elems) {
				Signature extracted = extractElementFromAlt(s);
				if (extracted instanceof AltSignature)
					sigs.addAll(((AltSignature) extracted).elems);
				else
					sigs.add(extracted);
			}
			outcome = new AltSignature(sigs);
		}

		return outcome;
	}

	/**
	 * Returns a shortened class name which is the first atom in a type
	 * signature returned by the modified typer.
	 * 
	 * @return reduced class name.
	 */
	public static String getSigName(Object obj) {
		String name = obj.getClass().getName();
		name = name.substring(name.lastIndexOf('.') + 1);
		name = name.substring(0, name.indexOf("Signature"));
		return name;
	}

	/** Used by subclasses of Signature to build string representation. */
	public String erlangTypeToString(OtpErlangList listA, OtpErlangList listB) {
		List<OtpErlangObject> details = new LinkedList<OtpErlangObject>();
		details.add(new OtpErlangAtom(getSigName(this)));
		if (listA != null) {
			details.add(listA);
			if (listB != null)
				details.add(listB);
		} else
			throw new IllegalArgumentException("listA is null but listB is not");

		return ErlangLabel.dumpErlangObject(new OtpErlangTuple(details.toArray(new OtpErlangObject[0])));
	}

	public static List<List<OtpErlangObject>> computeCrossProduct(List<Signature> listOfArgs) {
		LinkedList<List<OtpErlangObject>> res = new LinkedList<List<OtpErlangObject>>();
		if (listOfArgs.isEmpty()) {
			res.add(new LinkedList<OtpErlangObject>());
		} else {// a non-empty list of arguments.
			List<Signature> tail = new LinkedList<Signature>(listOfArgs);
			Signature head = tail.remove(0);
			List<OtpErlangObject> headVals = head.instantiateAllAlts();

			if (!tail.isEmpty()) {
				List<List<OtpErlangObject>> tailVals = computeCrossProduct(tail);
				assert !tailVals.isEmpty();
				for (OtpErlangObject h : headVals) {
					for (List<OtpErlangObject> t : tailVals) {
						List<OtpErlangObject> product = new LinkedList<OtpErlangObject>();
						product.add(h);
						product.addAll(t);
						res.add(product);
					}
				}
			} else
				// tail is empty - a sequence with a single element
				for (OtpErlangObject h : headVals) {
					List<OtpErlangObject> product = new LinkedList<OtpErlangObject>();
					product.add(h);
					res.add(product);
				}
		}
		return res;
	}

	/**
	 * Represents an Erlang term from which details of this signature can be
	 * reconstructed - this is currently only used to compare labels due to
	 * their immutability but can easily be used for serialisation.
	 */
	protected String erlangTermForThisType = null;

	@Override
	public int compareTo(Label o) {
		return toErlangTerm().compareTo(((Signature) o).toErlangTerm());
	}

	@Override
	public String toErlangTerm() {
		return erlangTermForThisType;
	}

	@Override
	public boolean equals(Object o) {
		return toErlangTerm().equals(((Label) o).toErlangTerm());
	}

	@Override
	public int hashCode() {
		return toErlangTerm().hashCode();
	}
}
