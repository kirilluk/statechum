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

import com.ericsson.otp.erlang.*;
import statechum.Configuration;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
/**
 *
 * @author Kirill
 */
public class MapSignature extends Signature {

    protected final List<Signature> mandatoryKeys = new ArrayList<>(),  mandatoryValues = new ArrayList<>(),
            optionalKeys = new ArrayList<>(),optionalValues = new ArrayList<>();
            //holes = new ArrayList<>();// keys that correspond to ?none value types, denoting holes, aka keys that should not be in map.
    protected Signature defaultKey, defaultValue;

    /** A tuple with an arbitrary number of elements of unknown types. */
    public MapSignature(Configuration config, OtpErlangList attributes) {
        super();
		if (attributes.arity() != 0) throw new IllegalArgumentException("MapSignature does not accept attributes");

        defaultKey = new AtomSignature(config,new OtpErlangList(),
                new OtpErlangList(new OtpErlangObject[] { new OtpErlangAtom("Awibble") }));
        defaultValue = new IntSignature(config,new OtpErlangList());
		erlangTermForThisType = erlangTypeToString(attributes,null);
    }
/*
The structure of typesOfElements is this:
{PairMand,PairOpt,DefK,DefV}

PairMand is the list of type-value entries where types are expected to be singletons. Elements falling into these pairs have
to be present in any instance of a map.
PairOpt is the list of type-value entries where types are expected to be singletons. Elements falling into these pairs may be
present or missing however if present values have to match that of the value type specified in PairOpt.
K 'optional' ?none is used to indicate that K should not appear in any considered map. In other words, if I think of a specific
type of keys such as Int, such K are 'holes' that never appear.

DefK is the default type for keys and DefV for values.
Default type for keys should not be singleton because if it is, it means there is just one default value for keys
and it hence should appear explicitly either as mandatory key-value or optional key-value.
default - K for any key should not be representable as a type within Dialyzer because key-value pairs encode specific elements and
all the rest are default elements. Therefore, the 'rest' means anything without explicit entries which
should be constructed at the time when type for map is inferred via
defK = possible_keys - all_used_in_pairs. Hence if defK - K is constructible it means dialyzer did not make a correct type inference.
 */

    /** A tuple with elements of known types. */
    public MapSignature(Configuration config, OtpErlangList attributes, OtpErlangList structureOfMap) {
        super();
        if (attributes.arity() != 0) throw new IllegalArgumentException("MapSignature does not accept attributes");

        if (structureOfMap.arity() != 4) throw new IllegalArgumentException("structureOfMap should have exactly 4 elements");
        OtpErlangList mandatory = (OtpErlangList)  structureOfMap.elementAt(0);
        for(int i=0;i<mandatory.arity();++i) {
            OtpErlangTuple pair = (OtpErlangTuple)mandatory.elementAt(i);
            if (pair.arity() != 2)
                throw new IllegalArgumentException("mandatory pairs should contain just two elements");
            mandatoryKeys.add(Signature.buildFromType(config, pair.elementAt(0)));
            mandatoryValues.add(Signature.buildFromType(config, pair.elementAt(1)));
        }
        // This one adds values for default keys and values.
        defaultKey = Signature.buildFromType(config, structureOfMap.elementAt(2));
        defaultValue = Signature.buildFromType(config, structureOfMap.elementAt(3));

        OtpErlangList optional = (OtpErlangList)  structureOfMap.elementAt(1);
        for(int i=0;i<optional.arity();++i) {
            OtpErlangTuple pair = (OtpErlangTuple)optional.elementAt(i);
            if (pair.arity() != 2)
                throw new IllegalArgumentException("optional pairs should contain just two elements");

            Signature values = Signature.buildFromType(config, pair.elementAt(1));
            //if (values instanceof NoneSignature)
            //    holes.add(Signature.buildFromType(config, pair.elementAt(0)));// this is a hole
            //else {
            optionalKeys.add(Signature.buildFromType(config, pair.elementAt(0)));
            optionalValues.add(values);
        }
		erlangTermForThisType = erlangTypeToString(attributes,structureOfMap);
   }

    @Override
    public List<OtpErlangObject> instantiateAllAlts() {
        List<OtpErlangObject> result = new LinkedList<OtpErlangObject>();
        List<Signature> mandatoryKeyValues = new ArrayList<>();
        for(int i=0;i<mandatoryKeys.size();++i) {
            mandatoryKeyValues.add(mandatoryKeys.get(i));mandatoryKeyValues.add(mandatoryValues.get(i));
        }
        if (defaultKey.getClass() != NoneSignature.class && defaultValue.getClass() != NoneSignature.class) {
            mandatoryKeyValues.add(defaultKey);
            mandatoryKeyValues.add(defaultValue);
        }
        List<Signature> optionalKeyValues = new ArrayList<>();
        for(int i=0;i<optionalKeys.size();++i) {
            optionalKeyValues.add(optionalKeys.get(i));optionalKeyValues.add(optionalValues.get(i));
        }
        List<List<OtpErlangObject>>
                mandatoryPairs = Signature.computeCrossProduct(mandatoryKeyValues),
                optionalPairs = Signature.computeCrossProduct(optionalKeyValues);
        // We do not really know how to distribute optional pairs among maps with mandatory pairs.
        // It is not even clear how many pairs to include at all so we just try something and
        // do domain-specific setup when needed. The approach is to use optional pairs on the first
        // element in mandatoryPairs since it might be the first one with all others falling into holes.
        boolean firstEntry = true;
        for(List<OtpErlangObject> listOfValues:mandatoryPairs) {
            // this for loop should iterate for mandatory key-values however we also include default key-value pairs in it
            // thus it should be able to construct a few values
            List<OtpErlangObject> keys = new ArrayList<>(), values = new ArrayList<>();
            boolean keyInHole = false;// if we've generated a key value that should not be present, drop the current value.
            // In practical terms, we are not going to drop much because holes have singleton types
            // by construction (in dialyzer) and there is hopefully not going to be a lot of them.
            assert listOfValues.size() % 2 == 0;// must be even for keys-values
            for(int i=0;i<listOfValues.size() && !keyInHole;i+=2) {
                // First, remove holes
                for(int opt=0;opt<optionalKeys.size() && !keyInHole;++opt)
                    if (optionalKeys.get(opt).typeCompatible(listOfValues.get(i)) &&
                            !optionalValues.get(opt).typeCompatible(listOfValues.get(i+1)))
                        keyInHole = true;

                /*
                for(int h=0;h<holes.size() && !keyInHole;++h)
                    if (holes.get(h).typeCompatible(listOfValues.get(i)))
                        keyInHole = true;

                 */
                keys.add(listOfValues.get(i));
                values.add(listOfValues.get(i + 1));
            }

            if (!keyInHole) {
                if (firstEntry) {
                    // now we need to handle optional key-pairs.
                    for(List<OtpErlangObject> listOfOptionalValues:optionalPairs) {
                        List<OtpErlangObject> keysToAdd = new ArrayList<>(keys), valuesToAdd = new ArrayList<>(values);
                        assert listOfOptionalValues.size() % 2 == 0;// must be even for keys-values
                        for (int opt = 0; opt< listOfOptionalValues.size();opt+=2) {
                            keysToAdd.add(listOfOptionalValues.get(opt));
                            valuesToAdd.add(listOfOptionalValues.get(opt + 1));
                        }
                        result.add(new OtpErlangMap(keysToAdd.toArray(new OtpErlangObject[0]), valuesToAdd.toArray(new OtpErlangObject[0])));
                    }
                    firstEntry = false;
                }

                result.add(new OtpErlangMap(keys.toArray(new OtpErlangObject[0]), values.toArray(new OtpErlangObject[0])));
            }
        }
        return result;
    }

    @Override
	public boolean typeCompatible(OtpErlangObject term) 
	{
		if (!(term instanceof OtpErlangMap)) return false;
        OtpErlangMap map = (OtpErlangMap)term;

        // now we need to validate the map
        for(Map.Entry<OtpErlangObject,OtpErlangObject> entry:map.entrySet()){
            boolean compatible = false;
            for(int i=0;i<mandatoryKeys.size() && !compatible;++i) {
                if (mandatoryKeys.get(i).typeCompatible(entry.getKey()) &&
                        mandatoryValues.get(i).typeCompatible(entry.getValue()))
                    compatible = true;
            }

            for(int i=0;i<optionalKeys.size() && !compatible;++i) {
                if (optionalKeys.get(i).typeCompatible(entry.getKey())) {
                    if (!optionalValues.get(i).typeCompatible(entry.getValue()))
                        return false;// special case for optional pairs - includes holes into consideration.
                    compatible = true;// optional key is compatible
                }
            }

            if (!compatible && defaultKey.typeCompatible(entry.getKey()) && defaultValue.typeCompatible(entry.getValue()))
                compatible = true;

            if (!compatible)
                return false;
        }
        // if we are here, key-value entries in the map are compatible with the type, return true.
		return true;
	}
}
