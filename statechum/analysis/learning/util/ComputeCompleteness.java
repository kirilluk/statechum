/* Copyright (c) 2006, 2007, 2008, 2009 Neil Walkinshaw and Kirill Bogdanov
 * 
 * This file is part of StateChum.
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
 */

package statechum.analysis.learning.util;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;

import com.sun.xml.internal.bind.v2.schemagen.xmlschema.List;

import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.rpnicore.LearnerGraph;

/**
 * Computes the completeness of the inferred graph in terms of proscribed, prescribed and unknown transitions
 * see FSE2003 paper by Uchitel et al.
 *
 */

public class ComputeCompleteness {
	
	public static void computeCompleteness(LearnerGraph lg){
		Set<StateLabelPair> prescribed = new HashSet<StateLabelPair>();
		Set<StateLabelPair> proscribed = new HashSet<StateLabelPair>();
		Set<StateLabelPair> unknown = new HashSet<StateLabelPair>();
		Set<String> alphabet = lg.pathroutines.computeAlphabet();
		for (CmpVertex v : lg.getTransitionMatrix().keySet()) {
			for (String label : alphabet) {
				StateLabelPair pair = new StateLabelPair(v,label);
				ArrayList seq = new ArrayList();
				seq.add(label);
				CmpVertex dest = lg.getVertex(v, seq);
				if(dest == null)
					unknown.add(pair);
				else if(dest.isAccept())
					prescribed.add(pair);
				else
					proscribed.add(pair);
			}
		}
		System.out.println("Prescribed: "+prescribed.size());
		System.out.println("Proscribed: "+proscribed.size());
		System.out.println("Unknown: "+unknown.size());
		
	}

}
