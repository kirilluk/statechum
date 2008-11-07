/* Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
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

package statechum.analysis.learning;

import statechum.DeterministicDirectedSparseGraph.CmpVertex;

/** Important: although compatibility score is recorded and reported, it is ignored in 
 * all computations since it is considered for information only. The getter for it
 * is provided because this class is not exclusively used for learning, but in other
 * cases when I need to associate two scores to states, such as in GD. 
 */
public class PairScore extends StatePair implements Comparable<StatePair>
{
	private final int score, compatibilityScore;

	public PairScore(CmpVertex q, CmpVertex r, int sc, int compat) {
		super(q, r);
		score = sc;compatibilityScore = compat;
	}
	
	public int getScore() {
		return score;
	}

	public int getAnotherScore() {
		return compatibilityScore;
	}
	
	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int PRIME = 31;
		int result = super.hashCode();
		result = PRIME * result + score;
		return result;
	}

	/** This one is used when pairs of states are identified and 
	 * we'd like to sort them in the order of decreasing score, so that 
	 * the first one will have the highest score etc.
	 * Note: this assumes the argument is not null etc; this routing throws up if something is wrong.  
	 * 
	 * @param b the state pair to compare to.
	 */
	@Override
	public int compareTo(StatePair b){
		PairScore pB = (PairScore)b;
		if (score != pB.score)
			return score < pB.score? -1:1;
		return super.compareTo(b);
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!(obj instanceof PairScore))
			return false;
		final PairScore other = (PairScore) obj;
		if (score != other.score)
			return false;
		if (!super.equals(obj))
			return false;
		return true;
	}
	
	@Override
	public String toString(){
		return "[ "+getQ().getID().toString()+"("+getQ().isAccept()+"), "+getR().getID().toString()+"("+getR().isAccept()+") : "+score+","+compatibilityScore+" ]";
	}
}