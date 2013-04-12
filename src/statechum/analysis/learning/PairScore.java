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

import statechum.JUConstants;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.Pair;

/** Important: although compatibility score is recorded and reported, it is ignored in 
 * all computations since it is considered for information only. The getter for it
 * is provided because this class is not exclusively used for learning, but in other
 * cases when I need to associate two scores to states, such as in GD. 
 * @param <B>
 */
public class PairScore extends StatePair
{
	private final long score, compatibilityScore;

	public PairScore(CmpVertex q, CmpVertex r, long sc, long compat) {
		super(q, r);
		score = sc;compatibilityScore = compat;
	}
	
	public long getScore() {
		return score;
	}

	public long getAnotherScore() {
		return compatibilityScore;
	}
	
	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int PRIME = 31;
		int result = super.hashCode();
		result = PRIME * result + (int)score;// doing a truncate here
		return result;
	}

	/** This one is used when pairs of states are identified and 
	 * we'd like to sort them in the order of decreasing score, so that 
	 * the first one will have the highest score etc. Unlike {@link compareTo}, this one
	 * uses depth so that states closer to the root state are given a priority. If used in place of 
	 * {@link compareTo}, some 
	 * of the tests will fail, such as {@link TestRpniLearner#testLearner2a}
	 * that would need additional sequences 
	 * {@code new String[]{"a","b"},new String[]{"b","a","b"}}
	 * as part of negatives to learn correctly.
	 * The existing question generator does not generate any questions for the incorrect merge of state P1002
	 * into P1000, this is the way question generator works which was never aimed to execute an equivalence 
	 * query.
	 * <br>
	 * Note: this assumes the argument is not null etc; this routing throws up if something is wrong.  
	 * <br>
	 * Note the type of argument: it has to be a supertype since we're overriding Pair's <em>compareTo</em> method.
	 * @param b the state pair to compare to.
	 */
	public int compareInTermsOfDepth(Pair<CmpVertex,CmpVertex> b)
	{
		PairScore pB = (PairScore)b;
		if (score != pB.score)
			return score < pB.score? -1:1;

		/*
		// if scores are the same, we give priority to positive pairs.
		if (secondElem.isAccept() != pB.secondElem.isAccept())
			return secondElem.isAccept()?-1:1;
		*/
		
		// Check red state first
		if (secondElem.getDepth() != pB.secondElem.getDepth())
		{
			assert secondElem.getDepth() != JUConstants.intUNKNOWN && pB.secondElem.getDepth() != JUConstants.intUNKNOWN;
			if (secondElem.getDepth() < pB.secondElem.getDepth())
				return 1;
			if (secondElem.getDepth() > pB.secondElem.getDepth())
				return -1;
			return 0;
		}

		// Now blue one
		if (firstElem.getDepth() != pB.firstElem.getDepth())
		{
			assert firstElem.getDepth() != JUConstants.intUNKNOWN && pB.firstElem.getDepth() != JUConstants.intUNKNOWN;
			if (firstElem.getDepth() < pB.firstElem.getDepth())
				return 1;
			if (firstElem.getDepth() > pB.firstElem.getDepth())
				return -1;
			return 0;
		}

		// The comparison routine is modified to ensure that we compare red states first and then blue ones.
		int secondComp = 0;
		
		if (secondElem == null)
		{
			if (pB.secondElem != null)
				secondComp = -1;
		}
		else
		{
			if (pB.secondElem == null)
				secondComp = 1;
			else
				secondComp = secondElem.compareTo(pB.secondElem);
		}
		
		if(secondComp != 0)
			return secondComp;

		int firstComp = 0;
		
		if (firstElem == null)
		{
			if (pB.firstElem != null)
				firstComp = -1;
		}
		else
		{
			if (pB.firstElem == null)
				firstComp = 1;
			else
				firstComp = firstElem.compareTo(pB.firstElem);
		}
		
		return firstComp;
	}
	
	/** This one is used when pairs of states are identified and 
	 * we'd like to sort them in the order of decreasing score, so that 
	 * the first one will have the highest score etc.
	 * Note: this assumes the argument is not null etc; this routing throws up if something is wrong.  
	 * <br>
	 * Note the type of argument: it has to be a supertype since we're overriding Pair's <em>compareTo</em> method.
	 * @param b the state pair to compare to.
	 */
	@Override
	public int compareTo(Pair<CmpVertex,CmpVertex> b)
	{
		PairScore pB = (PairScore)b;
		if (score != pB.score)
			return score < pB.score? -1:1;

		return super.compareTo(pB);
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) 
	{
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
	public String toString()
	{
		return "[ "+getQ().getStringId()+"("+getQ().isAccept()+","+getQ().getDepth()+"), "+getR().getStringId()+"("+getR().isAccept()+","+getR().getDepth()+") : "+score+","+compatibilityScore+" ]";
	}
}