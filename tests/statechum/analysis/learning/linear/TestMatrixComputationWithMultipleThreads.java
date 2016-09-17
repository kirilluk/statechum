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

package statechum.analysis.learning.linear;

import static statechum.analysis.learning.rpnicore.FsmParser.buildLearnerGraph;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Map.Entry;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.ParameterizedWithName;
import org.junit.runners.Parameterized.Parameters;
import org.junit.runners.ParameterizedWithName.ParametersToString;

import statechum.Configuration;
import statechum.Configuration.GDScoreComputationAlgorithmEnum;
import statechum.Configuration.GDScoreComputationEnum;
import statechum.Configuration.STATETREE;
import statechum.Label;
import statechum.Pair;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph.StatesToConsider;
import statechum.analysis.learning.rpnicore.EquivalenceClass;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import statechum.analysis.learning.rpnicore.LSolver;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphCachedData;
import statechum.analysis.learning.rpnicore.LearnerGraphND;
import statechum.analysis.learning.rpnicore.PairScoreComputation;
import statechum.analysis.learning.rpnicore.TestLearnerGraphND;
import statechum.analysis.learning.rpnicore.Transform;
import statechum.analysis.learning.linear.GDLearnerGraph.DDRH_BCR;
import statechum.analysis.learning.linear.GDLearnerGraph.DetermineDiagonalAndRightHandSide;
import statechum.analysis.learning.linear.GDLearnerGraph.StateBasedRandom;
import cern.colt.matrix.DoubleFactory1D;
import cern.colt.matrix.DoubleFactory2D;
import cern.colt.matrix.DoubleMatrix1D;
import cern.colt.matrix.DoubleMatrix2D;
import static statechum.analysis.learning.linear.GDLearnerGraph.PAIR_INCOMPATIBLE;
import static statechum.analysis.learning.linear.GDLearnerGraph.PAIR_OK;

@RunWith(ParameterizedWithName.class)
public class TestMatrixComputationWithMultipleThreads {
	final int threadNumber;
	/** Label converter to use. */
	private final ConvertALabel converter;

	final double k=Configuration.getDefaultConfiguration().getAttenuationK();
	
	@Parameters
	public static Collection<Object[]> data() 
	{
		Collection<Object []> result = new LinkedList<Object []>();
		for(int i=1;i<8;++i)
		{
			result.add(new Object[]{new Integer(i),false});
			result.add(new Object[]{new Integer(i),true});
		}
		return result;
	}
	
	@ParametersToString
	public static String parametersToString(Integer threads, Boolean useArrays)
	{
		return ""+threads+" threads, arrays="+useArrays;
	}
	
	/** Creates the test class with the number of threads to create as an argument. */
	public TestMatrixComputationWithMultipleThreads(int th, boolean useArrays)
	{
		threadNumber = th;
		if (useArrays)
		{
			config = Configuration.getDefaultConfiguration().copy();config.setTransitionMatrixImplType(STATETREE.STATETREE_ARRAY);
			converter = new Transform.InternStringLabel();
		}
		else
		{
			config = Configuration.getDefaultConfiguration().copy();
			converter = null;
		}
	}
	
	private Configuration config = null;

	/* commented out because Array should work in a similar way to a slow tree
	@Before
	public void reassignConfiguration()
	{
		config = Configuration.getDefaultConfiguration().copy();config.setTransitionMatrixImplType(STATETREE.STATETREE_SLOWTREE);
	}
	*/
	
	protected DoubleMatrix1D getExpectedMatrix1DSlowly(LearnerGraph gr)
	{
		int size=gr.getStateNumber()*(gr.getStateNumber()+1)/2;
		DoubleMatrix1D result = DoubleFactory1D.dense.make(size);
		StatesToConsider filter = LearnerGraphND.ignoreRejectStates;
		GDLearnerGraph ndGraph = new GDLearnerGraph(gr,filter, false);
		DetermineDiagonalAndRightHandSide ddrhInstance = ndGraph.new DDRH_default();
		for(Entry<CmpVertex,Map<Label,List<CmpVertex>>> entryA:ndGraph.matrixForward.transitionMatrix.entrySet())
		{
			// Now iterate through states
			Iterator<Entry<CmpVertex,Map<Label,List<CmpVertex>>>> stateB_It = ndGraph.matrixForward.transitionMatrix.entrySet().iterator();
			while(stateB_It.hasNext())
			{
				Entry<CmpVertex,Map<Label,List<CmpVertex>>> stateB = stateB_It.next();

				int currentStatePair = ndGraph.vertexToIntNR(stateB.getKey(),entryA.getKey());
				ddrhInstance.compute(entryA.getKey(),stateB.getKey(),entryA.getValue(),stateB.getValue());
				result.setQuick(currentStatePair, ddrhInstance.getRightHandSide());
				
				if (stateB.getKey().equals(entryA.getKey())) break; // we only process a triangular subset.
			}
		}
		
		return result;
	}

	protected DoubleMatrix2D getExpectedMatrix2DSlowly(LearnerGraph gr)
	{
		int size=gr.getStateNumber()*(gr.getStateNumber()+1)/2;
		DoubleMatrix2D result = DoubleFactory2D.sparse.make(size,size,0);
		StatesToConsider filter = LearnerGraphND.ignoreRejectStates;
		GDLearnerGraph ndGraph = new GDLearnerGraph(gr,filter, false);
		for(Entry<CmpVertex,Map<Label,List<CmpVertex>>> entryA:ndGraph.matrixForward.transitionMatrix.entrySet())
		{
			// Now iterate through states
			Iterator<Entry<CmpVertex,Map<Label,List<CmpVertex>>>> stateB_It = ndGraph.matrixForward.transitionMatrix.entrySet().iterator();
			while(stateB_It.hasNext())
			{
				Entry<CmpVertex,Map<Label,List<CmpVertex>>> stateB = stateB_It.next();

				int currentStatePair = ndGraph.vertexToIntNR(entryA.getKey(),stateB.getKey());
				
				int totalOutgoing =0;
				
				for(Entry<Label,List<CmpVertex>> targetsA:entryA.getValue().entrySet())
				{
					List<CmpVertex> toB_list = stateB.getValue().get(targetsA.getKey());
					if (toB_list != null)
						for(CmpVertex toA:targetsA.getValue())
							for(CmpVertex toB:toB_list)
							{
								++totalOutgoing;
								int targetStatePair = ndGraph.vertexToIntNR(toA, toB);
								result.setQuick(currentStatePair,targetStatePair,
										result.getQuick(currentStatePair, targetStatePair)-gr.config.getAttenuationK());
							}
					else
						totalOutgoing+=targetsA.getValue().size();// add the number of possible target states to the number of outgoing transitions
				}
				for(Entry<Label,List<CmpVertex>> targetsB:stateB.getValue().entrySet())
					if (!entryA.getValue().containsKey(targetsB.getKey()))
							totalOutgoing+=targetsB.getValue().size();// add the number of possible target states to the number of outgoing transitions
				totalOutgoing*=2;// doubling the diagonal to normalise the result of solving
				if (totalOutgoing == 0) totalOutgoing = 1;// if neither element of a pair of states has an outgoing transition, force the identity to ensure that the solution will be zero. 
				result.setQuick(currentStatePair,currentStatePair,result.getQuick(currentStatePair, currentStatePair)+totalOutgoing);
				
				if (stateB.getKey().equals(entryA.getKey())) break; // we only process a triangular subset.
			}
		}
		
		return result;
	}
	
	
	/** Tests matrix construction for a supplied graph and matrix builder. 
	 * Since it messes up the configuration of the graph, it has to be run at the end of every test method rather than multiple times. 
	 */
	protected void checkBuildMatrix(LearnerGraph gr, DoubleMatrix2D expectedAx, DoubleMatrix1D expectedB)
	{
		GDLearnerGraph ndGraph = new GDLearnerGraph(gr,LearnerGraphND.ignoreRejectStates, false);
		LSolver solver = ndGraph.buildMatrix(threadNumber);
		DoubleMatrix2D Ax=solver.toDoubleMatrix2D();
		Assert.assertEquals(getExpectedMatrix2DSlowly(gr),Ax);
		if (expectedAx != null) Assert.assertEquals(expectedAx, Ax);
		DoubleMatrix1D b=solver.toDoubleMatrix1D();
		if (expectedB != null) Assert.assertEquals(expectedB, b);Assert.assertEquals(getExpectedMatrix1DSlowly(gr),b);
		solver.solveExternally(1);// check if we have a solution, just in case it fails.

		// Now check consistency.
		gr.config.setAttenuationK_testOnly(1);DoubleMatrix2D Ax1 = ndGraph.buildMatrix(threadNumber).toDoubleMatrix2D();
		gr.config.setAttenuationK(0);DoubleMatrix2D Ax0 = ndGraph.buildMatrix(threadNumber).toDoubleMatrix2D();
		DoubleMatrix1D one = DoubleFactory1D.dense.make(Ax1.rows(), 1), a=DoubleFactory1D.dense.make(Ax.rows(), 0);
		
		// check A(1)*one >= 0
		Ax1.zMult(one, a);for(int i=0;i<a.size();++i) Assert.assertTrue(a.getQuick(i)>=0);
		
		// check (A(1)-A(0))*one = b
		Ax1.assign(Ax0, cern.jet.math.Functions.minus);
		Ax1.zMult(one, a);for(int i=0;i<a.size();++i) Assert.assertTrue(a.getQuick(i) == -b.getQuick(i));
		
		// Finally, we check that neither states are incompatible (there are no reject states so there should not be any)
		int pairNumber [] = new int[gr.getStateNumber()*(gr.getStateNumber()+1)/2];
		ndGraph.findIncompatiblePairs(pairNumber, threadNumber);GDLearnerGraph.numberNonNegativeElements(pairNumber);
		for(int i=0;i<pairNumber.length;++i) pairNumber[i]=i;
	}
	
	@Test
	public final void testBuildMatrix0()
	{
		LearnerGraph gr=new LearnerGraph(config);
		checkBuildMatrix(gr, DoubleFactory2D.dense.make(new double[][]{new double[]{1}}), DoubleFactory1D.dense.make(new double[]{0}));
	}

	@Test
	public final void testBuildMatrix1()
	{
		LearnerGraph gr=buildLearnerGraph("A-a->B-a->B-b->A",	"testBuildMatrix1",config,converter);
		checkBuildMatrix(gr, DoubleFactory2D.dense.make(new double[][]{
				new double[]{1*2,0,-k},
				new double[]{0,2*2,-k},
				new double[]{-k,0,2*2-k}
				}), DoubleFactory1D.dense.make(new double[]{1,1,2}));
	}
	
	@Test
	public final void testBuildMatrix2()
	{
		LearnerGraph gr=buildLearnerGraph("A-a->A-b->B",	"testBuildMatrix2",config,converter);
		checkBuildMatrix(gr, null,null);
		/*DoubleFactory2D.dense.make(new double[][]{
				new double[]{1,0,-k},
				new double[]{0,2,-k},
				new double[]{-k,0,2-k}
				}), DoubleFactory1D.dense.make(new double[]{1,2,1}));*/
	}

	/** "Identity" sets diagonal to 2 because that's what we do 
	 * in order to normalise the result of linear solution. 
	 */
	private static DoubleMatrix2D createIdentity(int size)
	{
		DoubleMatrix2D matrix=DoubleFactory2D.sparse.make(size, size);
		for(int i=0;i<size;++i) matrix.setQuick(i, i, 2);
		return matrix;
	}
	
	@Test
	public final void testBuildMatrix3()
	{
		LearnerGraph gr=buildLearnerGraph("A-a->B\nC-a->D",	"testBuildMatrix3",config,converter);
		final int size =4*5/2; 
		DoubleMatrix2D matrix=createIdentity(size);
		DoubleMatrix1D row=DoubleFactory1D.dense.make(size, 0);
		matrix.setQuick(2, 2, 1);matrix.setQuick(7, 7, 1);matrix.setQuick(9, 9, 1);// pairs with zero total outgoing transitions.
		matrix.setQuick(0, 2, -k);row.setQuick(0, 1);
		matrix.setQuick(5, 9, -k);row.setQuick(5, 1);
		matrix.setQuick(3, 7, -k);row.setQuick(3, 1);
		
		checkBuildMatrix(gr,matrix,row);
	}

	@Test
	public final void testBuildMatrix4()
	{
		LearnerGraph gr=buildLearnerGraph("A-a->B\nA-b->C\nD-a->C",	"testBuildMatrix4",config,converter);
		final int size =4*5/2; 
		DoubleMatrix2D matrix=createIdentity(size);
		DoubleMatrix1D row=DoubleFactory1D.dense.make(size, 0);
		matrix.setQuick(2, 2, 1);matrix.setQuick(4, 4, 1);matrix.setQuick(5, 5, 1);// pairs with zero total outgoing transitions.
		matrix.setQuick(0, 0, 2*2);matrix.setQuick(0, 2, -k);matrix.setQuick(0, 5, -k);row.setQuick(0, 2);
		matrix.setQuick(1, 1, 2*2);
		matrix.setQuick(3, 3, 2*2);
		matrix.setQuick(6, 6, 2*2);
	
		matrix.setQuick(9, 5, -k);row.setQuick(9, 1);
		matrix.setQuick(6, 4, -k);row.setQuick(6, 1);
				
		checkBuildMatrix(gr,matrix,row);
	}
	
	@Test
	public final void testBuildMatrix5()
	{
		LearnerGraph gr=buildLearnerGraph("A-a->B\nA-b->C\nD-a->C\nD-b->C","testBuildMatrix5",config,converter);
		final int size =4*5/2; 
		DoubleMatrix2D matrix=createIdentity(size);
		DoubleMatrix1D row=DoubleFactory1D.dense.make(size, 0);
		matrix.setQuick(2, 2, 1);matrix.setQuick(4, 4, 1);matrix.setQuick(5, 5, 1);// pairs with zero total outgoing transitions.
		matrix.setQuick(0, 0, 2*2);matrix.setQuick(0, 2, -k);matrix.setQuick(0, 5, -k);row.setQuick(0, 2);
		matrix.setQuick(1, 1, 2*2);
		matrix.setQuick(3, 3, 2*2);
		matrix.setQuick(6, 6, 2*2);
	
		matrix.setQuick(9, 5, -2*k);row.setQuick(9, 2);
		matrix.setQuick(6, 4, -k);matrix.setQuick(6, 5, -k);row.setQuick(6, 2);
		matrix.setQuick(9, 9, 2*2);
		matrix.setQuick(8, 8, 2*2);
		matrix.setQuick(7, 7, 2*2);
		matrix.setQuick(6, 6, 2*2);
		
		checkBuildMatrix(gr,matrix,row);
	}

	@Test
	public final void testBuildMatrix6()
	{
		LearnerGraph gr=buildLearnerGraph("A-a->B\nA-b->C\nD-a->C\nD-b->C\nD-c->A","testBuildMatrix6",config,converter);
		final int size =4*5/2; 
		DoubleMatrix2D matrix=createIdentity(size);
		DoubleMatrix1D row=DoubleFactory1D.dense.make(size, 0);
		matrix.setQuick(2, 2, 1);matrix.setQuick(4, 4, 1);matrix.setQuick(5, 5, 1);// pairs with zero total outgoing transitions.
		matrix.setQuick(0, 0, 2*2);matrix.setQuick(0, 2, -k);matrix.setQuick(0, 5, -k);row.setQuick(0, 2);
		matrix.setQuick(1, 1, 2*2);
		matrix.setQuick(3, 3, 2*2);
		matrix.setQuick(6, 6, 2*2);
	
		matrix.setQuick(9, 5, -2*k);matrix.setQuick(9, 0, -k);row.setQuick(9, 3);
		matrix.setQuick(6, 4, -k);matrix.setQuick(6, 5, -k);row.setQuick(6, 2);
		matrix.setQuick(9, 9, 3*2);
		matrix.setQuick(8, 8, 3*2);
		matrix.setQuick(7, 7, 3*2);
		matrix.setQuick(6, 6, 3*2);
		
		checkBuildMatrix(gr,matrix,row);
	}

	@Test
	public final void testBuildMatrix7()
	{
		LearnerGraph gr=buildLearnerGraph("A-a->B\nA-b->C\nA-c->C\nD-a->C\nD-b->C\nD-c->A","testBuildMatrix7",config,converter);

		final int size =4*5/2; 
		DoubleMatrix2D matrix=createIdentity(size);
		DoubleMatrix1D row=DoubleFactory1D.dense.make(size, 0);
		matrix.setQuick(2, 2, 1);matrix.setQuick(4, 4, 1);matrix.setQuick(5, 5, 1);// pairs with zero total outgoing transitions.
		matrix.setQuick(0, 0, 3*2);matrix.setQuick(0, 2, -k);matrix.setQuick(0, 5, -2*k);row.setQuick(0, 3);
		matrix.setQuick(1, 1, 3*2);
		matrix.setQuick(3, 3, 3*2);
		matrix.setQuick(6, 6, 3*2);
	
		matrix.setQuick(9, 5, -2*k);matrix.setQuick(9, 0, -k);row.setQuick(9, 3);
		matrix.setQuick(6, 4, -k);matrix.setQuick(6, 5, -k);matrix.setQuick(6, 3, -k);row.setQuick(6, 3);
		matrix.setQuick(9, 9, 3*2);
		matrix.setQuick(8, 8, 3*2);
		matrix.setQuick(7, 7, 3*2);
		matrix.setQuick(6, 6, 3*2);
		
		checkBuildMatrix(gr,matrix,row);
	}
	
	@Test
	public final void testBuildMatrix8()
	{
		LearnerGraph gr=buildLearnerGraph("A-a->B\nA-b->C\nA-c->C\nD-a->C\nD-b->C\nD-d->C\nD-c->A","testBuildMatrix8",config,converter);

		final int size =4*5/2; 
		DoubleMatrix2D matrix=createIdentity(size);
		DoubleMatrix1D row=DoubleFactory1D.dense.make(size, 0);
		matrix.setQuick(2, 2, 1);matrix.setQuick(4, 4, 1);matrix.setQuick(5, 5, 1);// pairs with zero total outgoing transitions.
		matrix.setQuick(0, 0, 3*2);matrix.setQuick(0, 2, -k);matrix.setQuick(0, 5, -2*k);row.setQuick(0, 3);
		matrix.setQuick(1, 1, 3*2);
		matrix.setQuick(3, 3, 3*2);
		matrix.setQuick(6, 6, 3*2);
	
		matrix.setQuick(9, 5, -3*k);matrix.setQuick(9, 0, -k);row.setQuick(9, 4);
		matrix.setQuick(6, 4, -k);matrix.setQuick(6, 5, -k);matrix.setQuick(6, 3, -k);row.setQuick(6, 3);
		matrix.setQuick(9, 9, 4*2);
		matrix.setQuick(8, 8, 4*2);
		matrix.setQuick(7, 7, 4*2);
		matrix.setQuick(6, 6, 4*2);
		
		checkBuildMatrix(gr,matrix,row);
	}
		
	@Test
	public final void testBuildMatrix9()
	{
		LearnerGraph gr=buildLearnerGraph("A-a->B-a->B-b->A","testAddToBuffer9",config,converter);
		GDLearnerGraph ndGraph = new GDLearnerGraph(gr,LearnerGraphND.ignoreRejectStates, false);
		ndGraph.buildMatrix(threadNumber);
/*		Collection<SMTLabel> expected = new HashSet<SMTLabel>();expected.addAll(asList(new String[] {
			"mat(1,1)=1.0;","mat(1,3)=-"+k+";",// AA
			"mat(3,3)="+(2.0-k)+";","mat(3,1)=-"+k+";", // BB
			"mat(2,2)="+2.0+";","mat(2,3)=-"+k+";"}));// AB
*/
		checkBuildMatrix(gr,null,null);
	}

	@Test
	public final void testBuildMatrix10()
	{
		LearnerGraph gr=buildLearnerGraph("A-a->Q\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C\nD-c->A","testBuildMatrix10",config,converter);
		checkBuildMatrix(gr,null,null);
	}

	@Test
	public final void testBuildMatrix11()
	{
		LearnerGraph gr=buildLearnerGraph("A-a->Q\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C-a->C\nD-c->A","testBuildMatrix11",config,converter);
		checkBuildMatrix(gr,null,null);
	}

	@Test
	public final void testBuildMatrix12()
	{
		LearnerGraph gr=buildLearnerGraph("A-a->Q\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C-a->C\nD-c->A-c->R","testBuildMatrix12",config,converter);
		checkBuildMatrix(gr,null,null);
	}

	@Test
	public final void testBuildMatrix13()
	{
		LearnerGraph gr=buildLearnerGraph("A-a->Q\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C-a->C\nD-c->A-c->R-a->F","testBuildMatrix13",config,converter);
		checkBuildMatrix(gr,null,null);
	}

	/** No translation of vertices. */
	@Test
	public final void testDumpEquations1()
	{
		GDLearnerGraph ndGraph = new GDLearnerGraph(buildLearnerGraph("A-a->B-a->B-b->A",	"testBuildMatrix1",config,converter),LearnerGraphND.ignoreRejectStates, false);
		final int [] incompatiblePairs = new int[ndGraph.getPairNumber()];for(int i=0;i<incompatiblePairs.length;++i) incompatiblePairs[i]=PAIR_OK;
		final int pairsNumber = ndGraph.findIncompatiblePairs(incompatiblePairs,threadNumber);
		LSolver solver = ndGraph.buildMatrix_internal(incompatiblePairs, pairsNumber, threadNumber,null);

		String outcome = ndGraph.dumpEquations(solver, incompatiblePairs, null).toString();
		Assert.assertEquals("2.0([A,A]:[A,A]) + -"+k+"([A,A]:[B,B]) = 1.0\n"+
				"4.0([B,A]:[B,A]) + -"+k+"([B,A]:[B,B]) = 1.0\n"+
				"-"+k+"([B,B]:[A,A]) + "+(4-k)+"([B,B]:[B,B]) = 2.0\n",outcome);
	}
	
	/** Incomplete translation of vertices. */
	@Test
	public final void testDumpEquations2()
	{
		LearnerGraph graph = buildLearnerGraph("A-a->B-a->B-b->A",	"testBuildMatrix1",config,converter);
		GDLearnerGraph ndGraph = new GDLearnerGraph(graph,LearnerGraphND.ignoreRejectStates, false);
		final int [] incompatiblePairs = new int[ndGraph.getPairNumber()];for(int i=0;i<incompatiblePairs.length;++i) incompatiblePairs[i]=PAIR_OK;
		final int pairsNumber = ndGraph.findIncompatiblePairs(incompatiblePairs,threadNumber);
		LSolver solver = ndGraph.buildMatrix_internal(incompatiblePairs, pairsNumber, threadNumber,null);
		Map<CmpVertex,CmpVertex> newToOrig = new TreeMap<CmpVertex,CmpVertex>();
		String P67="P67",P99="B";
		newToOrig.put(graph.findVertex("A"),AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID(P67), graph.config));
		String outcome = ndGraph.dumpEquations(solver, incompatiblePairs, newToOrig).toString();
		Assert.assertEquals("2.0(["+P67+","+P67+"]:["+P67+","+P67+"]) + -"+k+"(["+P67+","+P67+"]:["+P99+","+P99+"]) = 1.0\n"+
				"4.0(["+P99+","+P67+"]:["+P99+","+P67+"]) + -"+k+"(["+P99+","+P67+"]:["+P99+","+P99+"]) = 1.0\n"+
				"-"+k+"(["+P99+","+P99+"]:["+P67+","+P67+"]) + "+(4-k)+"(["+P99+","+P99+"]:["+P99+","+P99+"]) = 2.0\n",outcome);
	}
	
	/** Map from all original to all new vertices. */
	@Test
	public final void testDumpEquations3()
	{
		LearnerGraph graph = buildLearnerGraph("A-a->B-a->B-b->A",	"testBuildMatrix1",config,converter);
		GDLearnerGraph ndGraph = new GDLearnerGraph(graph,LearnerGraphND.ignoreRejectStates, false);
		final int [] incompatiblePairs = new int[ndGraph.getPairNumber()];for(int i=0;i<incompatiblePairs.length;++i) incompatiblePairs[i]=PAIR_OK;
		final int pairsNumber = ndGraph.findIncompatiblePairs(incompatiblePairs,threadNumber);
		LSolver solver = ndGraph.buildMatrix_internal(incompatiblePairs, pairsNumber, threadNumber,null);
		Map<CmpVertex,CmpVertex> newToOrig = new TreeMap<CmpVertex,CmpVertex>();
		String P67="P67",P99="P99";
		newToOrig.put(graph.findVertex("A"),AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID(P67), graph.config));
		newToOrig.put(graph.findVertex("B"),AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID(P99), graph.config));
		String outcome = ndGraph.dumpEquations(solver, incompatiblePairs, newToOrig).toString();
		Assert.assertEquals("2.0(["+P67+","+P67+"]:["+P67+","+P67+"]) + -"+k+"(["+P67+","+P67+"]:["+P99+","+P99+"]) = 1.0\n"+
				"4.0(["+P99+","+P67+"]:["+P99+","+P67+"]) + -"+k+"(["+P99+","+P67+"]:["+P99+","+P99+"]) = 1.0\n"+
				"-"+k+"(["+P99+","+P99+"]:["+P67+","+P67+"]) + "+(4-k)+"(["+P99+","+P99+"]:["+P99+","+P99+"]) = 2.0\n",outcome);
	}
	
	@Test(expected=IllegalArgumentException.class)
	public final void findIncompatiblePairs_fail()
	{
		LearnerGraph gr=buildLearnerGraph("A-a->Q\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C-a->C\nD-c->A-c->R-a->F","findIncompatiblePairs_fail",config,converter);
		GDLearnerGraph ndGraph = new GDLearnerGraph(gr,LearnerGraphND.ignoreRejectStates, false);
		ndGraph.findIncompatiblePairs(new int[]{},threadNumber);
	}

	/** Tests that if all pairs are not compatible, this is preserved. */
	@Test
	public final void TestFindIncompatibleStatesA()
	{
		LearnerGraph gr=buildLearnerGraph("A-a->Q\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C-a->C\nD-c->A-c->R-a->F","TestFindIncompatibleStatesA",config,converter);
		GDLearnerGraph ndGraph = new GDLearnerGraph(gr,LearnerGraphND.ignoreRejectStates, false);
		final int size=ndGraph.getPairNumber();
		int pairs[]=new int[size];for(int i=0;i<pairs.length;++i) pairs[i]=PAIR_INCOMPATIBLE;
		ndGraph.findIncompatiblePairs(pairs,threadNumber);
		for(int i=0;i<pairs.length;++i) Assert.assertEquals(PAIR_INCOMPATIBLE,pairs[i]);
	}

	/** Tests that if all pairs are not compatible, this is preserved. */
	@Test
	public final void TestFindIncompatibleStatesB()
	{
		LearnerGraph gr=buildLearnerGraph("A-a->Q\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C-a->C\nD-c->A-c-#R","TestFindIncompatibleStatesB",config,converter);
		GDLearnerGraph ndGraph = new GDLearnerGraph(gr,LearnerGraphND.ignoreRejectStates, false);
		final int size=ndGraph.getPairNumber();
		int pairs[]=new int[size];for(int i=0;i<pairs.length;++i) pairs[i]=PAIR_INCOMPATIBLE;
		ndGraph.findIncompatiblePairs(pairs,threadNumber);
		
		for(int i=0;i<pairs.length;++i)
			Assert.assertEquals(PAIR_INCOMPATIBLE,pairs[i]);
	}

	protected Map<Integer,StatePair> reverseMap = null;
	
	private class StringPair extends Pair<String,String>{
		public StringPair(String a, String b) {
			super(a, b);
		}}
	
	/** Uses computePairCompatibilityScore_general to identify incompatible pairs of states. */
	protected static Set<StatePair> buildSetOfIncompatiblePairsSlowly(final LearnerGraph gr,int ThreadNumber)
	{
		final Set<StatePair> result = Collections.synchronizedSet(new LinkedHashSet<StatePair>());
		final StatesToConsider filter = LearnerGraphND.ignoreRejectStates;
		final GDLearnerGraph ndGraph = new GDLearnerGraph(gr,filter, false);
		List<GDLearnerGraph.HandleRow<List<CmpVertex>>> handlerList = new LinkedList<GDLearnerGraph.HandleRow<List<CmpVertex>>>();
		for(int threadCnt=0;threadCnt<ThreadNumber;++threadCnt)
		handlerList.add(new GDLearnerGraph.HandleRow<List<CmpVertex>>()
		{
			@Override
			public void init(@SuppressWarnings("unused") int threadNo)
			{
				// No per-thread initialisation is needed.
			}
			
			@Override
			public void handleEntry(Entry<CmpVertex, Map<Label, List<CmpVertex>>> entryA,@SuppressWarnings("unused")  int threadNo) 
			{
				Assert.assertTrue(LearnerGraphND.ignoreRejectStates.stateToConsider(entryA.getKey()));
					
				// Now iterate through states
				Iterator<Entry<CmpVertex,Map<Label,List<CmpVertex>>>> stateB_It = ndGraph.matrixForward.transitionMatrix.entrySet().iterator();
				while(stateB_It.hasNext())
				{
					Entry<CmpVertex,Map<Label,List<CmpVertex>>> stateB = stateB_It.next();
					if (LearnerGraphND.ignoreRejectStates.stateToConsider(stateB.getKey()))
					{
						StatePair currentPair = new StatePair(entryA.getKey(),stateB.getKey());
						if (gr.pairscores.computePairCompatibilityScore_general(currentPair,null, new LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>(), false) < 0)
						{// result is constructed as a synchronized set
							result.add(currentPair);
						}
					}						
					if (stateB.getKey().equals(entryA.getKey())) break; // we only process a triangular subset.
				}// iterating through states (stateB)
			}
		});
		GDLearnerGraph.performRowTasks(handlerList, ThreadNumber,ndGraph.matrixForward.transitionMatrix,filter,
				GDLearnerGraph.partitionWorkLoadTriangular(ThreadNumber, ndGraph.matrixForward.transitionMatrix.size()));
		return result;
	}

	protected final void findIncompatibleTestHelper(LearnerGraph gr,List<StringPair> incompatibles_list)
	{
		HashSet<StatePair> incompatibles = new HashSet<StatePair>();
		StatesToConsider filter = LearnerGraphND.ignoreRejectStates;
		GDLearnerGraph ndGraph = new GDLearnerGraph(gr,filter, false);
		for(StringPair p:incompatibles_list)
		{
			incompatibles.add(new StatePair(gr.findVertex(p.firstElem),gr.findVertex(p.secondElem)));
			incompatibles.add(new StatePair(gr.findVertex(p.secondElem),gr.findVertex(p.firstElem)));
		}
		Set<StatePair> incompatiblePairs = new HashSet<StatePair>();
		for(StatePair s:buildSetOfIncompatiblePairsSlowly(gr,threadNumber))
		{
			incompatiblePairs.add(s);incompatiblePairs.add(new StatePair(s.secondElem,s.firstElem));
		}
		// incompatiblePairs these are the pairs which cannot be merged which may be a larger set than 
		// the set of clearly incompatible pairs, because incompatiblePairs is computed based on 
		// the outcome of computePairCompatibilityScore_general. The difference between computing incompatibility of pairs
		// and doing computePairCompatibilityScore_general is that when pairs are merged, more traces
		// are created, hence it is possible that some of those new traces will be in conflict.
		// Example: states A3 and A2 in findIncompatibleStates6()
		HashSet<StatePair> pairs_extra = new HashSet<StatePair>();pairs_extra.addAll(incompatibles);pairs_extra.removeAll(incompatiblePairs);
		Assert.assertTrue("compatible pairs included :"+pairs_extra,pairs_extra.isEmpty());// this is a check that incompatibles_list does not include any compatible pairs

		final int size=ndGraph.getPairNumber();
		final int highNumber = 10000;
		int pairs[]=new int[size];for(int i=0;i<pairs.length;++i) pairs[i]=highNumber;
		
		// reverseMap maps state pair number to the actual state pairs, we slowly build it here
		// rather then rely on getPairScore() which may have not even been written when 
		// I was doing this kind of testing.
		reverseMap = new HashMap<Integer,StatePair>();
		for(CmpVertex A:gr.transitionMatrix.keySet())
			if (filter.stateToConsider(A))
				for(CmpVertex B:gr.transitionMatrix.keySet())
					if (filter.stateToConsider(B))
						reverseMap.put(ndGraph.vertexToIntNR(A, B), new StatePair(A,B));
		Assert.assertEquals(size,reverseMap.size());
		
		ndGraph.buildMatrix(threadNumber);
		ndGraph.findIncompatiblePairs(pairs,threadNumber);
		
		int cnt=0;
		for(int i=0;i<pairs.length;++i)
		{
			StatePair pair = reverseMap.get(i);
			if (incompatibles.contains(pair))
				Assert.assertEquals("pair ("+pair.firstElem+","+pair.secondElem+", id "+i+") should be marked as incompatible",
						PAIR_INCOMPATIBLE,pairs[i]);// this pair is not compatible
			else
				Assert.assertEquals("invalid counter for pair ("+pair.firstElem+","+pair.secondElem+")", 
						cnt++,pairs[i]);// compatible pairs should have monotonically increasing numbers
				
		}
	}

	/** Tests that if A is not compatible to D, this is recorded. */
	@Test
	public final void TestFindIncompatibleStates1()
	{
		LearnerGraph gr=buildLearnerGraph("A-a->Q\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C-a->C\nD-c->A-c-#R","TestFindIncompatibleStates1",config,converter);
		findIncompatibleTestHelper(gr, Arrays.asList(new StringPair[]{
				new StringPair("A","D")
		}));
	}

	/** Tests that if A is not compatible to C and D, this is recorded. */
	@Test
	public final void TestFindIncompatibleStates2()
	{
		LearnerGraph gr=buildLearnerGraph("A-a-#Q\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C-a->C\nD-c->A-c-#R","TestFindIncompatibleStates2",config,converter);

		findIncompatibleTestHelper(gr, Arrays.asList(new StringPair[]{
				new StringPair("A","D"),new StringPair("A","C")
		}));
	}

	@Test
	public final void TestFindIncompatibleStates3a()
	{
		LearnerGraph gr=buildLearnerGraph("A3-a->A2-a->A1-a->A-b-#R\nB3-a->B2-a->B1-a->B-b->D","TestFindIncompatibleStates3",config,converter);

		findIncompatibleTestHelper(gr, Arrays.asList(new StringPair[]{
				new StringPair("A","B"),new StringPair("A1","B1"),new StringPair("A2","B2"),new StringPair("A3","B3")
		}));
	}

	/** Tests using <em>addRejectVertices()</em> run the tool on graphs 
	 * with numerous reject-states. An earlier version
	 * which was not using GDLearnerGraph had to go through all the states and ignore reject
	 * ones, moreover, most methods of it had to do this too. These tests attempt to flood it with
	 * rejects to ensure I did not forget such checks. The current version filters all reject
	 * states out and hence does not need this kind of testing, but these tests are worth
	 * keeping to both test filtering and I may yet decide to go back, not sure why though. 
	 */
	@Test
	public final void TestFindIncompatibleStates3b()
	{
		LearnerGraph gr=buildLearnerGraph("C6-c->C5-c->C4-c->C3\n"+"B6-c->B5-c->B4-c->B3\n"+
				"C3-a->C2-a->C1-a->C0-b-#R\nB3-a->B2-a->B1-a->B0-b->D","TestFindIncompatibleStates3",config,converter);
		for(String prefix:new String[]{"Z","Y"})
			TestLinear.addRejectVertices(gr,prefix,5);
		findIncompatibleTestHelper(gr, Arrays.asList(new StringPair[]{
				new StringPair("C0","B0"),new StringPair("C1","B1"),new StringPair("C2","B2"),new StringPair("C3","B3"),
				new StringPair("C4","B4"),new StringPair("C5","B5"),new StringPair("C6","B6")
		}));
	}

	@Test
	public final void TestFindIncompatibleStates3c()
	{
		LearnerGraph gr=buildLearnerGraph("C4-c->C0\nC3-c->C0\n"+
				"B4-c->B0\nB3-c->B0\n"+
				"C3-a->C2-a->C1-a->C0-b-#R\nB3-a->B2-a->B1-a->B0-b->D","TestFindIncompatibleStates3",config,converter);
		for(String prefix:new String[]{"Z","Y"})
			TestLinear.addRejectVertices(gr,prefix,5);
		findIncompatibleTestHelper(gr, Arrays.asList(new StringPair[]{
				new StringPair("C0","B0"),new StringPair("C1","B1"),new StringPair("C2","B2"),new StringPair("C3","B3"),
				new StringPair("C4","B4"),
				new StringPair("C4","B3"),new StringPair("C3","B4")
		}));
	}


	
	@Test
	public final void TestFindIncompatibleStates4()
	{
		LearnerGraph gr=buildLearnerGraph("B1-b->C\n"+
				"A3-a->A2-a->A1-a->A-b-#R\nB3-a->B2-a->B1-a->B-b->D","TestFindIncompatibleStates4",config,converter);

		findIncompatibleTestHelper(gr, Arrays.asList(new StringPair[]{
				new StringPair("A","B"),new StringPair("A1","B1"),new StringPair("A2","B2"),new StringPair("A3","B3"),
				new StringPair("A","B1"),new StringPair("A1","B2"),new StringPair("A2","B3")
		}));
	}

	@Test
	public final void TestFindIncompatibleStates5()
	{
		LearnerGraph gr=buildLearnerGraph("B3-b->B3\n"+
				"B1-b->C\n"+
				"A3-a->A2-a->A1-a->A-b-#R\nB3-a->B2-a->B1-a->B-b->D","TestFindIncompatibleStates5",config,converter);

		findIncompatibleTestHelper(gr, Arrays.asList(new StringPair[]{
				new StringPair("A","B"),new StringPair("A1","B1"),new StringPair("A2","B2"),new StringPair("A3","B3"),
				new StringPair("A","B1"),new StringPair("A1","B2"),new StringPair("A2","B3"),
				new StringPair("A","B3")
		}));
	}

	@Test
	public final void TestFindIncompatibleStates6()
	{
		LearnerGraph gr=buildLearnerGraph("B3-b->B3\n"+"A3-b->A3\n"+
				"B1-b->C\n"+
				"A3-a->A2-a->A1-a->A-b-#R\nB3-a->B2-a->B1-a->B-b->D","TestFindIncompatibleStates6",config,converter);

		findIncompatibleTestHelper(gr, Arrays.asList(new StringPair[]{
				new StringPair("A","B"),new StringPair("A1","B1"),new StringPair("A2","B2"),new StringPair("A3","B3"),
				new StringPair("A","B1"),new StringPair("A1","B2"),new StringPair("A2","B3"),
				new StringPair("A","B3"),
				new StringPair("A3","A")
		}));
	}

	@Test
	public final void TestFindIncompatibleStates7()
	{
		LearnerGraph gr=buildLearnerGraph("A-a->E-a->C-a->A-b-#R\nB-a->H-a->G-a->F-a->D-a->B-b->Q"
				,"TestFindIncompatibleStates7",config,converter);

		findIncompatibleTestHelper(gr, Arrays.asList(new StringPair[]{
				new StringPair("A","B"),new StringPair("A","D"),new StringPair("A","F"),new StringPair("A","G"),new StringPair("A","H"),
				new StringPair("C","B"),new StringPair("C","D"),new StringPair("C","F"),new StringPair("C","G"),new StringPair("C","H"),
				new StringPair("E","B"),new StringPair("E","D"),new StringPair("E","F"),new StringPair("E","G"),new StringPair("E","H")
		}));
	}
	
	@Test
	public final void TestFindIncompatibleStates8()
	{
		LearnerGraph gr=buildLearnerGraph("E-a->C-a->A-b-#R\nE-c->C-c->E\nB-c->H-c->G-c->F-c->D-c->B-b->Q\n"
				,"TestFindIncompatibleStates8",config,converter);

		findIncompatibleTestHelper(gr, Arrays.asList(new StringPair[]{
				new StringPair("A","B")
		}));
	}
	
	@Test
	public final void TestFindIncompatibleStates9()
	{
		LearnerGraph gr=buildLearnerGraph("D-a->B\n"+
				"E-a->C-a->A-b-#R\nE-c->C-c->E\nB-c->H-c->G-c->F-c->D-c->B-b->Q\n"
				,"TestFindIncompatibleStates9",config,converter);

		findIncompatibleTestHelper(gr, Arrays.asList(new StringPair[]{
				new StringPair("A","B"),
				new StringPair("C","B"),new StringPair("C","D"),new StringPair("C","F"),new StringPair("C","G"),new StringPair("C","H"),
				new StringPair("E","B"),new StringPair("E","D"),new StringPair("E","F"),new StringPair("E","G"),new StringPair("E","H")
		}));
	}
	@Test
	public final void TestFindIncompatibleStates10()
	{
		LearnerGraph gr=buildLearnerGraph("B-a->C-a-#A\nC-b-#A"
				,"TestFindIncompatibleStates10",config,converter);

		findIncompatibleTestHelper(gr, Arrays.asList(new StringPair[]{
				new StringPair("C","B")
		}));
	}
	
	public static Set<PairScore> addAllPermutations(Collection<PairScore> scores)
	{
		Set<PairScore> pairsSet = new TreeSet<PairScore>();
		for(PairScore p:scores)
		{
			pairsSet.add(new PairScore(p.firstElem,p.secondElem,p.getScore(),0));
			pairsSet.add(new PairScore(p.secondElem,p.firstElem,p.getScore(),0));
		}
		return pairsSet;
	}
	
	protected final static String machineCompatibility1="A-a->B-a->C";

	@Test
	public final void TestComputeStateCompatibility1a()
	{
		LearnerGraph gr = buildLearnerGraph(machineCompatibility1,"TestComputeStateCompatibility1",config,converter);
		GDLearnerGraph ndGraph = new GDLearnerGraph(gr,LearnerGraphND.ignoreRejectStates, false);
		DoubleMatrix1D result = DoubleFactory1D.dense.make(ndGraph.computeStateCompatibility(threadNumber,null));
		Assert.assertTrue(DoubleFactory1D.dense.make(new double[]{(1+k/2)/2,1./2,1./2,0,0,0}).equals(result));
	}	
	
	@Test
	public final void TestComputeStateCompatibility1b()
	{
		LearnerGraph gr = buildLearnerGraph(machineCompatibility1,"TestComputeStateCompatibility1",config,converter);
		
		Set<PairScore> pairsSet = addAllPermutations(PairScoreComputation.chooseStatePairs_filtered(gr,PAIR_INCOMPATIBLE,10,threadNumber,null,LearnerGraphND.ignoreRejectStates, new NonRandomRandom()));
		Set<PairScore> expected = addAllPermutations(Arrays.asList(new PairScore[]{
				new PairScore(gr.findVertex("A"),gr.findVertex("A"),(int)(10*(1+k/2)/2),1),
				new PairScore(gr.findVertex("A"),gr.findVertex("B"),10/2,1),
				new PairScore(gr.findVertex("B"),gr.findVertex("B"),10/2,1),
				new PairScore(gr.findVertex("A"),gr.findVertex("C"),0,1),
				new PairScore(gr.findVertex("B"),gr.findVertex("C"),0,1),
				new PairScore(gr.findVertex("C"),gr.findVertex("C"),0,1)
		}));
		Assert.assertEquals(expected, pairsSet);
	}	
	
	class NonRandomRandom extends StateBasedRandom
	{
		public NonRandomRandom() 
		{
			super(0);
		}
		
		@Override
		synchronized public Random getRandom(CmpVertex A)
		{
			return new Random(A.getStringId().hashCode());
		}
	}
	
	@Test
	public final void TestComputeStateCompatibility1b_WalksRH()
	{
		config.setGdScoreComputationAlgorithm(GDScoreComputationAlgorithmEnum.SCORE_RANDOMPATHS);
		config.setGdScoreComputation(GDScoreComputationEnum.GD_RH);
		config.setGdScoreComputationAlgorithm_RandomWalk_NumberOfSequences(2);
		config.setGdScoreComputationAlgorithm_RandomWalk_ExtraLength(0);

		LearnerGraph gr = buildLearnerGraph(machineCompatibility1,"TestComputeStateCompatibility1",config,converter);
		
		Set<PairScore> pairsSet = addAllPermutations(PairScoreComputation.chooseStatePairs_filtered(gr,PAIR_INCOMPATIBLE,10,threadNumber,DDRH_BCR.class,
				LearnerGraphND.ignoreRejectStates, new NonRandomRandom()));
		Set<PairScore> expected = addAllPermutations(Arrays.asList(new PairScore[]{
				new PairScore(gr.findVertex("A"),gr.findVertex("A"),325,1),
				new PairScore(gr.findVertex("A"),gr.findVertex("B"),125,1),
				new PairScore(gr.findVertex("B"),gr.findVertex("B"),250,1),
				new PairScore(gr.findVertex("A"),gr.findVertex("C"),0,1),
				new PairScore(gr.findVertex("B"),gr.findVertex("C"),0,1),
				new PairScore(gr.findVertex("C"),gr.findVertex("C"),0,1)
		}));
		Assert.assertEquals(expected, pairsSet);
	}	

	@Test
	public final void TestComputeStateCompatibility1b_TestsRH()
	{
		config.setGdScoreComputationAlgorithm(GDScoreComputationAlgorithmEnum.SCORE_TESTSET);
		config.setGdScoreComputation(GDScoreComputationEnum.GD_RH);
		config.setGdScoreComputationAlgorithm_RandomWalk_NumberOfSequences(2);
		config.setGdScoreComputationAlgorithm_RandomWalk_ExtraLength(0);

		LearnerGraph gr = buildLearnerGraph(machineCompatibility1,"TestComputeStateCompatibility1",config,converter);
		
		Set<PairScore> pairsSet = addAllPermutations(PairScoreComputation.chooseStatePairs_filtered(gr,PAIR_INCOMPATIBLE,10,threadNumber,DDRH_BCR.class,
				LearnerGraphND.ignoreRejectStates, new NonRandomRandom()));
		Set<PairScore> expected = addAllPermutations(Arrays.asList(new PairScore[]{
				new PairScore(gr.findVertex("A"),gr.findVertex("A"),325,1),
				new PairScore(gr.findVertex("A"),gr.findVertex("B"),162,1),
				new PairScore(gr.findVertex("B"),gr.findVertex("B"),250,1),
				new PairScore(gr.findVertex("A"),gr.findVertex("C"),125,1),
				new PairScore(gr.findVertex("B"),gr.findVertex("C"),125,1),
				new PairScore(gr.findVertex("C"),gr.findVertex("C"),0,1)
		}));
		Assert.assertEquals(expected, pairsSet);
	}	

	@Test
	public final void TestComputeStateCompatibility1b_Tests()
	{
		config.setGdScoreComputationAlgorithm(GDScoreComputationAlgorithmEnum.SCORE_TESTSET);
		config.setGdScoreComputation(GDScoreComputationEnum.GD_DIRECT);
		config.setGdScoreComputationAlgorithm_RandomWalk_NumberOfSequences(2);
		config.setGdScoreComputationAlgorithm_RandomWalk_ExtraLength(0);

		LearnerGraph gr = buildLearnerGraph(machineCompatibility1,"TestComputeStateCompatibility1",config,converter);
		
		Set<PairScore> pairsSet = addAllPermutations(PairScoreComputation.chooseStatePairs_filtered(gr,PAIR_INCOMPATIBLE,10,threadNumber,DDRH_BCR.class,
				LearnerGraphND.ignoreRejectStates, new NonRandomRandom()));
		Set<PairScore> expected = addAllPermutations(Arrays.asList(new PairScore[]{
				new PairScore(gr.findVertex("A"),gr.findVertex("A"),500,1),
				new PairScore(gr.findVertex("A"),gr.findVertex("B"),250,1),
				new PairScore(gr.findVertex("B"),gr.findVertex("B"),500,1),
				new PairScore(gr.findVertex("A"),gr.findVertex("C"),250,1),
				new PairScore(gr.findVertex("B"),gr.findVertex("C"),250,1),
				new PairScore(gr.findVertex("C"),gr.findVertex("C"),0,1)
		}));
		Assert.assertEquals(expected, pairsSet);
	}	

	@Test
	public final void TestComputeStateCompatibility1b_Walks()
	{
		config.setGdScoreComputationAlgorithm(GDScoreComputationAlgorithmEnum.SCORE_RANDOMPATHS);
		config.setGdScoreComputation(GDScoreComputationEnum.GD_DIRECT);
		config.setGdScoreComputationAlgorithm_RandomWalk_NumberOfSequences(2);
		config.setGdScoreComputationAlgorithm_RandomWalk_ExtraLength(0);

		LearnerGraph gr = buildLearnerGraph(machineCompatibility1,"TestComputeStateCompatibility1",config,converter);
		
		Set<PairScore> pairsSet = addAllPermutations(PairScoreComputation.chooseStatePairs_filtered(gr,PAIR_INCOMPATIBLE,10,threadNumber,DDRH_BCR.class,
				LearnerGraphND.ignoreRejectStates, new NonRandomRandom()));
		Set<PairScore> expected = addAllPermutations(Arrays.asList(new PairScore[]{
				new PairScore(gr.findVertex("A"),gr.findVertex("A"),500,1),
				new PairScore(gr.findVertex("A"),gr.findVertex("B"),250,1),
				new PairScore(gr.findVertex("B"),gr.findVertex("B"),500,1),
				new PairScore(gr.findVertex("A"),gr.findVertex("C"),0,1),
				new PairScore(gr.findVertex("B"),gr.findVertex("C"),0,1),
				new PairScore(gr.findVertex("C"),gr.findVertex("C"),0,1)
		}));
		Assert.assertEquals(expected, pairsSet);
	}	

	
	protected final static String machineCompatibility2="A-a->B-a->C-a-#D";

	@Test
	public final void TestComputeStateCompatibility2a()
	{
		LearnerGraph gr=buildLearnerGraph(machineCompatibility2,"TestComputeStateCompatibility1",config,converter);
		GDLearnerGraph ndGraph = new GDLearnerGraph(gr,LearnerGraphND.ignoreRejectStates, false);
		DoubleMatrix1D result = DoubleFactory1D.dense.make(ndGraph.computeStateCompatibility(threadNumber,null));
		//DoubleMatrix1D expected=DoubleFactory1D.dense.make(new double[]{1+k,PAIR_INCOMPATIBLE,1,PAIR_INCOMPATIBLE,PAIR_INCOMPATIBLE,0});
		DoubleMatrix1D expected=DoubleFactory1D.dense.make(new double[]{(1+k/2*(k/2+1))/2,PAIR_INCOMPATIBLE,(1+k/2)/2,PAIR_INCOMPATIBLE,PAIR_INCOMPATIBLE,1./2});
		Assert.assertTrue("unexpected results: "+result+"\nexpected: "+expected,expected.equals(result));
	}

	@Test
	public final void TestComputeStateCompatibility2b()
	{
		LearnerGraph gr=buildLearnerGraph(machineCompatibility2,"TestComputeStateCompatibility1",config,converter);
		Set<PairScore> pairsSet = addAllPermutations(PairScoreComputation.chooseStatePairs_filtered(gr,PAIR_INCOMPATIBLE*2,10,threadNumber,null,LearnerGraphND.ignoreRejectStates, new NonRandomRandom()));
		Set<PairScore> exp = addAllPermutations(Arrays.asList(new PairScore[]{
				new PairScore(gr.findVertex("A"),gr.findVertex("A"),(int)(10*(1+k/2*(1+k/2))/2),1),
				new PairScore(gr.findVertex("A"),gr.findVertex("B"),10*PAIR_INCOMPATIBLE,1),
				new PairScore(gr.findVertex("B"),gr.findVertex("B"),(int)(10*(1+k/2)/2),1),
				new PairScore(gr.findVertex("A"),gr.findVertex("C"),10*PAIR_INCOMPATIBLE,1),
				new PairScore(gr.findVertex("B"),gr.findVertex("C"),10*PAIR_INCOMPATIBLE,1),
				new PairScore(gr.findVertex("C"),gr.findVertex("C"),10/2,1)
		}));
		Assert.assertEquals(exp, pairsSet);
	}


	@Test
	public final void TestComputeStateCompatibility2c()
	{
		LearnerGraph gr=buildLearnerGraph(machineCompatibility2,"TestComputeStateCompatibility1",config,converter);
		Set<PairScore> pairsSet = addAllPermutations(PairScoreComputation.chooseStatePairs_filtered(gr,PAIR_INCOMPATIBLE,10,threadNumber,null,LearnerGraphND.ignoreRejectStates, new NonRandomRandom()));
		Set<PairScore> exp = addAllPermutations(Arrays.asList(new PairScore[]{
				new PairScore(gr.findVertex("A"),gr.findVertex("A"),(int)(10*(1+k/2*(1+k/2))/2),1),
				new PairScore(gr.findVertex("B"),gr.findVertex("B"),(int)(10*(1+k/2)/2),1),
				new PairScore(gr.findVertex("C"),gr.findVertex("C"),10/2,1)
		}));
		Assert.assertEquals(exp, pairsSet);
	}

	@Test
	public final void TestComputeStateCompatibility2d()
	{
		LearnerGraph gr=buildLearnerGraph(machineCompatibility2,"TestComputeStateCompatibility1",config,converter);
		Set<PairScore> pairsSet = addAllPermutations(PairScoreComputation.chooseStatePairs_filtered(gr,0.5,10,threadNumber,null,LearnerGraphND.ignoreRejectStates, new NonRandomRandom()));
		Set<PairScore> exp = addAllPermutations(Arrays.asList(new PairScore[]{
				new PairScore(gr.findVertex("A"),gr.findVertex("A"),(int)(10*(1+k/2*(1+k/2))/2),1),
				new PairScore(gr.findVertex("B"),gr.findVertex("B"),(int)(10*(1+k/2)/2),1)
		}));
		Assert.assertEquals(exp, pairsSet);
	}

	@Test
	public final void TestComputeStateCompatibility2e()
	{
		LearnerGraph gr=buildLearnerGraph(machineCompatibility2,"TestComputeStateCompatibility1",config,converter);
		Set<PairScore> pairsSet = addAllPermutations(PairScoreComputation.chooseStatePairs_filtered(gr,5,10,threadNumber,null,LearnerGraphND.ignoreRejectStates, new NonRandomRandom()));
		Assert.assertTrue(pairsSet.isEmpty());
	}
	
	@Test
	public final void TestComputeStateCompatibility_checking_filtering1()
	{
		LearnerGraph gr=buildLearnerGraph(machineCompatibility2,"TestComputeStateCompatibility1",config,converter);
		Set<PairScore> pairsSet = addAllPermutations(PairScoreComputation.chooseStatePairs(gr,PAIR_INCOMPATIBLE*2,10,threadNumber,null,LearnerGraphND.ignoreRejectStates, new NonRandomRandom()));
		Set<PairScore> exp = addAllPermutations(Arrays.asList(new PairScore[]{
				new PairScore(gr.findVertex("A"),gr.findVertex("A"),(int)(10*(1+k/2*(1+k/2))/2),1),
				new PairScore(gr.findVertex("A"),gr.findVertex("B"),10*PAIR_INCOMPATIBLE,1),
				new PairScore(gr.findVertex("B"),gr.findVertex("B"),(int)(10*(1+k/2)/2),1),
				new PairScore(gr.findVertex("A"),gr.findVertex("C"),10*PAIR_INCOMPATIBLE,1),
				new PairScore(gr.findVertex("B"),gr.findVertex("C"),10*PAIR_INCOMPATIBLE,1),
				new PairScore(gr.findVertex("C"),gr.findVertex("C"),10/2,1),
				
				new PairScore(gr.findVertex("A"),gr.findVertex("D"),10*PAIR_INCOMPATIBLE,1),
				new PairScore(gr.findVertex("B"),gr.findVertex("D"),10*PAIR_INCOMPATIBLE,1),
				new PairScore(gr.findVertex("C"),gr.findVertex("D"),10*PAIR_INCOMPATIBLE,1),
				new PairScore(gr.findVertex("D"),gr.findVertex("D"),0,1),
				
		}));
		Assert.assertEquals(exp, pairsSet);
	}

	private final void TestScoreComputationWithFilters(Class<? extends StatesToConsider> filterClass)
	{
		LearnerGraph gr=buildLearnerGraph(machineCompatibility2+
				"\nB-b-#G\n","TestComputeStateCompatibility_checking_filtering2",config,converter);
		StatesToConsider filter = TestLearnerGraphND.createInstanceOfFilter(filterClass, gr);
		double score_CC=1./2,score_BB=(2+score_CC*k)/4, score_AA=(1+k*score_BB)/2;
		{
			GDLearnerGraph ndGraph = new GDLearnerGraph(gr,LearnerGraphND.ignoreNone, false);
			LSolver solver = ndGraph.buildMatrix(threadNumber);
			DoubleMatrix2D Ax=solver.toDoubleMatrix2D();
			Assert.assertEquals(6,Ax.columns());
		}
		Set<PairScore> pairsSet = addAllPermutations(PairScoreComputation.chooseStatePairs(gr,PAIR_INCOMPATIBLE*2,10,threadNumber,null,filter, new NonRandomRandom()));
		Set<PairScore> exp = addAllPermutations(Arrays.asList(new PairScore[]{
				new PairScore(gr.findVertex("A"),gr.findVertex("A"),(int)(10*score_AA),1),
				new PairScore(gr.findVertex("A"),gr.findVertex("B"),10*PAIR_INCOMPATIBLE,1),
				new PairScore(gr.findVertex("B"),gr.findVertex("B"),(int)(10*score_BB),1),
				new PairScore(gr.findVertex("A"),gr.findVertex("C"),10*PAIR_INCOMPATIBLE,1),
				new PairScore(gr.findVertex("B"),gr.findVertex("C"),10*PAIR_INCOMPATIBLE,1),
				new PairScore(gr.findVertex("C"),gr.findVertex("C"),(int)(10*score_CC),1),
				
				new PairScore(gr.findVertex("A"),gr.findVertex("D"),10*PAIR_INCOMPATIBLE,1),
				new PairScore(gr.findVertex("B"),gr.findVertex("D"),10*PAIR_INCOMPATIBLE,1),
				new PairScore(gr.findVertex("C"),gr.findVertex("D"),10*PAIR_INCOMPATIBLE,1),
				new PairScore(gr.findVertex("D"),gr.findVertex("D"),0,1),
				
				new PairScore(gr.findVertex("A"),gr.findVertex("G"),10*PAIR_INCOMPATIBLE,1),
				new PairScore(gr.findVertex("B"),gr.findVertex("G"),10*PAIR_INCOMPATIBLE,1),
				new PairScore(gr.findVertex("C"),gr.findVertex("G"),10*PAIR_INCOMPATIBLE,1),
				new PairScore(gr.findVertex("D"),gr.findVertex("G"),0,1),
				new PairScore(gr.findVertex("G"),gr.findVertex("G"),0,1),
				
		}));
		Assert.assertEquals(exp, pairsSet);
		
	}
	
	@Test
	public final void TestComputeStateCompatibility_checking_filtering2a()
	{
		TestScoreComputationWithFilters(LearnerGraphND.ignoreNoneClass.class);
	}

	@Test
	public final void TestComputeStateCompatibility_checking_filtering2b()
	{
		TestScoreComputationWithFilters(LearnerGraphND.ignoreRejectStatesClass.class);
	}

	@Test
	public final void TestComputeStateCompatibility_checking_filtering2c()
	{
		TestScoreComputationWithFilters(LearnerGraphND.ignoreZeroClass.class);
	}

	@Test
	public final void TestComputeStateCompatibility_checking_filtering3()
	{
		LearnerGraph gr=buildLearnerGraph(machineCompatibility2+
				"\nB-c-#G\n","TestComputeStateCompatibility_checking_filtering2",config,converter);
		double score_CC=1./2,score_BB=(2+score_CC*k)/4, score_AA=(1+k*score_BB)/2;
		Set<PairScore> pairsSet = addAllPermutations(PairScoreComputation.chooseStatePairs(gr,PAIR_INCOMPATIBLE,10,threadNumber,null,LearnerGraphND.ignoreRejectStates, new NonRandomRandom()));
		Set<PairScore> exp = addAllPermutations(Arrays.asList(new PairScore[]{
				new PairScore(gr.findVertex("A"),gr.findVertex("A"),(int)(10*score_AA),1),
				new PairScore(gr.findVertex("B"),gr.findVertex("B"),(int)(10*score_BB),1),
				new PairScore(gr.findVertex("C"),gr.findVertex("C"),(int)(10*score_CC),1),
				
				new PairScore(gr.findVertex("D"),gr.findVertex("D"),0,1),
				
				new PairScore(gr.findVertex("D"),gr.findVertex("G"),0,1),
				new PairScore(gr.findVertex("G"),gr.findVertex("G"),0,1),
				
		}));
		Assert.assertEquals(exp, pairsSet);
	}
	
	@Test
	public final void TestComputeStateCompatibility_checking_filtering4()
	{
		LearnerGraph gr=buildLearnerGraph(machineCompatibility2+
				"\nB-b-#G\n","TestComputeStateCompatibility_checking_filtering2",config,converter);
		double score_CC=1./2,score_BB=(2+score_CC*k)/4, score_AA=(1+k*score_BB)/2;
		Set<PairScore> pairsSet = addAllPermutations(PairScoreComputation.chooseStatePairs(gr,0,10,threadNumber,null,LearnerGraphND.ignoreRejectStates, new NonRandomRandom()));
		Set<PairScore> exp = addAllPermutations(Arrays.asList(new PairScore[]{
				new PairScore(gr.findVertex("A"),gr.findVertex("A"),(int)(10*score_AA),1),
				new PairScore(gr.findVertex("B"),gr.findVertex("B"),(int)(10*score_BB),1),
				new PairScore(gr.findVertex("C"),gr.findVertex("C"),(int)(10*score_CC),1),
		}));
		Assert.assertEquals(exp, pairsSet);
	}
}
