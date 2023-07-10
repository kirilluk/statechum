package statechum.analysis.learning.rpnicore;

import java.util.Collection;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;

import statechum.Label;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass.IncompatibleStatesException;
import statechum.collections.ConvertibleToInt;

public interface EquivalenceClass<TARGET_TYPE, CACHE_TYPE extends CachedData<TARGET_TYPE, CACHE_TYPE>> extends ConvertibleToInt {

	int getNumber();

	/** Returns transitions leaving states contained in this equivalence class. */
	Map<Label, Object> getOutgoing();

	Set<CmpVertex> getStates();

	/** Returns the current representative. */
	CmpVertex getRepresentative();

	/** Adds transitions from the supplied collection.
	 * 
	 * @param from transitions to add from.
	 * @throws IncompatibleStatesException if vertex is not compatible with any vertices in the collection.
	 * @throws statechum.analysis.learning.rpnicore.AMEquivalenceClass.IncompatibleStatesException
	 */
	boolean mergeWith(CmpVertex vert, Collection<Entry<Label, CmpVertex>> from) throws IncompatibleStatesException;

	/** Adds the contents of the supplied argument to outgoing transitions of this class.
	 * 
	 * @param to the equivalence class to merge with
	 * @throws IncompatibleStatesException if vertex is not compatible with any vertices in the collection.
	 * @throws statechum.analysis.learning.rpnicore.AMEquivalenceClass.IncompatibleStatesException
	 */
	boolean mergeWith(EquivalenceClass<TARGET_TYPE, CACHE_TYPE> to) throws IncompatibleStatesException;

	int compareTo(EquivalenceClass<TARGET_TYPE, CACHE_TYPE> o);

	CmpVertex getMergedVertex();

	/** Returns states that are not compatible with any state in this equivalence class. */
	Collection<CmpVertex> incompatibleStates();
	
	/** Generates a vertex representing the representative vertex. If <em>useDifferentName</em> is false, 
	 * a true clone of a representative vertex is made, with the same ID; otherwise  
	 * a new name is chosen.
	 * 
	 * @param graph the graph which will be used to store the generated vertex
	 * @param useDifferentNameIfAlreadyExist whether to retain the ID of a representative vertex in the merged one or 
	 * check if the representative vertex already exists in the graph and if so create a new one.
	 * @param setOrigState whether to set the original state of a merged vertex to the ID of the representative state.
	 */
	<TARGET_C_TYPE, CACHE_C_TYPE extends CachedData<TARGET_C_TYPE, CACHE_C_TYPE>> void constructMergedVertex(
			AbstractLearnerGraph<TARGET_C_TYPE, CACHE_C_TYPE> graph, boolean useDifferentNameIfAlreadyExist,
			boolean setOrigState);

	@Override
	int toInt();

}