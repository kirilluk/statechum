/* Copyright (c) 2016 The University of Sheffield.
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
package statechum.analysis.learning.experiments.MarkovEDSM;

import java.util.*;

import statechum.Pair;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms.ScoringToApply;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.ThreadResultID;
import statechum.analysis.learning.rpnicore.RandomPathGenerator;

/** Describes setup of experiments using markov learning. This is different to
 * {@link MarkovParameters} that describe how to learn each automaton.
 */
public abstract class MarkovLearningParameters implements ThreadResultID
{
	public ScoringToApply learnerToUse;
	public final int states;
	public final int sample;
	public final int trainingSample;
	public double alphabetMultiplier = 1;
	public double traceLengthMultiplier = 1;
	public int traceQuantity;
	public double traceLengthMultiplierMax,alphabetMultiplierMax;
	boolean usePrintf = false;
	public int perStateSquaredDensityMultipliedBy100 = 0;
	public MarkovParameters markovParameters = new MarkovParameters();
	boolean reportMergeStatisticsWhenTheCorrectSolutionIsKnown = true;

	public RandomPathGenerator.WALKTYPE walkType = null;
	public double explorationPreference;
	public int selectionPenalty;

	public MarkovLearningParameters(ScoringToApply l, int argStates, double argAlphabetMultiplier, int perStateSquaredDensity10, int argSample, int argTrainingSample)
	{
		learnerToUse = l;
		states = argStates;alphabetMultiplier = argAlphabetMultiplier;
		perStateSquaredDensityMultipliedBy100 = perStateSquaredDensity10;sample = argSample;trainingSample = argTrainingSample;
	}
	
	public void setExperimentID(int traceQuantity, double argTraceLengthMultiplierMax, double argAlphabetMultiplierMax)
	{
		this.traceQuantity = traceQuantity;this.traceLengthMultiplierMax = argTraceLengthMultiplierMax;this.alphabetMultiplierMax = argAlphabetMultiplierMax;
	}

	public void setWalkType(RandomPathGenerator.WALKTYPE walkType, double explorationPreference, int selectionPenalty) {
		this.walkType = walkType;this.explorationPreference = explorationPreference;this.selectionPenalty = selectionPenalty;
	}

	enum MarkovRowEnum {
		M_ROW_TRACEQUANTITY("tQ"),
		M_ROW_TRACELENGTHMULTIPLIERMAX("tMM"),
		M_ROW_ALPHABETMULTIPLIERMAX("aMM"),
		M_ROW_STATES("S"),
		M_ROW_ALPHABETMULTIPLIER("m"),
		M_ROW_DENSITY("d"),
		M_ROW_SAMPLE("sa"),// automaton number
		M_ROW_TRAININGSAMPLE("tS"),// PTA number for specific automaton
		M_ROW_TRACELENGTHMULTIPLIER("tM");
		public final String text;
		MarkovRowEnum(String v) {
			text = v;
		}

		static MarkovRowEnum fromString(String v) {
			for(MarkovRowEnum c : MarkovRowEnum.values())
				if (c.text.equals(v))
					return c;

			throw new IllegalArgumentException("Invalid text \""+v+"\" passed for MarkovRowEnum");
		}
	}


	/**
	 * Reflects the name of the experiment attempting inference from a range of FSMs.
	 * Here I should be using MarkovRowEnum constants instead of "tQ" and the like,
	 * however these are long which would make the text of getExperimentID() nearly
	 * unreadable and I need to keep looking at it in order to understand what file names refer to (experiment ID is part of a directory name).
	 */
	public String getExperimentID()
	{
		return "tQ="+traceQuantity+"_tMM="+traceLengthMultiplierMax+"_aMM="+alphabetMultiplierMax;
	}
	
	public void setUsePrintf(boolean value)
	{
		usePrintf = value;
	}
	
	public void setTraceLengthMultiplier(double traceMulti) {
		traceLengthMultiplier=traceMulti;
	}

	public void disableReportMergeStatisticsWhenSolutionIsKnown() {
		reportMergeStatisticsWhenTheCorrectSolutionIsKnown = false;
	}

	@Override
	public String getRowID() {
		return getExperimentID()+"_S="+states+"_m="+alphabetMultiplier+"_d="+ perStateSquaredDensityMultipliedBy100 +"_sa="+sample+"_tS="+trainingSample+
				"_tM="+traceLengthMultiplier;
	}

	public static MarkovLearningParameters parseMarkovParametersRowFromCSV(String row) {
		return parseMarkovParametersRowFromCSV(row,null);
	}

	public static MarkovLearningParameters parseMarkovParametersRowFromCSV(String row, ScoringToApply learner) {
		int states = -1;
		int sample = -1;
		int trainingSample = -1;
		double alphabetMultiplier = -1;
		double traceLengthMultiplier = -1;
		int traceQuantity = -1;
		double traceLengthMultiplierMax = -1,alphabetMultiplierMax = -1;
		int perStateSquaredDensityMultipliedBy100 = -1;

		String [] elem = row.split("[_=]");
		if (elem.length % 2 != 0)
			throw new IllegalArgumentException("Row "+row+" should have an even number of entries");
		for(int i=0;i<elem.length;i+=2) {
			MarkovRowEnum elemTag = MarkovRowEnum.fromString(elem[i]);
			switch(elemTag) {
				 case M_ROW_TRACEQUANTITY:
					 traceQuantity = Integer.parseInt(elem[i+1]);break;
				case M_ROW_TRACELENGTHMULTIPLIERMAX:
					traceLengthMultiplierMax = Double.parseDouble(elem[i+1]);break;
				case M_ROW_ALPHABETMULTIPLIERMAX:
					alphabetMultiplierMax = Double.parseDouble(elem[i+1]);break;
				case M_ROW_STATES:
					states = Integer.parseInt(elem[i+1]);break;
				case M_ROW_ALPHABETMULTIPLIER:
					alphabetMultiplier = Double.parseDouble(elem[i+1]);break;
				case M_ROW_DENSITY:
					perStateSquaredDensityMultipliedBy100 = Integer.parseInt(elem[i+1]);break;
				case M_ROW_SAMPLE:
					sample = Integer.parseInt(elem[i+1]);break;
				case M_ROW_TRAININGSAMPLE:
					trainingSample = Integer.parseInt(elem[i+1]);break;
				case M_ROW_TRACELENGTHMULTIPLIER:
					traceLengthMultiplier = Double.parseDouble(elem[i+1]);break;
				default:
					throw new IllegalArgumentException("Unknown tag "+elem[i]+" in "+row);
			}
		}

		MarkovLearningParameters outcome = new MarkovLearningParameters(learner,states,alphabetMultiplier,perStateSquaredDensityMultipliedBy100,sample,trainingSample) {

			@Override
			public String getSubExperimentName() {
				return "PARSE_RESULT";
			}
		};
		outcome.markovParameters = null;
		outcome.setTraceLengthMultiplier(traceLengthMultiplier);
		outcome.setExperimentID(traceQuantity,traceLengthMultiplierMax,alphabetMultiplierMax);

		if (!outcome.getRowID().equals(row))
			throw new IllegalArgumentException("Parsing of row \""+row+"\" produced a different outcome of \""+outcome.getRowID()+"\"");

		return outcome;
	}

	enum MarkovColumnEnum {
		M_COLUMN_AVERAGEORMAX("dv"),
		M_COLUMN_DIVISORFORPATHCOUNT("d"),
		M_COLUMN_EXPECTEDWLEN("wl"),
		M_COLUMN_BLUESTATESFORWARDANDBACKWARD("b"),
		M_COLUMN_CHUNKLEN("cl"),
		M_COLUMN_WEIGHT("wW"),
		M_COLUMN_OFFSET("wO"),
		M_COLUMN_PENALISEMISSING("m"),
		M_COLUMN_SEEDTOSHUFFLEBLUE("sh");

		public final String text;
		MarkovColumnEnum(String v) {
			text = v;
		}

		static MarkovColumnEnum fromString(String v) {
			for(MarkovColumnEnum c : MarkovColumnEnum.values())
				if (c.text.equals(v))
					return c;

			throw new IllegalArgumentException("Invalid text \""+v+"\" passed for MarkovColumnEnum");
		}
	}

	public static class ColumnParseOutcome {
		public final MarkovParameters parameters;
		public final ScoringToApply learner;


		public ColumnParseOutcome(MarkovParameters parameters, ScoringToApply learner) {
			this.parameters = parameters;
			this.learner = learner;
		}

		@Override
		public boolean equals(Object o) {
			if (!(o instanceof ColumnParseOutcome)) return false;
			ColumnParseOutcome that = (ColumnParseOutcome) o;
			return Objects.equals(parameters, that.parameters) && learner == that.learner;
		}

		@Override
		public int hashCode() {
			return Objects.hash(parameters, learner);
		}
	}

	public static ColumnParseOutcome parseMarkovParametersColumnFromCSV(String column) {
		double weightOfInconsistencies_Weight = -1, weightAndOffsetOfInconsistencies_Offset = -1;
		boolean blue_states_forward_and_backwards = false;
		boolean useAverageOrMax = true;
		int divisorForPathCount = -1, expectedWLen = -1;
		boolean penaliseMissingPaths = true;
		int seedToShuffleSurroundingStates = -1;
		int chunkLen = -1;

		String [] elem = column.split("[_=]");
		if (elem.length % 2 == 0)
			throw new IllegalArgumentException("Column "+column+" should have an odd number of entries");

		String [] learner_preset = elem[0].split("-");
		// When parsing, we replace the @ sign with an underscore because underscore is reserved as a separator for key-value pairs.
		LearningAlgorithms.ScoringToApply learner = LearningAlgorithms.ScoringToApply.valueOf("SCORING_"+learner_preset[0].replace('@','_'));
		int preset = Integer.parseInt(learner_preset[1]);

		for(int i=1;i<elem.length;i+=2) {
			MarkovColumnEnum elemTag = MarkovColumnEnum.fromString(elem[i]);
			switch(elemTag) {
				case M_COLUMN_AVERAGEORMAX:
					switch(elem[i+1]) {
						case "A":
							useAverageOrMax = true;break;
						case "M":
							useAverageOrMax = false;break;
						default:
							throw new IllegalArgumentException("Entry \""+elem[i+1]+"\" should be either A or M in column "+column);
					}
					break;
				case M_COLUMN_DIVISORFORPATHCOUNT:
					divisorForPathCount = Integer.parseInt(elem[i+1]);break;
				case M_COLUMN_EXPECTEDWLEN:
					expectedWLen = Integer.parseInt(elem[i+1]);break;
				case M_COLUMN_BLUESTATESFORWARDANDBACKWARD:
					switch(elem[i+1]) {
						case "T":
							blue_states_forward_and_backwards = true;break;
						case "F":
							blue_states_forward_and_backwards = false;break;
						default:
							throw new IllegalArgumentException("Entry \""+elem[i+1]+"\" should be either T or F in column "+column);
					}
					break;
				case M_COLUMN_CHUNKLEN:
					chunkLen = Integer.parseInt(elem[i+1]);break;
				case M_COLUMN_WEIGHT:
					weightOfInconsistencies_Weight = Double.parseDouble(elem[i+1]);break;
				case M_COLUMN_OFFSET:
					weightAndOffsetOfInconsistencies_Offset = Double.parseDouble(elem[i+1]);break;
				case M_COLUMN_PENALISEMISSING:
					penaliseMissingPaths = Boolean.parseBoolean(elem[i+1]);break;
				case M_COLUMN_SEEDTOSHUFFLEBLUE:
					seedToShuffleSurroundingStates = Integer.parseInt(elem[i+1]);break;
				default:
					throw new IllegalArgumentException("Unknown tag "+elem[i]+" in "+column);
			}
		}
		MarkovParameters outcome = new MarkovParameters();
		outcome.setShuffleSeed(seedToShuffleSurroundingStates);
		outcome.setMarkovParameters(preset,chunkLen,true,
				new MarkovParameters.WeightAndOffsetOfInconsistencies(weightOfInconsistencies_Weight,weightAndOffsetOfInconsistencies_Offset),
				penaliseMissingPaths,useAverageOrMax,divisorForPathCount,0,expectedWLen);

		if (outcome.useCentreVertex && outcome.blue_states_forward_and_backwards != blue_states_forward_and_backwards)
			throw new IllegalArgumentException("Parsing of column \""+column+"\" returned a different value of blue_states_forward_and_backwards");

		String expectedValue = learner+"-"+outcome.getColumnID(learner.isMarkov());

		if (!expectedValue.equals(column))
			throw new IllegalArgumentException("Parsing of column \""+column+"\" produced a different outcome of \""+expectedValue+"\"");

		return new ColumnParseOutcome(outcome,learner);
	}

	@Override
	/** Reports the meaning of column header values */
	public String[] getColumnText() {
		List<String> columnData = new ArrayList<>(Collections.singletonList(learnerToUse.toString()));
		if (learnerToUse.isMarkov())
			columnData.addAll(markovParameters.getColumnListForMarkovLearner());
		else
			columnData.addAll(markovParameters.getColumnListForNonMarkovLearner());
			
		return columnData.toArray(new String[]{});
	}

	@Override
	/** Reports the values of column header values. */
	public String getColumnID() 
	{
        return learnerToUse.toString()+"-"+markovParameters.getColumnID(learnerToUse.isMarkov());
	}

	@Override
	/** For each row (a specific walk in a specific automaton) and column (learner type and parameters), reports
	 * experiment results.
	 *
	 * % Invalid/missed refer to mergers,
	 * States refer to the difference between expected and learnt states,
	 * I_Ref, I_Lnt are inconsistencies of the original and learnt automata.
	 *
	 * fracS is the %% of states identifiable as singletons,
	 * marPre is Markov precision
	 * marRec is Morkov recall,
	 * %transitions is the %% of transitions in the reference automata covered by walk from which we are learning.
	 * Time (should always be the last one) refers to the wall clock taken by learner to learn.
	 */
	public String[] headerValuesForEachCell() 
	{
		List<String> headers = new LinkedList<>(Arrays.asList("Success","BCR","Diff","Invalid R","Missed R","Invalid Far","Missed Far","Valid mergers","States","I_Ref", "I_Lnt"));
		if (learnerToUse.isMarkov())
			headers.addAll(Arrays.asList("dI_Ave","dI_SD","alwaysPos","fracS","marTPre","marTRec","marHPre","marHRec","accRef","relIncLrnt","accLrnt","Comparisons"));
		if (markovParameters.useCentreVertex)
			headers.addAll(Arrays.asList("centreCorrect","centerpaths"));
		headers.addAll(Arrays.asList("alphabet","densityRef","densityLrnt","%transitions","Time"));
		return headers.toArray(new String[]{});
	}

	@Override
	public int executionTimeInCell() 
	{// here time is always the last value.
		return headerValuesForEachCell().length-1;
	}
}
