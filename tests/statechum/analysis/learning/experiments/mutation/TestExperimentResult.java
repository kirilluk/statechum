package statechum.analysis.learning.experiments.mutation;

import static statechum.TestHelper.checkForCorrectException;

import org.junit.Assert;
import org.junit.Test;

import statechum.Configuration;
import statechum.TestHelper.whatToRun;
import statechum.analysis.learning.experiments.mutation.ExperimentResult.DOUBLE_V;
import statechum.analysis.learning.experiments.mutation.ExperimentResult.LONG_V;

public class TestExperimentResult {

	@Test
	public void testExprimentResultFail1()
	{
		final ExperimentResult res = new ExperimentResult();res.experimentValid = true;
		checkForCorrectException(new whatToRun() { public @Override void run() {
			res.getValue(DOUBLE_V.ACCURACY_W);
		}},IllegalArgumentException.class,"has not previously");
	}
	@Test
	public void testExprimentResult1()
	{
		final ExperimentResult res = new ExperimentResult();res.experimentValid = true;
		res.setValue(DOUBLE_V.ACCURACY_RAND, 4.5);
		Assert.assertEquals(4.5, res.getValue(DOUBLE_V.ACCURACY_RAND),Configuration.fpAccuracy);
		
		checkForCorrectException(new whatToRun() { public @Override void run() {
			res.getValue(DOUBLE_V.ACCURACY_W);
		}},IllegalArgumentException.class,"has not previously");
	}
	
	@Test
	public void testExprimentResultFail2()
	{
		final ExperimentResult res = new ExperimentResult();res.experimentValid = true;
		checkForCorrectException(new whatToRun() { public @Override void run() {
			res.getValue(LONG_V.DURATION_W);
		}},IllegalArgumentException.class,"has not previously");
	}
	
	@Test
	public void testExprimentResult2()
	{
		final ExperimentResult res = new ExperimentResult();res.experimentValid = true;
		res.setValue(LONG_V.DURATION_RAND, 4);
		Assert.assertEquals(4, res.getValue(LONG_V.DURATION_RAND));
		
		checkForCorrectException(new whatToRun() { public @Override void run() {
			res.getValue(DOUBLE_V.ACCURACY_W);
		}},IllegalArgumentException.class,"has not previously");
	}
	
	/** Set multiple values and check them. */
	@Test
	public void testExperimentResult3()
	{
		final ExperimentResult res = new ExperimentResult();res.experimentValid = true;
		res.setValue(DOUBLE_V.ACCURACY_RAND, 4.5);
		Assert.assertEquals(4.5, res.getValue(DOUBLE_V.ACCURACY_RAND),Configuration.fpAccuracy);
		res.setValue(LONG_V.DURATION_RAND, 4);
		Assert.assertEquals(4, res.getValue(LONG_V.DURATION_RAND));
		Assert.assertEquals(4.5, res.getValue(DOUBLE_V.ACCURACY_RAND),Configuration.fpAccuracy);
	}
	
	/** Empty collections. */
	@Test
	public void testExperimentAddition1()
	{
		final ExperimentResult res = new ExperimentResult(), a = new ExperimentResult();
		res.experimentValid = true;a.experimentValid = true;
		res.add(a);
		Assert.assertTrue(res.D_varToValue.isEmpty());
		Assert.assertTrue(res.L_varToValue.isEmpty());

		res.add(res);
		Assert.assertTrue(res.D_varToValue.isEmpty());
		Assert.assertTrue(res.L_varToValue.isEmpty());
	}
	
	/** Empty collections. */
	@Test
	public void testExperimentAddition2()
	{
		final ExperimentResult res = new ExperimentResult();
		Assert.assertTrue(res.toString().contains("invalid"));
	}

	/** Empty collections. */
	@Test
	public void testExperimentAddition3()
	{
		final ExperimentResult res = new ExperimentResult();res.experimentValid = true;
		Assert.assertFalse(res.toString().contains("invalid"));
	}
	
	/** Tests addition. */
	@Test
	public void testExprimentResultAdditionFail1()
	{
		final ExperimentResult res = new ExperimentResult(), a = new ExperimentResult();
		res.experimentValid = true;a.experimentValid = true;
		a.setValue(DOUBLE_V.ACCURACY_RAND, 0);
		Assert.assertEquals(0,a.getValue(DOUBLE_V.ACCURACY_RAND),Configuration.fpAccuracy);
		checkForCorrectException(new whatToRun() { public @Override void run() {
			res.add(a);
		}},IllegalArgumentException.class,"result missing value");
	}
	/** Tests addition. */
	@Test
	public void testExprimentResultAdditionFail2()
	{
		final ExperimentResult res = new ExperimentResult(), a = new ExperimentResult();
		res.experimentValid = true;a.experimentValid = true;
		a.setValue(LONG_V.DURATION_GD, 0);
		Assert.assertEquals(0,a.getValue(LONG_V.DURATION_GD));
		checkForCorrectException(new whatToRun() { public @Override void run() {
			res.add(a);
		}},IllegalArgumentException.class,"result missing value");
	}
	/** Tests addition. */
	@Test
	public void testExprimentResultAdditionFail3()
	{
		final ExperimentResult res = new ExperimentResult(), a = new ExperimentResult();
		res.experimentValid = true;a.experimentValid = true;
		res.setValue(DOUBLE_V.ACCURACY_RAND, 0);
		Assert.assertEquals(0,res.getValue(DOUBLE_V.ACCURACY_RAND),Configuration.fpAccuracy);
		checkForCorrectException(new whatToRun() { public @Override void run() {
			res.add(a);
		}},IllegalArgumentException.class,"exp missing value");
	}
	
	/** Tests addition. */
	@Test
	public void testExprimentResultAdditionFail4()
	{
		final ExperimentResult res = new ExperimentResult(), a = new ExperimentResult();
		res.experimentValid = true;a.experimentValid = true;
		res.setValue(LONG_V.DURATION_RAND, 0);
		Assert.assertEquals(0,res.getValue(LONG_V.DURATION_RAND));
		checkForCorrectException(new whatToRun() { public @Override void run() {
			res.add(a);
		}},IllegalArgumentException.class,"exp missing value");
	}
	
	/** Tests addition. */
	@Test
	public void testExprimentResultAddition3()
	{
		final ExperimentResult res = new ExperimentResult(), a = new ExperimentResult();
		res.experimentValid = true;a.experimentValid = true;
		res.setValue(LONG_V.DURATION_RAND, 6);a.setValue(LONG_V.DURATION_RAND, 7);
		res.add(a);
		Assert.assertEquals(13,res.getValue(LONG_V.DURATION_RAND));
		Assert.assertEquals(7,a.getValue(LONG_V.DURATION_RAND));
	}
	
	/** Tests addition. */
	@Test
	public void testExprimentResultAddition4()
	{
		final ExperimentResult res = new ExperimentResult(), a = new ExperimentResult();
		res.experimentValid = true;a.experimentValid = true;
		res.setValue(DOUBLE_V.ACCURACY_RAND, 6.5);a.setValue(DOUBLE_V.ACCURACY_RAND, 7);
		res.add(a);
		Assert.assertEquals(13.5,res.getValue(DOUBLE_V.ACCURACY_RAND),Configuration.fpAccuracy);
		Assert.assertEquals(7,a.getValue(DOUBLE_V.ACCURACY_RAND),Configuration.fpAccuracy);
	}
	
	/** Tests addition. */
	@Test
	public void testExprimentResultFail5()
	{
		final ExperimentResult res = new ExperimentResult(), a = new ExperimentResult();
		res.experimentValid = true;a.experimentValid = true;
		res.setValue(DOUBLE_V.ACCURACY_RAND, 6.5);a.setValue(DOUBLE_V.ACCURACY_RAND, 7);
		
		res.experimentValid = false;
		checkForCorrectException(new whatToRun() { public @Override void run() {
			res.add(a);
		}},IllegalArgumentException.class,"invalid");
	}
	
	/** Tests addition. */
	@Test
	public void testExprimentResultFail6()
	{
		final ExperimentResult res = new ExperimentResult(), a = new ExperimentResult();
		res.experimentValid = true;a.experimentValid = true;
		res.setValue(DOUBLE_V.ACCURACY_RAND, 6.5);a.setValue(DOUBLE_V.ACCURACY_RAND, 7);
		
		a.experimentValid = false;
		checkForCorrectException(new whatToRun() { public @Override void run() {
			res.add(a);
		}},IllegalArgumentException.class,"invalid");
	}
	
	@Test
	public void testExprimentAverageFail1()
	{
		final ExperimentResult res = new ExperimentResult();res.experimentValid = true;
		res.setValue(LONG_V.DURATION_RAND, 4);
		Assert.assertEquals(4, res.getValue(LONG_V.DURATION_RAND));
		checkForCorrectException(new whatToRun() { public @Override void run() {
			res.divide(0);
		}},IllegalArgumentException.class,"cannot average");
	}

	@Test
	public void testExprimentAverageFail2()
	{
		final ExperimentResult res = new ExperimentResult();res.experimentValid = true;
		res.setValue(LONG_V.DURATION_RAND, 4);
		Assert.assertEquals(4, res.getValue(LONG_V.DURATION_RAND));
		checkForCorrectException(new whatToRun() { public @Override void run() {
			res.divide(-1);
		}},IllegalArgumentException.class,"cannot average");
	}

	@Test
	public void testExprimentAverageFail3()
	{
		final ExperimentResult res = new ExperimentResult();res.experimentValid = true;
		res.setValue(LONG_V.DURATION_RAND, 4);
		Assert.assertEquals(4, res.getValue(LONG_V.DURATION_RAND));
		
		res.experimentValid = false;
		checkForCorrectException(new whatToRun() { public @Override void run() {
			res.divide(1);
		}},IllegalArgumentException.class,"invalid");
	}

	@Test
	public void testExprimentAverage()
	{
		final ExperimentResult res = new ExperimentResult();res.experimentValid = true;
		res.setValue(LONG_V.DURATION_RAND, 4);
		res.setValue(DOUBLE_V.ACCURACY_RAND, 9);
		ExperimentResult ave = res.divide(2);
		Assert.assertEquals(4, res.getValue(LONG_V.DURATION_RAND));
		Assert.assertEquals(2, ave.getValue(LONG_V.DURATION_RAND));
		Assert.assertEquals(9, res.getValue(DOUBLE_V.ACCURACY_RAND),Configuration.fpAccuracy);
		Assert.assertEquals(4.5, ave.getValue(DOUBLE_V.ACCURACY_RAND),Configuration.fpAccuracy);
	}
}
