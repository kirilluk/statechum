package org.junit.runners;

import java.lang.annotation.Annotation;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.junit.internal.builders.AllDefaultPossibilitiesBuilder;
import org.junit.runner.Description;
import org.junit.runner.Runner;
import org.junit.runner.notification.RunNotifier;
import org.junit.runners.model.InitializationError;
import org.junit.runners.model.RunnerBuilder;


/** Licensed under CPL since it is almost verbatim from JUnit 4.8.1.
 *
 * This is a modified {@link Parameterized} in order to run a suite with parameters. The idea is to use one or more test methods to set global parameters and then run the test suite via {@link Suite}.
 *  
 * @author kirill
 *
 */
public class ParameterizedSuite extends Suite {
	/**
	 * Annotation for a method which provides parameters to be injected into the
	 * test class constructor by <code>Parameterized</code>
	 */
	@Retention(RetentionPolicy.RUNTIME)
	@Target(ElementType.METHOD)
	public static @interface ParametersToString {
	}
	

		private final ArrayList<Runner> runners= new ArrayList<Runner>();

		
		protected static class SuiteWithStringInName extends Suite
		{
			private static Class<?>[] getAnnotatedClasses(Class<?> klass) throws InitializationError {
				SuiteClasses annotation= klass.getAnnotation(SuiteClasses.class);
				if (annotation == null)
					throw new InitializationError(String.format("class '%s' must have a SuiteClasses annotation", klass.getName()));
				return annotation.value();
			}

			protected static List<Runner> renameRunners(List<Runner> runners, final String description)
			{
				ArrayList<Runner> outcome = new ArrayList<Runner>(runners.size());
				for(final Runner r:runners)
					outcome.add(new Runner(){
						
						@Override
						public Description getDescription() {
							Description origDescr = r.getDescription();
							Description modifiedDescription=Description.createSuiteDescription("("+origDescr.getClassName()+")"+description, origDescr.getAnnotations().toArray(new Annotation[0]));
							for(Description child:origDescr.getChildren())
								modifiedDescription.addChild(child);
							return modifiedDescription;
						}

						@Override
						public void run(RunNotifier notifier) {
							r.run(notifier);
						}});
				return outcome;
			}
			
			final String description;
			public SuiteWithStringInName(Class<?> klass, RunnerBuilder builder, String descr) throws InitializationError 
			{
				super(klass, renameRunners(builder.runners(klass, getAnnotatedClasses(klass)),descr));description = descr;
			}

			@Override
			public String getName()
			{
				return getTestClass().getName()+" "+description;
			}
		}
		
		protected static class SuiteRunnerWithParameters extends SuiteWithStringInName
		{
			final Object seriesInitialiser;
			final Method methodToCall;
			
			/** Used to distinguish between the first and non-first invocation of {@link SuiteRunnerWithParameters#runChild}. */
			private boolean initCompleted = false;
			
			public SuiteRunnerWithParameters(Class<?> klass, RunnerBuilder builder, String descr,Object seriesInit) throws InitializationError {
				super(klass,builder,descr);seriesInitialiser = seriesInit;
				Method method = null;
				try {
					method  = seriesInitialiser.getClass().getMethod("initSeries", new Class[0]);
				} catch (Exception e) {
					// cannot do this, explain why
					System.out.println("Was looking for a method initSeries()");
					for(Method constr:seriesInitialiser.getClass().getMethods())
						System.out.println("\t"+constr);
				}
				if (method == null || java.lang.reflect.Modifier.isStatic(method.getModifiers()))
					throw new IllegalArgumentException("failed to find initSeries()");
					//result = converterMethod.invoke(null, parameters).toString();
				methodToCall = method;
			}

			@Override
			protected void runChild(Runner runner, final RunNotifier notifier) 
			{
				if (!initCompleted)
				{
					System.out.println("STARTED "+description);
					initCompleted = true;
					try {
						methodToCall.invoke(seriesInitialiser, new Object[0]);
					} catch (IllegalAccessException e) {
						e.printStackTrace();
					} catch (IllegalArgumentException e) {
						e.printStackTrace();
					} catch (InvocationTargetException e) {
						e.printStackTrace();
					}
				}
				super.runChild(runner, notifier);
			}
		}
		
		/**
		 * Only called reflectively. Do not use programmatically.
		 */
		public ParameterizedSuite(Class<?> klass) throws Throwable 
		{
			super(klass, Collections.<Runner>emptyList());

			RunnerBuilder builder = new AllDefaultPossibilitiesBuilder(true);
			List<Object[]> parametersList= ParameterizedWithName.getParametersList(getTestClass());
			
			
			
			for (int i= 0; i < parametersList.size(); i++)
			{
				Object [] parameters = null;
				try {
					parameters = parametersList.get(i);
				} catch (ClassCastException e) {
					throw new Exception(String.format("%s.%s() must return a Collection of arrays.",getTestClass().getName(), ParameterizedWithName.getParametersMethod(getTestClass()).getName()));
				}
				
				
				String parameterDescr = ParameterizedWithName.TestClassRunnerForParameters.obtainStringDescription(getTestClass().getJavaClass(),parameters);
				Suite testSuite = new SuiteRunnerWithParameters(getTestClass().getJavaClass(), builder, parameterDescr,getTestClass().getOnlyConstructor().newInstance(parameters));
				runners.add(testSuite);
				//List<Runner> runnersToAdd = new org.junit.runners.Suite(klass,builder).getChildren();
				//runnerForParameters.addTestRunner(runnersToAdd);
			}
		}

		@Override
		protected List<Runner> getChildren() {
			return runners;
		}


}
