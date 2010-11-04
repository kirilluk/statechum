/* Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 * 
 * This file is part of StateChum
 * 
 * StateChum is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * 
 * StateChum is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
 * A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along with
 * StateChum. If not, see <http://www.gnu.org/licenses/>.
 */ 

package statechum;

import static statechum.Helper.throwUnchecked;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.LinkedList;
import java.util.List;

import junit.framework.Assert;
import statechum.Configuration.GDScoreComputationEnum;
import statechum.Configuration.IDMode;
import statechum.Configuration.LEARNER;
import statechum.Configuration.QuestionGeneratorKind;
import statechum.Configuration.SMTGRAPHDOMAINCONSISTENCYCHECK;
import statechum.Configuration.SMTGRAPHRANGECONSISTENCYCHECK;
import statechum.Configuration.ScoreMode;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.analysis.learning.rpnicore.GDLearnerGraph;

public class Test_AttributeMutator {
	/** Makes it possible to construct mutators by reflection in order to test that 
	 * methods of a specific object look at all the attributes.
	 *  
	 * @author kirill
	 */
	public static final class MethodAndArgs<T> 
	{
		MethodAndArgs(Method m, Field f, Object a, Object b)
		{
			method=m;Arg=a;AlternativeArg=b;field = f;
		}
		private Field field;
		private Method method;Object Arg, AlternativeArg;

		/** Returns the name of the field mutated by this instance of MethodAndArgs. */
		public String getField()
		{
			return field.toString();
		}
		
		/** Mutates a field of the supplied object. */
		public void assignA(T object)
		{
			try
			{
				method.invoke(object, new Object[]{Arg});
			} catch (Exception e) 
			{
				e.printStackTrace();
				Assert.fail(e.getMessage());
			}
		}
		
		/** Mutates a field of the supplied object. */
		public void assignB(T object)
		{
			try
			{
				method.invoke(object, new Object[]{AlternativeArg});
			} catch (Exception e) 
			{
				e.printStackTrace();
				Assert.fail(e.getMessage());
			}
		}
	}
	
	/** Whether a method is get.../is ..., or set...  */
	public enum GETMETHOD_KIND { FIELD_GET, FIELD_SET} 
	
	/** In order to serialise/deserialise data, we need access to fields and getter/setter methods.
	 * This method takes a field and returns the corresponding method. Although supposedly
	 * universal, this does not take bean properties into account, such as introspector,
	 * transient designation and others. 
	 * 
	 * @param classToConsider methods of which class to extract
	 * @param kind whether to extract a getter or a setter.
	 * @param var the field to consider
	 * @return method which permits a specific kind of access to the supplied variable in instances of the provided class.
	 */
	public static <T> Method getMethod(Class<T> classToConsider,GETMETHOD_KIND kind,java.lang.reflect.Field var)
	{
		String varName = var.getName();
		 
		String methodNameSuffix = (Character.toUpperCase(varName.charAt(0)))+varName.substring(1);
		String methodName = ((kind == GETMETHOD_KIND.FIELD_GET)?"get":"set")+methodNameSuffix;
		Method method = null;
		try {
			method = classToConsider.getMethod(methodName, 
					(kind == GETMETHOD_KIND.FIELD_GET)?new Class[]{}:new Class[]{var.getType()});
		} catch (SecurityException e) {
			throwUnchecked("security exception on method "+kind+" for variable "+var.getName(), e);
		} catch (NoSuchMethodException e) {
			if (kind == GETMETHOD_KIND.FIELD_SET) throwUnchecked("failed to extract method "+kind+" for variable "+var.getName(), e);

			// ignore if looking for a getter - method is null indicates we'll try again.
		}
		
		if (method == null) // not found, try another one.
			try {
				methodName = ((kind == GETMETHOD_KIND.FIELD_GET)?"is":"set")+methodNameSuffix;
				method = classToConsider.getMethod(methodName, 
						(kind == GETMETHOD_KIND.FIELD_GET)?new Class[]{}:new Class[]{var.getType()});
			} catch (Exception e) {
				throwUnchecked("failed to extract method "+kind+" for variable "+var.getName(), e);
			}		return method;
	}
	
	/** Takes a class and builds a collection of methods such that each element in that collection
	 * can mutate a different attribute of an instance of the supplied class.
	 * 
	 * @param clazz the class to process
	 * @return the collection of attribute mutators.
	 */
	public static <T> List<MethodAndArgs<T>> constructArgList(Class<T> clazz)
	{
		List<MethodAndArgs<T>> MethodsArgs=new LinkedList<MethodAndArgs<T>>();
		for(Field var:clazz.getDeclaredFields())
		{
			if (var.getType() != clazz && 
					var.getName() != "$VRc"// added by eclemma (coverage analysis)
					&& !java.lang.reflect.Modifier.isFinal(var.getModifiers()))
			{
				String varName = var.getName();
				Method setter = getMethod(clazz,GETMETHOD_KIND.FIELD_SET, var);
				Object valueA = null, valueB = null;
				if (var.getType().equals(Boolean.class) || var.getType().equals(boolean.class))
				{
					valueA = new Boolean(true);valueB=new Boolean(false);
				}
				else
				if (var.getType().equals(Double.class) || var.getType().equals(double.class))
				{
					valueA = new Double(0.4);valueB=new Double(0.5);// note that we have to choose values which fall within the allowed range of values
				}
				else
				if (var.getType().equals(String.class))
				{
					valueA = varName+", value A";valueB=varName+", value B";
				}
				else
				if (var.getType().equals(IDMode.class))
				{
					valueA = IDMode.POSITIVE_NEGATIVE;valueB=IDMode.POSITIVE_ONLY;
				}
				else
				if (var.getType().equals(ScoreMode.class))
				{
					valueA = ScoreMode.KTAILS;valueB=ScoreMode.COMPATIBILITY;
				}
				else
				if (var.getType().equals(QuestionGeneratorKind.class))
				{
						valueA = QuestionGeneratorKind.CONVENTIONAL;valueB=QuestionGeneratorKind.SYMMETRIC;
				}
				else
				if (var.getType().equals(SMTGRAPHDOMAINCONSISTENCYCHECK.class))
				{
						valueA = SMTGRAPHDOMAINCONSISTENCYCHECK.DETERMINISM;valueB=SMTGRAPHDOMAINCONSISTENCYCHECK.TRANSITIONSFROMALLORNONE;
				}
				else
				if (var.getType().equals(SMTGRAPHRANGECONSISTENCYCHECK.class))
				{
						valueA = SMTGRAPHRANGECONSISTENCYCHECK.RANGECONTAINMENT;valueB=SMTGRAPHRANGECONSISTENCYCHECK.RANGEINTERSECTION;
				}
				else
				if (var.getType().equals(LEARNER.class))
				{
						valueA = LEARNER.LEARNER_BLUEFRINGE_MAY2008;valueB=LEARNER.LEARNER_BLUEAMBER_MAY2008;
				}
				else
				if (var.getType().equals(GDScoreComputationEnum.class))
				{
						valueA = GDScoreComputationEnum.GD_BCR;valueB=GDScoreComputationEnum.GD_LINEAR_RH_OUTGOINGINCOMING;
				}
				else
				if (var.getType().equals(Integer.class) || var.getType().equals(int.class))
				{
					valueA = varName.hashCode();valueB=setter.hashCode();// just some integers likely to be different from each other between different variables.
				}
				else
				if (var.getType().equals(VertexID.class))
				{
					valueA = new VertexID("A");valueB = new VertexID("B");
				}
				else
				if (var.getType().equals(JUConstants.class))
				{
					valueA = JUConstants.RED;valueB = JUConstants.BLUE;
				}
				else
					throw new IllegalArgumentException("A field "+var+" of "+clazz+" has an unsupported type "+var.getType());
				
				MethodsArgs.add(new MethodAndArgs<T>(setter,var,valueA,valueB));
			}
		}
		
		return MethodsArgs;
	}
}
