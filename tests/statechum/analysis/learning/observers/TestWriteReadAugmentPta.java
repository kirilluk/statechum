/* Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
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
 */ 

package statechum.analysis.learning.observers;

import static statechum.Helper.checkForCorrectException;
import static statechum.analysis.learning.observers.TestRecordProgressDecorator.removeTagFromString;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.w3c.dom.Element;

import statechum.Configuration;
import statechum.JUConstants;
import statechum.Label;
import statechum.StatechumXML;
import statechum.Helper.whatToRun;
import statechum.analysis.learning.Learner.RestartLearningEnum;
import static statechum.analysis.learning.rpnicore.FsmParserStatechum.buildLearnerGraph;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;

/**
 * @author kirill
 *
 */
public class TestWriteReadAugmentPta 
{
	LearnerGraph graph = null;
	List<Label> sequence = null;
	String xmlData = null;
	
	private final Configuration config = Configuration.getDefaultConfiguration().copy();
	private final ConvertALabel converter = null;

	@Before
	public final void beforeTest()
	{
		graph = buildLearnerGraph("A-a->B-a->C", "testWritePairs1",config,converter);
		sequence = AbstractLearnerGraph.buildList(Arrays.asList(new String[]{"p","q","r"}),graph.config,converter);
		ByteArrayOutputStream output = new ByteArrayOutputStream();
		RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,config,false);
		dumper.topElement.appendChild(dumper.writeAugmentPTA(
				new ProgressDecorator.AugmentPTAData(RestartLearningEnum.restartSOFT,sequence,true,JUConstants.BLUE)));dumper.close();
		
		xmlData = output.toString();
	}
	
	/** Normal sequence. */
	@Test
	public final void testWriteAugment0()
	{
		LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(xmlData.getBytes()),false,converter);
		loader.config = config;
		loader.initIO(loader.doc, loader.config);
		ProgressDecorator.AugmentPTAData data = loader.readAugmentPTA(loader.expectNextElement(StatechumXML.ELEM_AUGMENTPTA.name()));
		ProgressDecorator.AugmentPTAData expected = new ProgressDecorator.AugmentPTAData(
				RestartLearningEnum.restartSOFT,sequence,true,JUConstants.BLUE);
		Assert.assertEquals(expected,data);
	}
	
	/** Extra attributes are ignored. */
	@Test
	public final void testWriteAugment1()
	{
		LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(
				TestRecordProgressDecorator.addExtraAttribute(xmlData,StatechumXML.ELEM_AUGMENTPTA).getBytes()),false,converter);
		loader.config = config;
		loader.initIO(loader.doc, loader.config);
		ProgressDecorator.AugmentPTAData data = loader.readAugmentPTA(loader.expectNextElement(StatechumXML.ELEM_AUGMENTPTA.name()));
		ProgressDecorator.AugmentPTAData expected = new ProgressDecorator.AugmentPTAData(
				RestartLearningEnum.restartSOFT,sequence,true,JUConstants.BLUE);
		Assert.assertEquals(expected,data);
	}
	
	/** Empty sequence. */
	@Test
	public final void testWriteAugment2a()
	{
		ByteArrayOutputStream output = new ByteArrayOutputStream();
		RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,config,false);
		dumper.topElement.appendChild(dumper.writeAugmentPTA(
				new ProgressDecorator.AugmentPTAData(RestartLearningEnum.restartHARD,new LinkedList<Label>(),false,JUConstants.RED)));dumper.close();
		
		xmlData = output.toString();

		LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(xmlData.getBytes()),false,converter);
		loader.config = config;
		loader.initIO(loader.doc, loader.config);
		ProgressDecorator.AugmentPTAData data = loader.readAugmentPTA(loader.expectNextElement(StatechumXML.ELEM_AUGMENTPTA.name()));
		ProgressDecorator.AugmentPTAData expected = new ProgressDecorator.AugmentPTAData(
				RestartLearningEnum.restartHARD,new LinkedList<Label>(),false,JUConstants.RED);
		Assert.assertEquals(expected,data);
	}
	
	/** Null colour. */
	@Test
	public final void testWriteAugment2b()
	{
		ByteArrayOutputStream output = new ByteArrayOutputStream();
		RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,config,false);
		dumper.topElement.appendChild(dumper.writeAugmentPTA(
				new ProgressDecorator.AugmentPTAData(RestartLearningEnum.restartHARD,sequence,false,null)));dumper.close();
		
		xmlData = output.toString();

		LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(xmlData.getBytes()),false,converter);
		loader.config = config;
		loader.initIO(loader.doc, loader.config);
		ProgressDecorator.AugmentPTAData data = loader.readAugmentPTA(loader.expectNextElement(StatechumXML.ELEM_AUGMENTPTA.name()));
		ProgressDecorator.AugmentPTAData expected = new ProgressDecorator.AugmentPTAData(
				RestartLearningEnum.restartHARD,sequence,false,null);
		Assert.assertEquals(expected,data);
	}
	
	/** Missing main tag. */
	@Test
	public final void testWriteAugment3()
	{
		final LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(removeTagFromString(xmlData,StatechumXML.ELEM_AUGMENTPTA).getBytes()),false,converter);
		loader.config = config;
		loader.initIO(loader.doc, loader.config);
		checkForCorrectException(new whatToRun() { public @Override void run() {
			loader.readAugmentPTA(loader.expectNextElement(statechum.analysis.learning.observers.TestRecordProgressDecorator.junkTag));
		}},IllegalArgumentException.class,"cannot load augmentPTA data");
	}
	
	/** Missing sequence. */
	@Test
	public final void testWriteAugment4()
	{
		ByteArrayOutputStream output = new ByteArrayOutputStream();
		RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,config,false);
		Element augmentElement = dumper.writeAugmentPTA(
				new ProgressDecorator.AugmentPTAData(RestartLearningEnum.restartSOFT,sequence,true,JUConstants.BLUE));
		augmentElement.setTextContent("");
		dumper.topElement.appendChild(augmentElement);dumper.close();
		xmlData = output.toString();

		final LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(xmlData.getBytes()),false,converter);
		loader.config = config;
		loader.initIO(loader.doc, loader.config);
		checkForCorrectException(new whatToRun() { public @Override void run() {
			loader.readAugmentPTA(loader.expectNextElement(StatechumXML.ELEM_AUGMENTPTA.name()));
		}},IllegalArgumentException.class,"missing sequence");
	}
	
	/** Missing colour. */
	@Test
	public final void testWriteAugment5()
	{
		ByteArrayOutputStream output = new ByteArrayOutputStream();
		RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,config,false);
		Element augmentElement = dumper.writeAugmentPTA(
				new ProgressDecorator.AugmentPTAData(RestartLearningEnum.restartSOFT,sequence,true,JUConstants.BLUE));
		augmentElement.removeAttribute(StatechumXML.ATTR_COLOUR.name());
		dumper.topElement.appendChild(augmentElement);dumper.close();
		xmlData = output.toString();

		final LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(xmlData.getBytes()),false,converter);
		loader.config = config;
		loader.initIO(loader.doc, loader.config);
		ProgressDecorator.AugmentPTAData data = loader.readAugmentPTA(loader.expectNextElement(StatechumXML.ELEM_AUGMENTPTA.name()));
		ProgressDecorator.AugmentPTAData expected = new ProgressDecorator.AugmentPTAData(
				RestartLearningEnum.restartSOFT,sequence,true,null);
		Assert.assertEquals(expected,data);
	}
	
	/** Missing accept. */
	@Test
	public final void testWriteAugment6()
	{
		ByteArrayOutputStream output = new ByteArrayOutputStream();
		RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,config,false);
		Element augmentElement = dumper.writeAugmentPTA(
				new ProgressDecorator.AugmentPTAData(RestartLearningEnum.restartSOFT,sequence,true,JUConstants.BLUE));
		augmentElement.removeAttribute(StatechumXML.ATTR_ACCEPT.name());
		dumper.topElement.appendChild(augmentElement);dumper.close();
		xmlData = output.toString();

		final LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(xmlData.getBytes()),false,converter);
		loader.config = config;
		loader.initIO(loader.doc, loader.config);
		checkForCorrectException(new whatToRun() { public @Override void run() {
			loader.readAugmentPTA(loader.expectNextElement(StatechumXML.ELEM_AUGMENTPTA.name()));
		}},IllegalArgumentException.class,"missing accept");
	}
	
	/** Missing kind. */
	@Test
	public final void testWriteAugment7()
	{
		ByteArrayOutputStream output = new ByteArrayOutputStream();
		RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,config,false);
		Element augmentElement = dumper.writeAugmentPTA(
				new ProgressDecorator.AugmentPTAData(RestartLearningEnum.restartSOFT,sequence,true,JUConstants.BLUE));
		augmentElement.removeAttribute(StatechumXML.ATTR_KIND.name());
		dumper.topElement.appendChild(augmentElement);dumper.close();
		xmlData = output.toString();

		final LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(xmlData.getBytes()),false,converter);
		loader.config = config;
		loader.initIO(loader.doc, loader.config);
		checkForCorrectException(new whatToRun() { public @Override void run() {
			loader.readAugmentPTA(loader.expectNextElement(StatechumXML.ELEM_AUGMENTPTA.name()));
		}},IllegalArgumentException.class,"missing kind");
	}
	
}
