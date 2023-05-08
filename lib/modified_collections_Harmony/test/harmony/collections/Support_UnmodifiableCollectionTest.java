/* 
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

// The origin of this file is Apache Harmony SVN repository,
// location: classlib/modules/luni/src/main/java/java/util
// checked out Dec 3, 2022.
package harmony.collections;

import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.SortedSet;
import java.util.TreeSet;
import junit.framework.TestCase;

public class Support_UnmodifiableCollectionTest extends TestCase {

	Collection<Integer> col;

	// must be a collection containing the Integers 0 to 99 (which will iterate
	// in order)

	public Support_UnmodifiableCollectionTest(String p1) {
		super(p1);
	}

	public Support_UnmodifiableCollectionTest(String p1, Collection<Integer> c) {
		super(p1);
		col = c;
	}

	@Override
    public void runTest() {

		// contains
		assertTrue("UnmodifiableCollectionTest - should contain 0", col
				.contains(0));
		assertTrue("UnmodifiableCollectionTest - should contain 50", col
				.contains(50));
		assertFalse("UnmodifiableCollectionTest - should not contain 100", col
				.contains(100));

		// containsAll
		HashSet<Integer> hs = new HashSet<>();
		hs.add(0);
		hs.add(25);
		hs.add(99);
		assertTrue(
				"UnmodifiableCollectionTest - should contain set of 0, 25, and 99",
				col.containsAll(hs));
		hs.add(100);
		assertFalse("UnmodifiableCollectionTest - should not contain set of 0, 25, 99 and 100", col.containsAll(hs));

		// isEmpty
		assertFalse("UnmodifiableCollectionTest - should not be empty", col
				.isEmpty());

		// iterator
		Iterator<Integer> it = col.iterator();
		SortedSet<Integer> ss = new TreeSet<>();
		while (it.hasNext()) {
			ss.add(it.next());
		}
		it = ss.iterator();
		for (int counter = 0; it.hasNext(); counter++) {
			int nextValue = it.next().intValue();
			assertEquals("UnmodifiableCollectionTest - Iterator returned wrong value.  Wanted: "
					+ counter + " got: " + nextValue, nextValue, counter);
		}

		// size
		assertEquals("UnmodifiableCollectionTest - returned wrong size.  Wanted 100, got: "
				+ col.size(), 100, col.size());

		// toArray
		Object[] objArray;
		objArray = col.toArray();
		for (int counter = 0; it.hasNext(); counter++) {
			assertSame("UnmodifiableCollectionTest - toArray returned incorrect array", objArray[counter], it.next());
		}

		// toArray (Object[])
		objArray = new Object[100];
		col.toArray(objArray);
		for (int counter = 0; it.hasNext(); counter++) {
			assertSame("UnmodifiableCollectionTest - toArray(Object) filled array incorrectly", objArray[counter], it.next());
		}

	}

}
