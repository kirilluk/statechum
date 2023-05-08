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


import java.util.Map;
import junit.framework.TestCase;

public class Support_MapTest2 extends TestCase {

	Map<String, String> map;

	public Support_MapTest2(Map<String, String> m) {
		super();
		map = m;
		if (!map.isEmpty()) {
			fail("Map must be empty");
		}
	}

	@Override
    public void runTest() {
		try {
			map.put("one", "1");
			assertEquals("size should be one", 1, map.size());
			map.clear();
			assertEquals("size should be zero", 0, map.size());
			assertFalse("Should not have entries", map.entrySet().iterator()
					.hasNext());
			assertFalse("Should not have keys", map.keySet().iterator()
					.hasNext());
			assertFalse("Should not have values", map.values().iterator()
					.hasNext());
		} catch (UnsupportedOperationException ignored) {
		}

		try {
			map.put("one", "1");
			assertEquals("size should be one", 1, map.size());
			map.remove("one");
			assertEquals("size should be zero", 0, map.size());
			assertFalse("Should not have entries", map.entrySet().iterator()
					.hasNext());
			assertFalse("Should not have keys", map.keySet().iterator()
					.hasNext());
			assertFalse("Should not have values", map.values().iterator()
					.hasNext());
		} catch (UnsupportedOperationException ignored) {
		}
	}

}
