/*
 *  Licensed to the Apache Software Foundation (ASF) under one or more
 *  contributor license agreements.  See the NOTICE file distributed with
 *  this work for additional information regarding copyright ownership.
 *  The ASF licenses this file to You under the Apache License, Version 2.0
 *  (the "License"); you may not use this file except in compliance with
 *  the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
// The origin of this file is Apache Harmony SVN repository,
// location: classlib/modules/luni/src/main/java/java/util
// checked out Dec 3, 2022.
package harmony.collections;

import java.util.*;


public class HarmonyHarmonyAbstractMapTest extends junit.framework.TestCase {

    static final String specialKey = "specialKey".intern();

    static final String specialValue = "specialValue".intern();

    // The impl of MyMap is not realistic, but serves to create a type
    // that uses the default remove behavior.
    class MyMap extends HarmonyAbstractMap {
        final Set mySet = new HashSet(1);

        MyMap() {
            mySet.add(new Map.Entry() {
                public Object getKey() {
                    return specialKey;
                }

                public Object getValue() {
                    return specialValue;
                }

                public Object setValue(Object object) {
                    return null;
                }
            });
        }

        public Object put(Object key, Object value) {
            return null;
        }

        public Set entrySet() {
            return mySet;
        }
    }

    /**
     * @tests java.util.HarmonyAbstractMap#keySet()
     */
    public void test_keySet() {
        HarmonyAbstractMap map1 = new HarmonyHashMap(0);
        assertSame("HarmonyHashMap(0)", map1.keySet(), map1.keySet());

        HarmonyAbstractMap map2 = new HarmonyHashMap(10);
        assertSame("HarmonyHashMap(10)", map2.keySet(), map2.keySet());

        Map map3 = Collections.EMPTY_MAP;
        assertSame("EMPTY_MAP", map3.keySet(), map3.keySet());

        HarmonyAbstractMap map5 = new HashMapWithSearch(122);
        assertSame("LinkedHarmonyHashMap", map5.keySet(), map5.keySet());

        HarmonyAbstractMap map6 = new TreeMapWithSearch();
        assertSame("TreeMapWithSearch", map6.keySet(), map6.keySet());

    }

    /**
     * @tests java.util.HarmonyAbstractMap#remove(java.lang.Object)
     */
    public void test_removeLjava_lang_Object() {
        Object key = new Object();
        Object value = new Object();

        HarmonyAbstractMap map1 = new HarmonyHashMap(0);
        map1.put("key", value);
        assertSame("HarmonyHashMap(0)", map1.remove("key"), value);

        HarmonyAbstractMap map5 = new HashMapWithSearch(122);
        map5.put(key, value);
        assertSame("LinkedHarmonyHashMap", map5.remove(key), value);

        HarmonyAbstractMap map6 = new TreeMapWithSearch(new Comparator() {
            // Bogus comparator
            public int compare(Object object1, Object object2) {
                return 0;
            }
        });
        map6.put(key, value);
        assertSame("TreeMapWithSearch", map6.remove(key), value);

        HarmonyAbstractMap aSpecialMap = new MyMap();
        aSpecialMap.put(specialKey, specialValue);
        Object valueOut = aSpecialMap.remove(specialKey);
        assertSame("MyMap", valueOut, specialValue);
    }

    /**
     * @tests java.util.HarmonyAbstractMap#clear()
     */
    public void test_clear() {
        // normal clear()
        HarmonyAbstractMap map = new HarmonyHashMap();
        map.put(1, 1);
        map.clear();
        assertTrue(map.isEmpty());

        // Special entrySet return a Set with no clear method.
        HarmonyAbstractMap myMap = new MocHarmonyAbstractMap();
        try {
            myMap.clear();
            fail("Should throw UnsupportedOprationException");
        } catch (UnsupportedOperationException e) {
            // expected
        }
    }

    class MocHarmonyAbstractMap<K, V> extends HarmonyAbstractMap {

        public Set entrySet() {
            Set set = new MySet();
            return set;
        }

        class MySet extends HashSet {
            public void clear() {
                throw new UnsupportedOperationException();
            }
        }
    }

    /**
     * @tests java.util.HarmonyAbstractMap#containsKey(Object)
     */
    public void test_containsKey() {
        HarmonyAbstractMap map = new AMT();

        assertFalse(map.containsKey("k"));
        assertFalse(map.containsKey(null));

        map.put("k", "v");
        map.put("key", null);
        map.put(null, "value");
        map.put(null, null);

        assertTrue(map.containsKey("k"));
        assertTrue(map.containsKey("key"));
        assertTrue(map.containsKey(null));
    }

    /**
     * @tests java.util.HarmonyAbstractMap#containsValue(Object)
     */
    public void test_containValue() {
        HarmonyAbstractMap map = new AMT();

        assertFalse(map.containsValue("v"));
        assertFalse(map.containsValue(null));

        map.put("k", "v");
        map.put("key", null);
        map.put(null, "value");

        assertTrue(map.containsValue("v"));
        assertTrue(map.containsValue("value"));
        assertTrue(map.containsValue(null));
    }

    /**
     * @tests java.util.HarmonyAbstractMap#get(Object)
     */
    public void test_get() {
        HarmonyAbstractMap map = new AMT();
        assertNull(map.get("key"));
        assertNull(map.get(null));

        map.put("k", "v");
        map.put("key", null);
        map.put(null, "value");

        assertEquals("v", map.get("k"));
        assertNull(map.get("key"));
        assertEquals("value", map.get(null));
    }

    /**
     * @tests java.util.HarmonyAbstractMap#values()
     */
    public void test_values() {
        HarmonyAbstractMap map1 = new HarmonyHashMap(0);
        assertSame("HarmonyHashMap(0)", map1.values(), map1.values());

        HarmonyAbstractMap map2 = new HarmonyHashMap(10);
        assertSame("HarmonyHashMap(10)", map2.values(), map2.values());

        Map map3 = Collections.EMPTY_MAP;
        assertSame("EMPTY_MAP", map3.values(), map3.values());

        HarmonyAbstractMap map5 = new HashMapWithSearch(122);
        assertSame("IdentityHarmonyHashMap", map5.values(), map5.values());

        HarmonyAbstractMap map6 = new TreeMapWithSearch();
        assertSame("TreeMapWithSearch", map6.values(), map6.values());

    }

    /**
     * @tests java.util.HarmonyAbstractMap#clone()
     */
    public void test_clone() {
        class MyMap extends HarmonyAbstractMap implements Cloneable {
            private Map map = new HarmonyHashMap();

            public Set entrySet() {
                return map.entrySet();
            }

            public Object put(Object key, Object value) {
                return map.put(key, value);
            }

            public Map getMap() {
                return map;
            }

            public Object clone() {
                try {
                    return super.clone();
                } catch (CloneNotSupportedException e) {
                    return null;
                }
            }
        }
        ;
        MyMap map = new MyMap();
        map.put("one", "1");
        Map.Entry entry = (Map.Entry) map.entrySet().iterator().next();
        assertTrue("entry not added", entry.getKey() == "one"
                && entry.getValue() == "1");
        MyMap mapClone = (MyMap) map.clone();
        assertTrue("clone not shallow", map.getMap() == mapClone.getMap());
    }

    public class AMT extends HarmonyAbstractMap {

        // Very crude HarmonyAbstractMap implementation
        Vector values = new Vector();

        Vector keys = new Vector();

        public Set entrySet() {
            return new AbstractSet() {
                public Iterator iterator() {
                    return new Iterator() {
                        int index = 0;

                        public boolean hasNext() {
                            return index < values.size();
                        }

                        public Object next() {
                            if (index < values.size()) {
                                Map.Entry me = new Map.Entry() {
                                    Object v = values.elementAt(index);

                                    Object k = keys.elementAt(index);

                                    public Object getKey() {
                                        return k;
                                    }

                                    public Object getValue() {
                                        return v;
                                    }

                                    public Object setValue(Object value) {
                                        return null;
                                    }
                                };
                                index++;
                                return me;
                            }
                            return null;
                        }

                        public void remove() {
                        }
                    };
                }

                public int size() {
                    return values.size();
                }
            };
        }

        public Object put(Object k, Object v) {
            keys.add(k);
            values.add(v);
            return v;
        }
    }

    /**
     * @tests {@link HarmonyAbstractMap#putAll(Map)}
     */
    public void test_putAllLMap() {
        Hashtable ht = new Hashtable();
        AMT amt = new AMT();
        ht.put("this", "that");
        amt.putAll(ht);
        assertEquals("Should be equal", amt, ht);
    }

    public void testEqualsWithNullValues() {
        Map<String, String> a = new HarmonyHashMap<String, String>();
        a.put("a", null);
        a.put("b", null);

        Map<String, String> b = new HarmonyHashMap<String, String>();
        a.put("c", "cat");
        a.put("d", "dog");

        assertFalse(a.equals(b));
        assertFalse(b.equals(a));
    }

    public void testNullsOnViews() {
        Map<String, String> nullHostile = new Hashtable<String, String>();

        nullHostile.put("a", "apple");
        testNullsOnView(nullHostile.entrySet());

        nullHostile.put("a", "apple");
        testNullsOnView(nullHostile.keySet());

        nullHostile.put("a", "apple");
        testNullsOnView(nullHostile.values());
    }

    private void testNullsOnView(Collection<?> view) {
        try {
            assertFalse(view.contains(null));
        } catch (NullPointerException optional) {
        }

        try {
            assertFalse(view.remove(null));
        } catch (NullPointerException optional) {
        }

        Set<Object> setOfNull = Collections.singleton(null);
        assertFalse(view.equals(setOfNull));

        try {
            assertFalse(view.removeAll(setOfNull));
        } catch (NullPointerException optional) {
        }

        try {
            assertTrue(view.retainAll(setOfNull)); // destructive
        } catch (NullPointerException optional) {
        }
    }

    protected void setUp() {
    }

    protected void tearDown() {
    }
}
