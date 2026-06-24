/* Copyright (c) 2011 The University of Sheffield.
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
 * 
 */
package statechum;

import java.util.Objects;

public class StringLabelInt extends StringLabel
{
   /** The unique identifier of this label, made public because it is final. */
   public final int number;

   @Override
   public int toInt()
   {
	   return number;
   }

   public StringLabelInt(String l, int num)
   {
	   super(l);number=num;
   }

	@Override
	public boolean equals(Object o) {
		if (o == null) return false;
		if (getClass() != o.getClass())
			throw new IllegalArgumentException("StringLabelInt should only be compared with StringLabelInt");

		StringLabelInt that = (StringLabelInt) o;
		return number == that.number;
	}

	@Override
	public int hashCode() {
		return number;
	}
}
