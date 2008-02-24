/*Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 
This file is part of StateChum

StateChum is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

StateChum is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with StateChum.  If not, see <http://www.gnu.org/licenses/>.
*/ 

package statechum.analysis.learning.profileStringExtractor;

public class ClassMethodPair {
	
	private Integer classNumber;
	private Integer methodNumber;
	
	public ClassMethodPair(Integer classNumber, Integer methodNumber){
		this.classNumber = classNumber;
		this.methodNumber = methodNumber;
	}
	
	public Integer getClassNumber() {
		return classNumber;
	}
	public Integer getMethodNumber() {
		return methodNumber;
	}

}
