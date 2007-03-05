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
