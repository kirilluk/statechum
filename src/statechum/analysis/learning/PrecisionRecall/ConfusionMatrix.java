package statechum.analysis.learning.PrecisionRecall;

public class ConfusionMatrix {
	
	int tp,tn,fp,fn;

	public ConfusionMatrix(int tpArg, int tnArg, int fpArg, int fnArg) {
		super();
		this.tp = tpArg;
		this.tn = tnArg;
		this.fp = fpArg;
		this.fn = fnArg;
	}
	
	public ConfusionMatrix(ConfusionMatrix mat) {
		super();
		this.tp = mat.tp;
		this.tn = mat.tn;
		this.fp = mat.fp;
		this.fn = mat.fn;
	}

	private static double divide(double a, double b)
	{
		assert !Double.isNaN(a);assert !Double.isNaN(b);
		if (b == 0) return 0;
		double result = a/b;assert !Double.isNaN(result);
		return result;
	}
	
	public double getPrecision(){
		return divide(tp,tp+fp);
	}
	
	public double getRecall(){
		return divide(tp,tp+fn);
	}
	
	public double getSpecificity(){
		return divide(tn,tn+fp);
	}
	
	public double fMeasure(){
		double prec = getPrecision();
		double rec = getRecall();
		return divide(2*prec*rec,prec+rec);
	}
	
	public double BCR(){
		double specificity = getSpecificity();
		double sensitivity = getRecall();
		return divide(sensitivity+specificity,2);
	}

	@Override
	public String toString()
	{
		return "tp="+tp+", tn="+tn+", fp="+fp+", fn="+fn; 
	}
}
