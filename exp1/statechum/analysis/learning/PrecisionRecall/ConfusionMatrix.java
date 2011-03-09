package statechum.analysis.learning.PrecisionRecall;

public class ConfusionMatrix {
	
	double tp,tn,fp,fn;

	public ConfusionMatrix(double tp, double tn, double fp, double fn) {
		super();
		this.tp = tp;
		this.tn = tn;
		this.fp = fp;
		this.fn = fn;
	}
	
	public double getPrecision(){
		double precision = (tp/(tp+fp));
		if(Double.isNaN(precision))
			return 0;
		else
			return precision;
	}
	
	public double getRecall(){
		double recall = (tp/(tp+fn));
		if(Double.isNaN(recall))
			return 0;
		else
			return recall;
	}
	
	public double getSpecificity(){
		double specificity =  (tn/(tn+fp));
		if(Double.isNaN(specificity))
			return 0;
		else
			return specificity;
	}
	
	public double fMeasure(){
		double prec = getPrecision();
		double rec = getRecall();
		double f = (2*prec*rec)/(prec+rec);
		if(Double.isNaN(f))
			return 0;
		else
			return f;
	}
	
	public double BCR(){
		double specificity = getSpecificity();
		double sensitivity = getRecall();
		double bcr = (sensitivity+specificity)/2;
		if(Double.isNaN(bcr))
			return 0;
		else
			return bcr;
	}

}
