package statechum.analysis.learning.rpnicore;

import java.lang.management.ManagementFactory;

import javax.management.AttributeChangeNotification;
import javax.management.MBeanNotificationInfo;
import javax.management.MBeanServer;
import javax.management.Notification;
import javax.management.NotificationBroadcasterSupport;
import javax.management.ObjectName;

public class TestDiagnostics extends NotificationBroadcasterSupport implements TestDiagnosticsMBean 
{
	private String currentStatus;
	
	@Override
	public String getStatus() {
		return currentStatus;
	}

	private static TestDiagnostics diag = null;
	
	public static TestDiagnostics getDiagnostics()
	{
		if (diag == null)
		{
			MBeanServer mbs = ManagementFactory.getPlatformMBeanServer(); 
		    diag = new TestDiagnostics();
		      
		    try {
		    	  ObjectName name = new ObjectName("statechum.analysis.learning.rpnicore.testDiagnostics:type=TestDiagnostics"); 
				mbs.registerMBean(diag, name);
			} catch (Exception e) {
				e.printStackTrace();
			} 
		}
		return diag;
	}
	
	@Override
	public synchronized void setStatus(String newStatus) 
	{// based on http://download.oracle.com/javase/1.5.0/docs/guide/jmx/tutorial/essential.html#wp1053200
		String oldStatus = currentStatus;
		currentStatus=newStatus;
	     Notification n = 
	            new AttributeChangeNotification(this, 
						    sequenceNumber++, 
						    System.currentTimeMillis(), 
						    "New status", 
						    "Status", 
						    "String", 
						    newStatus, 
						    oldStatus); 
	 
		sendNotification(n); 
	}

    @Override 
    public MBeanNotificationInfo[] getNotificationInfo() 
    {// from  http://download.oracle.com/javase/1.5.0/docs/guide/jmx/tutorial/essential.html#wp1053200 
        String[] types = new String[] { 
            AttributeChangeNotification.ATTRIBUTE_CHANGE 
        }; 
        String name = AttributeChangeNotification.class.getName(); 
        String description = "An update to status was received"; 
        MBeanNotificationInfo info = 
            new MBeanNotificationInfo(types, name, description); 
        return new MBeanNotificationInfo[] {info}; 
    } 

	private long sequenceNumber=1;
}
