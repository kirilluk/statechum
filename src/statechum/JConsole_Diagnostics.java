package statechum;

import java.lang.management.ManagementFactory;

import javax.management.AttributeChangeNotification;
import javax.management.MBeanNotificationInfo;
import javax.management.MBeanServer;
import javax.management.Notification;
import javax.management.NotificationBroadcasterSupport;
import javax.management.ObjectName;

/** Integration into JConsole to show test progress for really long-running GD-tests. */
public class JConsole_Diagnostics extends NotificationBroadcasterSupport implements JConsole_DiagnosticsMBean 
{
	private String currentStatus;
	
	@Override
	public String getStatus() {
		return currentStatus;
	}

	private static JConsole_Diagnostics diag = null;
	private static final Object lockObject = new Object();
	
	public static JConsole_Diagnostics getDiagnostics()
	{
		synchronized (lockObject) 
		{
			if (diag == null)
			{
				MBeanServer mbs = ManagementFactory.getPlatformMBeanServer(); 
			    diag = new JConsole_Diagnostics();
			      
			    try {
			    	  ObjectName name = new ObjectName("statechum.JConsole_Diagnostics:type=JConsole_Diagnostics"); 
					mbs.registerMBean(diag, name);
				} catch (Exception e) {
					e.printStackTrace();
				} 
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
