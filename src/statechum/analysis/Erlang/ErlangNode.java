/* Copyright (c) 2013 The University of Sheffield.
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
 */
package statechum.analysis.Erlang;

import java.io.IOException;
import java.net.InetAddress;
import java.net.UnknownHostException;

import com.ericsson.otp.erlang.OtpNode;

import statechum.GlobalConfiguration;
import statechum.GlobalConfiguration.G_PROPERTIES;

public class ErlangNode {
	
	/** The name of this node. */
	protected final String ourNode;

	/** Erlang node of this Java runtime. */
	protected OtpNode self;

	/** Our cookie, created here because it has to be available in advance of node creation (when not invoked from Erlang, we have to launch Erlang runtime first so that it starts epmd, then create a node becase node creation needs epmd). */
	protected final String cookie;
	
	/** Accessor name chosen to match that of OtpNode. */
	public String getName()
	{
		return ourNode;
	}
	
	/** Accessor name chosen to match that of OtpNode. */
	public String cookie()
	{
		return cookie;
	}
	
	/** Initialises node parameters with the specified values.
	 * 
	 * @param ourNodeName node name to be used.
	 * @param cookieValue value to use for the cookie.
	 */
	protected ErlangNode(String ourNodeName, String cookieValue)
	{
		ourNode = ourNodeName;
		cookie = cookieValue;
	}
	
	/** Initialises node parameters with default values.
	 */
	protected ErlangNode()
	{
		String hostName = "@localhost";
		if (!Boolean.parseBoolean(GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.ERLANG_SHORTNODENAME)))
		{
			try
			{
				hostName = "@"+InetAddress.getLocalHost().getHostName();
			}
			catch(UnknownHostException ex)
			{// ignore this, host name remains as "localhost".
			}
		}
		/* This ID is used to construct node name as well as cookie value if it is not provided to us. */
		final String uniqueID = System.nanoTime()+ hostName;
		ourNode = "java" + uniqueID;
		cookie = "cookie_"+ uniqueID;
	}
	
	/** Initialises the node on the first call, after that ignores all calls. 
	 * 
	 * @param ourNodeName node name to be used. If null, ignores the value of the cookie and sets the default values.
	 * @param cookieValue value to use for the cookie. Used if <i>ourNodeName</i> is not null.
	 */
	public static void initNodeParameters(String ourNodeName, String cookieValue)
	{
		if (statechumNode == null)
		{
			if (ourNodeName == null)
				statechumNode = new ErlangNode();
			else
				statechumNode = new ErlangNode(ourNodeName, cookieValue);
		}
	}
	
	/** Creates a new node if necessary. Vital during initialisation. */
	public void createNode()
	{
		if (self == null)
		{
			// Based on
			// http://erlang.org/pipermail/erlang-questions/2010-March/050226.html
			try {
				self = new OtpNode(ourNode, cookie);
			} catch (IOException e) {
				e.printStackTrace();
				// if we are here, node remains null.
			}
		}
	}
	
	/** Singleton instance of the node per Java process. */
	protected static ErlangNode statechumNode = null;
	
	public static ErlangNode getErlangNode()
	{
		if (statechumNode == null)
			throw new IllegalArgumentException("node is not ready yet");
		return statechumNode;
	}
	
	public OtpNode getNode()
	{
		return self;
	}
	
}
