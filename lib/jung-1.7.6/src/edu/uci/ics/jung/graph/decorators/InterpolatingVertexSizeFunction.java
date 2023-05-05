/*
 * Created on Nov 3, 2005
 *
 * Copyright (c) 2005, the JUNG Project and the Regents of the University 
 * of California
 * All rights reserved.
 *
 * This software is open-source under the BSD license; see either
 * "license.txt" or
 * http://jung.sourceforge.net/license.txt for a description.
 */
package edu.uci.ics.jung.graph.decorators;

import edu.uci.ics.jung.graph.Vertex;

/**
 * Provides vertex sizes that are spaced proportionally between 
 * min_size and max_size depending on 
 * 
 * @author Joshua O'Madadhain
 */
public class InterpolatingVertexSizeFunction implements VertexSizeFunction
{
    protected double min;
    protected double max;
    protected NumberVertexValue nvv;
    protected int min_size;
    protected int size_diff;
    
    public InterpolatingVertexSizeFunction(NumberVertexValue nvv, 
            int min_size, int max_size)
    {
        super();
        if (min_size < 0 || max_size < 0)
            throw new IllegalArgumentException("sizes must be non-negative");
        if (min_size > max_size)
            throw new IllegalArgumentException("min_size must be <= max_size");
        this.min = 0;
        this.max = 0;
        this.nvv = nvv;
        setMinSize(min_size);
        setMaxSize(max_size);
    }

    public int getSize(Vertex v)
    {
        Number n = nvv.getNumber(v);
        double value = min;
        if (n != null)
            value = n.doubleValue();
        min = Math.min(this.min, value);
        max = Math.max(this.max, value);
        
        if (min == max)
            return min_size;
        
        // interpolate between min and max sizes based on how big value is 
        // with respect to min and max values
        return min_size + (int)(((value - min) / (max - min)) * size_diff);
    }
    
    public void setMinSize(int min_size)
    {
        this.min_size = min_size;
    }

    public void setMaxSize(int max_size)
    {
        this.size_diff = max_size - this.min_size;
    }
    
    public void setNumberVertexValue(NumberVertexValue nvv)
    {
        this.nvv = nvv;
    }
}
