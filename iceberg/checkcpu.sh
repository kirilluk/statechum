#!/bin/sh
CPU=`cat /proc/cpuinfo|grep 'model name'|uniq`
if [ "$CPU" != "model name	: Intel(R) Xeon(R) CPU E5-2630 v3 @ 2.40GHz" ];then
	echo $CPU >> differentcpu.txt
fi
