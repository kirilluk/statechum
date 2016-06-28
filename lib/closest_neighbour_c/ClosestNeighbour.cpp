// ClosestNeighbour.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"
#include <stdlib.h>
#include <iostream>
#include <chrono>
#include "immintrin.h"

typedef unsigned short bitCounterType;

#define NUMBER_OF_BITS_IN_BITCOUNTER (sizeof(bitCounterType)*8)

#define COUNTER_ARRAY_SIZE (1 << NUMBER_OF_BITS_IN_BITCOUNTER)

static int bitCounter[COUNTER_ARRAY_SIZE];


__int64 *buffer = nullptr;
__int64 *instance = nullptr;
const int attemptNumber = 150;

#define INSTANCEALIGNMENT  16

#define UNROLL(idx) cnt += __popcnt64(instancePtr[attr+(idx)] & instance[attr+(idx)]);

#define UNROLL4(idx) UNROLL(idx+0);UNROLL(idx+1);UNROLL(idx+2);UNROLL(idx+3)
#define UNROLL16(idx) UNROLL4(idx+0);UNROLL4(idx+4);UNROLL4(idx+8);UNROLL4(idx+12)
#define UNROLL64(idx) UNROLL16(idx+0);UNROLL16(idx+16);UNROLL16(idx+32);UNROLL16(idx+48)

int _tmain(int argc, _TCHAR* argv[])
{

	int instanceSizeBeforeRounding = 21000 / 16;
	int instanceNumber = 38191;

	int instanceSize = INSTANCEALIGNMENT * ((instanceSizeBeforeRounding + INSTANCEALIGNMENT - 1) / INSTANCEALIGNMENT);

	buffer = (__int64*)malloc(instanceSize*instanceNumber*sizeof(__int64));
	if (buffer == NULL)
		return -1;

	for (int i = 0; i < NUMBER_OF_BITS_IN_BITCOUNTER; ++i)
	{
		int counter = 0;
		for (int bit = 0; bit < 8; ++bit)
			if ((i & (1 << bit)) != 0)
				++counter;
		bitCounter[i] = counter;
	}

	instance = (__int64*)malloc(instanceSize*sizeof(__int64));
	int currentBestInstanceIdx = -1, currentBestCount = -1;
	auto start = std::chrono::high_resolution_clock::now();// using http://stackoverflow.com/questions/12937963/get-local-time-in-nanoseconds

	/*
	// this one uses a lookup table to count bits. It takes 60ms to run where Java version runs in 34ms using Long.bitCount
	for (int attemptCount = 0; attemptCount < attemptNumber; ++attemptCount)
	{
		instance[0] = attemptCount;
		for (int i = 0; i < instanceNumber; ++i)
		{
			int cnt = 0;
			bitCounterType *dataPtr = (bitCounterType *)&buffer[i*instanceSize], *instancePtr = (bitCounterType *)instance;
			for (long attr = 0; attr < instanceSize*sizeof(long)/sizeof(bitCounterType); ++attr)
			{
				cnt += bitCounter[dataPtr[attr] & instancePtr[attr]];
			}
			if (cnt > currentBestCount)
			{
				currentBestCount = cnt; currentBestInstanceIdx = i;
			}
		}
	}*/

	// this one uses a popcnt64 instruction to count bits. It takes 22ms to run where Java version runs in 34ms using Long.bitCount
	for (int attemptCount = 0; attemptCount < attemptNumber; ++attemptCount)
	{
		instance[0] = attemptCount;
		for (int i = 0; i < instanceNumber; ++i)
		{
			int cnt = 0;
			__int64 *instancePtr = buffer + i*instanceSize;
			for (long attr = 0; attr < instanceSize / INSTANCEALIGNMENT; ++attr)
			{
				UNROLL16(0); 
			}
			if (cnt > currentBestCount)
			{
				currentBestCount = cnt; currentBestInstanceIdx = i;
			}
		}
	}

	auto finish = std::chrono::high_resolution_clock::now();
	__int64 milliseconds = (std::chrono::duration_cast<std::chrono::nanoseconds>(finish - start).count()) / 1000000;
	printf("best entry is : %d, time %f\n", currentBestInstanceIdx, (double)milliseconds / attemptNumber);
	return 0;
}

