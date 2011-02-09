#!/bin/bash
#erl -eval 'tracer:gen_max_coverage_traces('$1', '$2', '$3', "'$4'"),halt().'
erl -eval 'tracer2:gen_random_traces('$1', '$2', '$3', "'$4'"),halt().'