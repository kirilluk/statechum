#!/bin/bash
erl -eval 'tracer_coverage:cover_map_to_file('$1', '$2', '$3', '$4', "'$5'"),halt().'
