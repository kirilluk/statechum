#!/bin/bash
erl -eval 'tracer:first_failure('$1', '$2', '$3', "'$4'"),halt().'
