#!/bin/bash
erl -eval 'tracer2:first_failure('$1', '$2', '$3', "'$4'"),halt().'
