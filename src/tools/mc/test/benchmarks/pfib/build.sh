#!/bin/bash

# generate pfib-dval-run.pml for benchmarking

tr '\n' ' ' <pfib-body.h >pfib-body-exp.h
cpp-4.0 -P pfib-dval.pml > pfib-dval-run.pml