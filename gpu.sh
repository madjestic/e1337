#!/bin/sh

# a wrapper for an otherwise lengthy expression

gpu() {
    optirun -b primus $1
}

gpu $1
