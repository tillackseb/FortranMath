#!/bin/bash

# Fortran compiler
FC=ifx
# common compiler flags
FFLAGS_COMMON="-fpp -fpic -qmkl"
# release compiler flags
FFLAGS_RELEASE="-O3 -qopenmp -standard-semantics"
# debug compiler flags
FFLAGS_DEBUG="-g -O0 -debug all -warn unused -traceback -fpe3 -fp-model strict -ftrapuv\
  -check bounds -check noarg_temp_created"

POSITIONAL_ARGS=()
PROFILE=release

while [[ $# -gt 0 ]]; do
  case $1 in
    -p|--profile)
      PROFILE="$2"
      shift # past argument
      shift # past value
      ;;
    --default)
      DEFAULT=YES
      shift # past argument
      ;;
    -*|--*)
      echo "Unknown option $1"
      return
      ;;
    *)
      POSITIONAL_ARGS+=("$1") # save positional arg
      shift # past argument
      ;;
  esac
done

set -- "${POSITIONAL_ARGS[@]}" # restore positional parameters

case $PROFILE in
  release)
    FFLAGS="${FFLAGS_RELEASE} ${FFLAGS_COMMON}"
    ;;
  debug)
    FFLAGS="${FFLAGS_DEBUG} ${FFLAGS_COMMON}"
    ;;
  *)
    echo "Unknown profile $PROFILE"
    return
    ;;
esac

export FPM_FC=$FC
export FPM_FFLAGS=$FFLAGS
