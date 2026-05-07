#!/usr/bin/env bash
set -euo pipefail

opam exec -- dune exec rizzoc -- examples/paper.rizz --info-heap

{
  printf 'show\n'
  sleep 5
  printf '1\n'
  sleep 5
  printf 'show\n'
  sleep 5
  printf '2\n'
  sleep 5
  printf 'negate\n'
  sleep 5
  printf '3\n'
  sleep 5
  printf 'show\n'
  sleep 5
  printf '4\n'
  sleep 5
  printf 'negate\n'
  sleep 5
  printf '12345678\n'
  sleep 5
  printf 'nothing\n'
  sleep 5
  printf 'show\n'
  sleep 5
  printf '1\n'
  sleep 5
  printf 'show\n'
  sleep 5
  printf '2\n'
  sleep 5
  printf 'negate\n'
  sleep 5
  printf '3\n'
  sleep 5
  printf 'show\n'
  sleep 5
  printf '4\n'
  sleep 5
  printf 'show\n'
  sleep 5
  sleep 100
  printf 'quit\n'
} | valgrind --leak-check=full ./output
