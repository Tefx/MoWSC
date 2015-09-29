cmake_minimum_required(VERSION 3.0)
project(MoWSC)

set(CMAKE_C_COMPILER "/usr/bin/clang")
set(CMAKE_TOOLCHAIN_PREFIX "llvm-")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -std=c11 -O3 -Wall -Werror")

add_library(chromosome STATIC chromosome.c)
add_library(evaluation STATIC evaluation.c)

add_library(spea2_core OBJECT spea2.c select.c)
add_library(spea2 STATIC $<TARGET_OBJECTS:spea2_core>)

add_library(nsga2_core OBJECT nsga2.c select.c)
add_library(nsga2 STATIC $<TARGET_OBJECTS:nsga2_core>)