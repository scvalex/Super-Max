# Super Max
# =========

cmake_minimum_required(VERSION 2.6)
project(super_max)

# Includes
# --------

set(super_max_include_dirs
)

include_directories(${super_max_include_dirs})

# Libraries
# ---------

set(super_max_libraries
)

# Super Max program
# -----------------

file(GLOB super_max_source_files "*.cpp")
add_executable(super-max ${super_max_source_files})
target_link_libraries(super-max ${super_max_libraries})
