# Super Max
# =========

cmake_minimum_required(VERSION 2.6)
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")
project(super_max)

set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -static")
set(CMAKE_FIND_LIBRARY_SUFFIXES .a ${CMAKE_FIND_LIBRARY_SUFFIXES})

# External packages
# -----------------

find_package(Chipmunk REQUIRED)
find_package(SDL REQUIRED)

# Includes
# --------

set(super_max_include_dirs
  ${CHIPMUNK_INCLUDE_DIRS}
)

include_directories(${super_max_include_dirs})

# Libraries
# ---------

set(super_max_libraries
  ${CHIPMUNK_LIBRARIES}
  ${SDL_LIBRARY}
)

# Super Max program
# -----------------

file(GLOB super_max_source_files "*.cpp")
add_executable(super-max ${super_max_source_files})
target_link_libraries(super-max ${super_max_libraries})
