# Super Max
# =========

cmake_minimum_required(VERSION 2.6)
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")
set(CMAKE_CXX_FLAGS ${CMAKE_CXX_FLAGS} "${CMAKE_C_FLAGS} -std=gnu++0x")
project(super_max)

# External packages
# -----------------

find_package(Chipmunk REQUIRED)
find_package(SDL REQUIRED)
find_package(SDL_image REQUIRED)
find_package(SDL_mixer REQUIRED)
find_package(SDL_ttf REQUIRED)

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
  ${SDLIMAGE_LIBRARY}
  ${SDLMIXER_LIBRARY}
  ${SDLTTF_LIBRARY}
)

# Super Max program
# -----------------

file(GLOB super_max_source_files "*.cpp")
add_executable(super-max ${super_max_source_files})
target_link_libraries(super-max ${super_max_libraries})
