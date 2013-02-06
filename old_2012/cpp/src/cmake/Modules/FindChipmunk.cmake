# - Try to find Chipmunk
# Once done this will define
#  CHIPMUNK_FOUND - System has Chipmunk
#  CHIPMUNK_INCLUDE_DIRS - The Chipmunk include directories
#  CHIPMUNK_LIBRARIES - The libraries needed to use Chipmunk

find_path(CHIPMUNK_INCLUDE_DIRS chipmunk)
find_library(CHIPMUNK_LIBRARIES NAMES chipmunk)
if (NOT CHIPMUNK_INCLUDE_DIRS OR NOT CHIPMUNK_LIBRARIES)
  message(SEND_ERROR "Chipmunk not found")
endif (NOT CHIPMUNK_INCLUDE_DIRS OR NOT CHIPMUNK_LIBRARIES)

set(CHIPMUNK_FOUND TRUE)

if (NOT Chipmunk_FIND_QUIETLY)
  message(STATUS "Found Chipmunk: ${CHIPMUNK_LIBRARIES}")
endif (NOT Chipmunk_FIND_QUIETLY)
