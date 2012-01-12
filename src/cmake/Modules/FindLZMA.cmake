# - Try to find LZMA
# Once done this will define
#  LZMA_FOUND - System has LZMA
#  LZMA_INCLUDE_DIRS - The LZMA include directories
#  LZMA_LIBRARIES - The libraries needed to use LZMA

find_path(LZMA_INCLUDE_DIRS lzma)
find_library(LZMA_LIBRARIES NAMES lzma)
if (NOT LZMA_INCLUDE_DIRS OR NOT LZMA_LIBRARIES)
  message(SEND_ERROR "LZMA not found")
endif (NOT LZMA_INCLUDE_DIRS OR NOT LZMA_LIBRARIES)

set(LZMA_FOUND TRUE)

if (NOT LZMA_FIND_QUIETLY)
  message(STATUS "Found LZMA: ${LZMA_LIBRARIES}")
endif (NOT LZMA_FIND_QUIETLY)
