if(NOT CPACK_GENERATOR)
    set(CPACK_GENERATOR "TGZ")
endif()

set(CPACK_CONFIG_FILE "" CACHE FILEPATH
    "Path to a CMake lists syntax file providing settings for CPack.")
set(CPACK_PACKAGE_REVISION "" CACHE STRING
    "A suffix string which can be appended to package versions to account for e. g. multiple rebuilds without changes to the upstream project of the package.")

if(CPACK_CONFIG_FILE)
    include(${CPACK_CONFIG_FILE})
endif()

message(STATUS "Using CPack package generator: ${CPACK_GENERATOR}")

include(CPack)
