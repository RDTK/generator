# CMakeLists.txt --- CMake build description file.
#
# Copyright (C) 2011-2017 Jan Moringen
#
# Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

cmake_minimum_required(VERSION 2.8)

# Extract version components from an approximately "git describe"-like
# formatted version string.
function(git_version_components INPUT OUT_MAJOR OUT_MINOR OUT_REVISION OUT_COMMIT)
    set(REGEX "^[-.a-z0-9]*([0-9]+)\\.([0-9]+)[-.]([0-9]+)-?(g[a-f0-9]+(:?-dirty)?|)$")
    string(REGEX REPLACE ${REGEX} "\\1" ${OUT_MAJOR}    "${INPUT}")
    string(REGEX REPLACE ${REGEX} "\\2" ${OUT_MINOR}    "${INPUT}")
    string(REGEX REPLACE ${REGEX} "\\3" ${OUT_REVISION} "${INPUT}")
    string(REGEX REPLACE ${REGEX} "\\4" ${OUT_COMMIT}   "${INPUT}")
    set(${OUT_MAJOR}    ${${OUT_MAJOR}}    PARENT_SCOPE)
    set(${OUT_MINOR}    ${${OUT_MINOR}}    PARENT_SCOPE)
    set(${OUT_REVISION} ${${OUT_REVISION}} PARENT_SCOPE)
    set(${OUT_COMMIT}   ${${OUT_COMMIT}}   PARENT_SCOPE)
endfunction()

# Git-based version: run git describe, parse the output and store the
# "revision" and "commit" components into version.sexp.
find_program(GIT_EXECUTABLE "git")
if(GIT_EXECUTABLE)
    function(git_based_version MAJOR MINOR)
        math(EXPR MINOR_MINUS_ONE "${MINOR} - 1")
        set(RELEASE_TAG "release-${MAJOR}.${MINOR_MINUS_ONE}")
        execute_process(COMMAND ${GIT_EXECUTABLE} describe
                                                  --long --dirty=-dirty
                                                  --tags --match "${RELEASE_TAG}"
                        RESULT_VARIABLE RESULT
                        OUTPUT_VARIABLE GIT_DESCRIBE_OUTPUT
                        OUTPUT_STRIP_TRAILING_WHITESPACE
                        ERROR_QUIET)
        if(RESULT EQUAL 0)
            git_version_components(${GIT_DESCRIBE_OUTPUT} IGN1 IGN2 REVISION COMMIT)
            file(WRITE "${CMAKE_SOURCE_DIR}/version-string.sexp" "\"${MAJOR}.${MINOR}.${REVISION}\"")
            file(WRITE "${CMAKE_SOURCE_DIR}/version.sexp"        "${REVISION} \"${COMMIT}\"")
        else()
            message(STATUS "git describe failed (not a git repository?) in ${CMAKE_SOURCE_DIR}")
        endif()
    endfunction()
else()
    message(STATUS "git executable not found")
endif()

# Find and check SBCL installation.
set(SBCL_HOME $ENV{SBCL_HOME})
if(NOT SBCL_HOME)
    message(STATUS "SBCL_HOME is not set. Trying to find SBCL on default path.")
    find_program(SBCL_EXECUTABLE sbcl)
else()
    message(STATUS "SBCL_HOME is set to ${SBCL_HOME}. Trying to find SBCL there.")
    find_program(SBCL_EXECUTABLE sbcl
                 PATHS "${SBCL_HOME}/../../bin" ${SBCL_HOME}
                 NO_DEFAULT_PATH)
endif()
message(STATUS "SBCL executable: ${SBCL_EXECUTABLE}")

# Configure Lisp environment.
set(LISP_RUNTIME_OPTIONS "" CACHE STRING
    "Runtime options to pass to the Lisp system.")
set(LISP_SOURCE_REGISTRY_ENTRIES "" CACHE STRING
    "Additional entries to place on ASDF source registry.")
set(LISP_FASL_CACHE_DIR "${CMAKE_CURRENT_BINARY_DIR}/fasl-cache"
    CACHE PATH
    "Directory in which the fasl cache should be created.")
set(LISP_INIT_FILE       "" CACHE FILEPATH
    "File to load as Lisp init file.")
set(LISP_INIT_CODE       "" CACHE STRING
    "Lisp code to be evaulated during initialization.")
set(LISP_INIT "")
if(LISP_INIT_FILE)
    set(LISP_INIT "${LISP_INIT} --userinit ${LISP_INIT_FILE}")
endif()
if(LISP_INIT_CODE)
    set(LISP_INIT "${LISP_INIT} --eval \"${LISP_INIT_CODE}\"")
endif()

set(CL_SOURCE_REGISTRY       "(:source-registry
                                (:tree \\\"${CMAKE_CURRENT_SOURCE_DIR}\\\")
                                ${LISP_SOURCE_REGISTRY_ENTRIES}
                                :inherit-configuration)")
set(ASDF_OUTPUT_TRANSLATIONS "(:output-translations
                                (t (\\\"${LISP_FASL_CACHE_DIR}\\\" :implementation))
                                :ignore-inherited-configuration)")

# Execute Lisp processes, potentially with output redirection.
function(configure_lisp NAME CODE)
    set(NAME "${NAME}")
    set(DO   "${CODE} --quit")
    configure_file("${CMAKE_CURRENT_SOURCE_DIR}/sbcl.cmake.in"
                   "${CMAKE_CURRENT_BINARY_DIR}/sbcl.${NAME}.cmake"
                   @ONLY)
    set(REDIRECTIONS)
endfunction()
function(configure_lisp_redirect NAME CODE)
    set(REDIRECTIONS "OUTPUT_FILE \"${CMAKE_CURRENT_BINARY_DIR}/${NAME}.out\"
                      ERROR_FILE  \"${CMAKE_CURRENT_BINARY_DIR}/${NAME}.err\"")
    configure_lisp(${NAME} ${CODE})
endfunction()
function(execute_lisp NAME CODE OUTPUT_VARIABLE)
    configure_lisp(${NAME} "${CODE}")
    message(STATUS "Executing Lisp process \"${NAME}\"")
    execute_process(COMMAND cmake -P "${CMAKE_CURRENT_BINARY_DIR}/sbcl.${NAME}.cmake"
                    OUTPUT_VARIABLE ${OUTPUT_VARIABLE}
                    OUTPUT_STRIP_TRAILING_WHITESPACE)
    set(${OUTPUT_VARIABLE} ${${OUTPUT_VARIABLE}} PARENT_SCOPE)
endfunction()

# Project definition (version is grovelled from asd system definition,
# possibly in combination with version.sexp file written below).
project(build-generator)
string(TOUPPER "${CMAKE_PROJECT_NAME}" PROJECT_NAME_UPPERCASE)
set(SYSTEM_NAME "jenkins.project")

# Determine at least major and minor version components.
set(REDIRECTIONS "ERROR_FILE \"${CMAKE_CURRENT_BINARY_DIR}/version.1.err\"")
execute_lisp("version"
             "--eval \"(princ (asdf:component-version (asdf:find-system :${SYSTEM_NAME})))\""
             VERSION_FROM_LISP)
git_version_components(${VERSION_FROM_LISP} ${PROJECT_NAME_UPPERCASE}_VERSION_MAJOR
                                            ${PROJECT_NAME_UPPERCASE}_VERSION_MINOR
                                            ${PROJECT_NAME_UPPERCASE}_VERSION_PATCH
                                            ${PROJECT_NAME_UPPERCASE}_VERSION_COMMIT)

# If necessary, try to run git describe and determine remaining
# version components.
if(NOT ${PROJECT_NAME_UPPERCASE}_VERSION_PATCH)
    git_based_version(${${PROJECT_NAME_UPPERCASE}_VERSION_MAJOR} ${${PROJECT_NAME_UPPERCASE}_VERSION_MINOR})
    set(REDIRECTIONS "ERROR_FILE \"${CMAKE_CURRENT_BINARY_DIR}/version.2.err\"")
    execute_lisp("version"
                 "--eval \"(princ (asdf:component-version (asdf:find-system :${SYSTEM_NAME})))\""
                 VERSION_FROM_LISP)
    git_version_components(${VERSION_FROM_LISP} ${PROJECT_NAME_UPPERCASE}_VERSION_MAJOR
                                                ${PROJECT_NAME_UPPERCASE}_VERSION_MINOR
                                                ${PROJECT_NAME_UPPERCASE}_VERSION_PATCH
                                                ${PROJECT_NAME_UPPERCASE}_VERSION_COMMIT)
endif()

message(STATUS "Project is ${CMAKE_PROJECT_NAME} (${VERSION_FROM_LISP})")

# Create binary.
set(MAIN_NAME "build-generator")
set(MAIN_SYSTEM_NAME "jenkins.project.commandline-interface")
configure_lisp_redirect("${MAIN_NAME}"
                        "--eval \"asdf::(defmethod perform ((o image-op) (c system))
                                          (dump-image (output-file o c) :executable  (typep o (quote program-op))
                                                                        :compression 9))\"
                         --eval \"(asdf:oos (quote asdf:program-op) :${MAIN_SYSTEM_NAME})\"
                         --eval \"(rename-file (asdf:output-file 'asdf:program-op :${MAIN_SYSTEM_NAME})
                                               (make-pathname :name     \\\"${MAIN_NAME}\\\"
                                                              :type     :unspecific
                                                              :defaults #P\\\"${CMAKE_CURRENT_BINARY_DIR}/\\\"))\"")
add_custom_command(OUTPUT  "${CMAKE_CURRENT_BINARY_DIR}/${MAIN_NAME}"
                   COMMAND "${CMAKE_COMMAND}" -P "${CMAKE_CURRENT_BINARY_DIR}/sbcl.${MAIN_NAME}.cmake"
                   COMMENT "Creating Lisp image ${MAIN_NAME} (this can take a long time)")
add_custom_target(binary ALL DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/${MAIN_NAME}")

# Tests
enable_testing()

configure_lisp_redirect(test
                        "--eval \"(proclaim (quote (optimize (debug 3) (safety 3) (speed 0))))\"
                         --eval \"(asdf:test-system :${SYSTEM_NAME})\"")
add_test(NAME    "${SYSTEM_NAME}"
         COMMAND "${CMAKE_COMMAND}" -P "${CMAKE_CURRENT_BINARY_DIR}/sbcl.test.cmake")

# Installation
install(PROGRAMS    "${CMAKE_CURRENT_BINARY_DIR}/${MAIN_NAME}"
        DESTINATION "bin")

file(GLOB ASD_FILES
     RELATIVE "${CMAKE_CURRENT_SOURCE_DIR}"
     "*.asd")
file(GLOB SEXP_FILES
     RELATIVE "${CMAKE_CURRENT_SOURCE_DIR}"
     "*.sexp")
install(DIRECTORY      "${CMAKE_CURRENT_SOURCE_DIR}/src"
        DESTINATION    "share/common-lisp/source/${CMAKE_PROJECT_NAME}"
        FILES_MATCHING PATTERN "*.lisp")
install(DIRECTORY      "${CMAKE_CURRENT_SOURCE_DIR}/test"
        DESTINATION    "share/common-lisp/source/${CMAKE_PROJECT_NAME}"
        FILES_MATCHING PATTERN "*.lisp")
install(DIRECTORY
        DESTINATION "share/common-lisp/source/${CMAKE_PROJECT_NAME}")
install(FILES       ${ASD_FILES} ${SEXP_FILES}
        DESTINATION "share/common-lisp/source/${CMAKE_PROJECT_NAME}/")

# Packaging
set(PACKAGE_BASE_NAME           "${CMAKE_PROJECT_NAME}")
set(CPACK_PACKAGE_VERSION_MAJOR ${${PROJECT_NAME_UPPERCASE}_VERSION_MAJOR})
set(CPACK_PACKAGE_VERSION_MINOR ${${PROJECT_NAME_UPPERCASE}_VERSION_MINOR})
set(CPACK_PACKAGE_VERSION_PATCH ${${PROJECT_NAME_UPPERCASE}_VERSION_PATCH})
set(CPACK_PACKAGE_VERSION       "${CPACK_PACKAGE_VERSION_MAJOR}.${CPACK_PACKAGE_VERSION_MINOR}.${CPACK_PACKAGE_VERSION_PATCH}")
set(CPACK_PACKAGE_VENDOR        "CoR-Lab, Bielefeld University")
set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_SOURCE_DIR}/COPYING")

include("${CMAKE_SOURCE_DIR}/cpack/CPackInclude.cmake")
