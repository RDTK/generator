set(CPACK_GENERATOR "DEB")

set(CPACK_DEBIAN_PACKAGE_NAME         "${PACKAGE_BASE_NAME}")
set(CPACK_DEBIAN_PACKAGE_VERSION      "${CPACK_PACKAGE_VERSION}${CPACK_PACKAGE_REVISION}")
set(CPACK_DEBIAN_PACKAGE_MAINTAINER   "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>")
set(CPACK_DEBIAN_PACKAGE_DESCRIPTION  "Automated Build Generator
 Generates jobs on a Jenkins server according to specifications")
set(CPACK_DEBIAN_PACKAGE_PRIORITY     "optional")
set(CPACK_DEBIAN_PACKAGE_SECTION      "lisp")
set(CPACK_DEBIAN_PACKAGE_DEPENDS      "")

# Generate system links.
set(COMMANDS "")
if(CMAKE_VERSION VERSION_LESS "2.8.7")
    set(PREFIX "\\\${CMAKE_INSTALL_PREFIX}")
else()
    set(PREFIX "\\\${CMAKE_INSTALL_PREFIX}/usr")
endif()
foreach(NAME ${ASD_FILES})
    set(COMMANDS "${COMMANDS} && ln -fs \\\"../source/${CMAKE_PROJECT_NAME}/${NAME}\\\" \\\"${PREFIX}/share/common-lisp/systems/${NAME}\\\"")
endforeach()
set(CPACK_INSTALL_COMMANDS "sh -c 'mkdir -p \\\"${PREFIX}/share/common-lisp/systems\\\" ${COMMANDS}'")

# Generate postinst and prerm hooks.
set(POSTINST_SCRIPT "${CMAKE_CURRENT_BINARY_DIR}/postinst")
set(PRERM_SCRIPT    "${CMAKE_CURRENT_BINARY_DIR}/prerm")
file(WRITE "${POSTINST_SCRIPT}"
           "#!/bin/sh\n\n                                              \\
            set -e\n                                                   \\
            if [ \"$1\" = \"configure\" ] &&                           \\
                 which register-common-lisp-source > /dev/null; then\n \\
              register-common-lisp-source \"${SYSTEM_NAME}\"\n         \\
            fi\n\n")
file(WRITE "${PRERM_SCRIPT}"
           "#!/bin/sh\n\n                                                \\
            set -e\n                                                     \\
            if [ \"$1\" = \"remove\" ]                                   \\
                 || [ \"$1\" = \"upgrade\" ]                             \\
                 || [ \"$1\" = \"deconfigure\" ]; then\n                 \\
              if which unregister-common-lisp-source > /dev/null; then\n \\
                unregister-common-lisp-source \"${SYSTEM_NAME}\"\n       \\
              fi\n                                                       \\
            fi\n\n")
execute_process(COMMAND chmod 755 "${POSTINST_SCRIPT}" "${PRERM_SCRIPT}")
set(CPACK_DEBIAN_PACKAGE_CONTROL_EXTRA "${POSTINST_SCRIPT};${PRERM_SCRIPT}")

# Generate required change log files.
find_program(LSB_EXECUTABLE "lsb_release")
execute_process(COMMAND ${LSB_EXECUTABLE} --short --codename
                OUTPUT_VARIABLE LSB_CODENAME
                OUTPUT_STRIP_TRAILING_WHITESPACE)

execute_process(COMMAND ${GIT_EXECUTABLE}
                        log "--format=%ad  %an  <%ae>%n%n%w(76,8,10)%s%w(76,8,8)%n%n%b%n"
                        --date=short
                COMMAND gzip -n -9
                OUTPUT_FILE "${CMAKE_BINARY_DIR}/changelog.gz")
execute_process(COMMAND sh -c "echo -n \"sed -e '\" ; for c in $(${GIT_EXECUTABLE} rev-list --all -- \"${CMAKE_CURRENT_LIST_FILE}\") ; do echo -n \"s/$c/$(${GIT_EXECUTABLE} describe --tags $c | sed -re s/[^0-9]*\\([0-9]+\\)\\.\\([0-9]+\\)-\\([0-9]+\\)-.*/\\\\1.\\'\\$\\(\\(\\\\2+1\\)\\)\\'.\\\\3/)/\\;\" ; done ; echo \"'\""
                OUTPUT_VARIABLE COMMIT_TO_VERSION_SED_RULES)
execute_process(COMMAND ${GIT_EXECUTABLE}
                        log "--format=${CPACK_DEBIAN_PACKAGE_NAME} (%H) ${LSB_CODENAME}; urgency=low%n%n%w(76,8,10)%s%w(76,8,8)%n%n%b%n%n%w(200,1,1)-- %an <%ae>  %ad%n"
                        --date=rfc
                        -- "${CMAKE_CURRENT_LIST_FILE}"
                COMMAND sh -c ${COMMIT_TO_VERSION_SED_RULES}
                COMMAND gzip -n -9
                OUTPUT_FILE "${CMAKE_BINARY_DIR}/changelog.Debian.gz")
install(FILES "${CMAKE_BINARY_DIR}/changelog.gz"
              "${CMAKE_BINARY_DIR}/changelog.Debian.gz"
        DESTINATION "share/doc/${CPACK_DEBIAN_PACKAGE_NAME}")

# Install copyright file.
install(FILES "${CMAKE_SOURCE_DIR}/COPYING"
        DESTINATION "share/doc/${CPACK_DEBIAN_PACKAGE_NAME}"
        RENAME      copyright)

set(CPACK_PACKAGE_FILE_NAME "${CPACK_DEBIAN_PACKAGE_NAME}-${CPACK_DEBIAN_PACKAGE_VERSION}_${CMAKE_SYSTEM_PROCESSOR}")

message(STATUS "Debian Package: ${CPACK_DEBIAN_PACKAGE_NAME} (${CPACK_DEBIAN_PACKAGE_VERSION}) [${CPACK_PACKAGE_FILE_NAME}.deb]")
