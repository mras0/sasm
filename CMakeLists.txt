cmake_minimum_required(VERSION 3.7)
project(sasm C)

if (MSVC)
    set(CMAKE_CONFIGURATION_TYPES "Debug;Release" CACHE STRING "Supported build configurations" FORCE)

    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} /Zi /Zo /sdl /W4")
    if (NOT "${CMAKE_C_COMPILER_ID}" STREQUAL "Clang")
        set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} /JMC")
    endif()
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /DEBUG")
    set_property(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} PROPERTY VS_STARTUP_PROJECT ${PROJECT_NAME})
    add_definitions("-D_CRT_SECURE_NO_WARNINGS")
else()
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -ggdb -Wall -Wextra -pedantic -std=c99")
endif()


if (WIN32)
    add_definitions("-DWIN32 -D_WIN32 -DUNICODE -D_UNICODE")
endif()

add_executable(sasm sasm.c)
add_executable(disktool disktool.c)

macro(SASM INPUT OUTPUT)
    set(OUTPUT_FILE "${CMAKE_CURRENT_BINARY_DIR}/${OUTPUT}")
    add_custom_command(
        OUTPUT "${OUTPUT_FILE}"
        COMMAND sasm -o "${OUTPUT_FILE}" "${CMAKE_CURRENT_SOURCE_DIR}/${INPUT}"
        DEPENDS sasm "${CMAKE_CURRENT_SOURCE_DIR}/${INPUT}"
    )
endmacro()

SASM(sasm.asm sasm.com)
SASM(boot.asm boot.bin)
SASM(os.asm os.sys)
SASM(cmdp.asm cmdp.com)
SASM(uvi.asm uvi.com)

add_custom_target(sasm_com ALL DEPENDS sasm.com)
add_custom_target(uvi_com ALL DEPENDS uvi.com)

set(DISK_IMG "${CMAKE_CURRENT_BINARY_DIR}/disk.img")

add_custom_command(
    OUTPUT ${DISK_IMG}
    COMMAND disktool "${DISK_IMG}" create
    COMMAND disktool "${DISK_IMG}" boot   "${CMAKE_CURRENT_BINARY_DIR}/boot.bin"
    COMMAND disktool "${DISK_IMG}" put    "${CMAKE_CURRENT_BINARY_DIR}/os.sys"
    COMMAND disktool "${DISK_IMG}" put    "${CMAKE_CURRENT_BINARY_DIR}/cmdp.com"
    COMMAND disktool "${DISK_IMG}" put    "${CMAKE_CURRENT_BINARY_DIR}/sasm.com"
    COMMAND disktool "${DISK_IMG}" put    "${CMAKE_CURRENT_SOURCE_DIR}/autoexec.bat"
    COMMAND disktool "${DISK_IMG}" put    "${CMAKE_CURRENT_SOURCE_DIR}/strap.bat"
    COMMAND disktool "${DISK_IMG}" put    "${CMAKE_CURRENT_SOURCE_DIR}/sasm.asm"
    COMMAND disktool "${DISK_IMG}" put    "${CMAKE_CURRENT_SOURCE_DIR}/boot.asm"
    COMMAND disktool "${DISK_IMG}" put    "${CMAKE_CURRENT_SOURCE_DIR}/os.asm"
    COMMAND disktool "${DISK_IMG}" put    "${CMAKE_CURRENT_SOURCE_DIR}/cmdp.asm"
    COMMAND disktool "${DISK_IMG}" put    "${CMAKE_CURRENT_SOURCE_DIR}/insboot.asm"
    COMMAND disktool "${DISK_IMG}" put    "${CMAKE_CURRENT_SOURCE_DIR}/uvi.asm"
    COMMAND disktool "${DISK_IMG}" list
    DEPENDS
        disktool
        "${CMAKE_CURRENT_BINARY_DIR}/boot.bin"
        "${CMAKE_CURRENT_BINARY_DIR}/os.sys"
        "${CMAKE_CURRENT_BINARY_DIR}/sasm.com"
        "${CMAKE_CURRENT_BINARY_DIR}/cmdp.com"
        "${CMAKE_CURRENT_SOURCE_DIR}/autoexec.bat"
        "${CMAKE_CURRENT_SOURCE_DIR}/strap.bat"
        "${CMAKE_CURRENT_SOURCE_DIR}/sasm.asm"
        "${CMAKE_CURRENT_SOURCE_DIR}/boot.asm"
        "${CMAKE_CURRENT_SOURCE_DIR}/os.asm"
        "${CMAKE_CURRENT_SOURCE_DIR}/cmdp.asm"
        "${CMAKE_CURRENT_SOURCE_DIR}/insboot.asm"
        "${CMAKE_CURRENT_SOURCE_DIR}/uvi.asm"
    )

add_custom_target(build_disk ALL DEPENDS "${DISK_IMG}")

find_program(QEMU qemu-system-i386)
if (QEMU)
    message("Found qemu: ${QEMU}")
    set(QEMU_EXTRA_ARGS "" CACHE STRING "Extra arguments to give QEMU. E.g. \"-s -S\"")
    add_custom_target(
        qemu_test
        COMMAND qemu-system-i386 -drive format=raw,file=\"${DISK_IMG}\",if=floppy ${QEMU_EXTRA_ARGS}
        DEPENDS ${DISK_IMG}
    )
endif()

find_program(DOSBOX dosbox)
if (DOSBOX)
    message("Found dosbox: ${DOSBOX}")
    add_custom_target(
        test_cmdp
        COMMAND ${DOSBOX} cmdp.com
        WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
        DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/cmdp.com"
    )
    add_custom_target(
        test_uvi
        COMMAND ${CMAKE_COMMAND} -E copy "${CMAKE_CURRENT_SOURCE_DIR}/sasm.asm" sasm.asm
        COMMAND ${DOSBOX} uvi.com
        WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
        DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/uvi.com"
    )
endif()

if (MSVC)
    # TODO: Get this working when using MSYS Makefiles....
    add_custom_target(test_sasm_c
        COMMAND set ASM="$<TARGET_FILE:sasm>"
        COMMAND test.cmd
        WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/tests"
        DEPENDS sasm
        )
    add_custom_target(test_sasm_asm
        COMMAND ${CMAKE_COMMAND} -E copy "${CMAKE_CURRENT_BINARY_DIR}/sasm.com" sasm.com
        COMMAND testsasm.cmd
        WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/tests"
        DEPENDS sasm_com
        )
    add_custom_target(test DEPENDS test_sasm_c test_sasm_asm)
endif()
