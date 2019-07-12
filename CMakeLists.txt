cmake_minimum_required(VERSION 3.7)
project(sasm C)

if (MSVC)
    set(CMAKE_CONFIGURATION_TYPES "Debug;Release" CACHE STRING "Supported build configurations" FORCE)

    set(CMAKE_CXX_FLAGS "${CMAKE_C_FLAGS} /Zi /Zo /sdl /W4")
    if (NOT "${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
        set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} /JMC")
    endif()
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /DEBUG")
    set_property(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} PROPERTY VS_STARTUP_PROJECT ${PROJECT_NAME})
    add_definitions("-D_CRT_SECURE_NO_WARNINGS")
else()
    message(ERROR "Not implemented")
endif()


if (WIN32)
    add_definitions("-DWIN32 -D_WIN32 -DUNICODE -D_UNICODE")
endif()

add_executable(sasm sasm.c)

set(SASM_COM ${CMAKE_CURRENT_BINARY_DIR}/sasm.com)

add_custom_command(
    OUTPUT ${SASM_COM}
    COMMAND sasm -o "${SASM_COM}" "${CMAKE_CURRENT_SOURCE_DIR}/sasm.asm"
    DEPENDS sasm
)

add_custom_target(SASM_COM ALL DEPENDS ${SASM_COM})
