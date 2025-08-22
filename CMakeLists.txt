cmake_minimum_required(VERSION 3.22)

project(flame LANGUAGES C CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

find_package(LLVM CONFIG REQUIRED)

list(APPEND CMAKE_MODULE_PATH "${LLVM_CMAKE_DIR}")
include(HandleLLVMOptions)
add_definitions(${LLVM_DEFINITIONS})

add_executable(flame src/main.cpp
        src/lexer.cpp
        src/utils.cpp
        src/lexer.hpp
        src/utils.hpp
        src/error.cpp
        src/error.hpp
)

target_include_directories(flame PRIVATE ${LLVM_INCLUDE_DIRS})

llvm_map_components_to_libnames(llvm_libs Support Core IRReader)

target_link_libraries(flame PRIVATE ${llvm_libs})

if (NOT MSVC)
    target_link_libraries(flame PRIVATE m)
endif()
