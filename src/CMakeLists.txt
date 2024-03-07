cmake_minimum_required(VERSION 3.15)
include(FetchContent)

project(ECSQL VERSION 0.1)

set(CMAKE_EXPORT_COMPILE_COMMANDS True)
set(CMAKE_C_VERSION 23)
set(CMAKE_C_VERSION_REQUIRED True)
set(FETCHCONTENT_QUIET False)

add_executable(Ecsql
  arena.c main.c
  ecs/ecs.c ecs/query.c
  lisp/lexer.c lisp/lisp.c lisp/memory.c lisp/primitives.c
  lisp/print.c lisp/reader.c lisp/types.c lisp/systems.c
)

target_compile_options(Ecsql PRIVATE -Wall -g)

set_property(TARGET Ecsql PROPERTY C_STANDARD 23)

# Dependencies
## Threads
find_package(Threads REQUIRED)
target_link_libraries(Ecsql PRIVATE Threads::Threads)

## Raylib
FetchContent_Declare(
  raylib
  URL https://github.com/raysan5/raylib/archive/refs/tags/5.0.tar.gz
)
FetchContent_MakeAvailable(raylib)
target_link_libraries(Ecsql PUBLIC raylib)

## Raygui
FetchContent_Declare(
  raygui
  URL https://github.com/raysan5/raygui/archive/refs/tags/4.0.tar.gz
  SOURCE_SUBDIR projects/CMake
)
FetchContent_MakeAvailable(raygui)
target_link_libraries(Ecsql PUBLIC raygui)

target_include_directories(Ecsql PUBLIC ../include ../external)
file(CREATE_LINK "${CMAKE_BINARY_DIR}/compile_commands.json" "compile_commands.json" SYMBOLIC)
