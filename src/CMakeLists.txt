cmake_minimum_required(VERSION 3.15)
include(FetchContent)

project(ECSQL VERSION 0.1)

set(CMAKE_EXPORT_COMPILE_COMMANDS True)
set(CMAKE_C_VERSION 23)
set(CMAKE_C_VERSION_REQUIRED True)

add_executable(Ecsql
  arena.c main.c
  ecs/ecs.c ecs/query.c
  lisp/lexer.c lisp/lisp.c lisp/memory.c lisp/primitives.c
  lisp/print.c lisp/reader.c lisp/types.c
)

set_property(TARGET Ecsql PROPERTY C_STANDARD 23)

# Dependencies
## Threads
find_package(Threads REQUIRED)
target_link_libraries(Ecsql PRIVATE Threads::Threads)

## Raylib
FetchContent_Declare(
  raylib
  GIT_REPOSITORY git@github.com:raysan5/raylib.git
  GIT_TAG 5.0
)
FetchContent_MakeAvailable(raylib)
target_link_libraries(Ecsql PUBLIC raylib)

target_include_directories(Ecsql PUBLIC ../include ../external)
file(CREATE_LINK "${CMAKE_BINARY_DIR}/compile_commands.json" "compile_commands.json" SYMBOLIC)
