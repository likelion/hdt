cmake_minimum_required(VERSION 3.22)
project(hdt4pl)

add_library(hdt4pl SHARED c/hdt4pl.cpp)

set_target_properties(hdt4pl
  PROPERTIES
  LIBRARY_OUTPUT_DIRECTORY "${PROJECT_SOURCE_DIR}/lib/x86_64-darwin"
  PREFIX ""
  SUFFIX ".so"
)

include_directories(/usr/local/opt/swipl/libexec/lib/swipl/include /usr/local/Cellar/hdt/1.3.3/include)

target_link_libraries(hdt4pl PRIVATE /usr/local/opt/swipl/libexec/lib/swipl/lib/x86_64-darwin/libswipl.dylib)
target_link_libraries(hdt4pl PRIVATE /usr/local/Cellar/hdt/1.3.3/lib/libhdt.dylib)
