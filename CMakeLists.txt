cmake_minimum_required(VERSION 3.8)
project(QuestDecisionTree)

set(CMAKE_CXX_STANDARD 11)


# Local header files here ONLY
SET(TARGET_H function.h)

# Local source files here
SET(TARGET_SRC function.cpp)



#add_library(function function.h function.cpp)
#include_directories(${PROJECT_SOURCE_DIR})

set(SOURCE_FILES main.cpp function.h function.cpp)
add_executable(QuestDecisionTree ${SOURCE_FILES})

find_package(GSL REQUIRED)    # See below (2)
target_link_libraries(QuestDecisionTree GSL::gsl)


