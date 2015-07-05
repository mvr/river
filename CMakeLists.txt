cmake_minimum_required(VERSION 2.6)
project(River)


SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")
find_package(OpenMP)
if (OPENMP_FOUND AND NOT WIN32)
  set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS}")
  set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${OpenMP_CXX_FLAGS}")
endif()

SET(CMAKE_VERBOSE_MAKEFILE ON)

set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/../cmake ${PROJECT_SOURCE_DIR}/cmake ${PROJECT_SOURCE_DIR}/cmake)
set(LIBIGL_ROOT ${PROJECT_SOURCE_DIR}/libigl)
set(CMAKE_COLOR_MAKEFILE ON)

############################
## SEPARATE BUILD OPTIONS ##
############################

find_package(OpenGL REQUIRED)
find_package(EIGEN REQUIRED)
find_package(LIBIGL REQUIRED)
find_package(ANTTWEAKBAR REQUIRED)
find_package(TRIANGLE REQUIRED)
find_package(GLFW REQUIRED)

find_package(GLEW REQUIRED)

set(CMAKE_CXX_LINK_EXECUTABLE "${CMAKE_CXX_LINK_EXECUTABLE} -lGL -lGLU -lrt -lX11 -lXxf86vm -lXrandr -lpthread -lXi  -lXcursor -lXinerama ")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")

include_directories( ${ANT_TWEAK_BAR_INCLUDE_DIR} )
include_directories( ${TRIANGLE_INCLUDE_DIR})
include_directories( ${GLEW_INCLUDE_DIR} )
include_directories( ${EIGEN_INCLUDE_DIR})
include_directories( ${LIBIGL_INCLUDE_DIRS})
include_directories( ${GLFW_INCLUDE_DIR})
include_directories(
	/usr/local/include
	/opt/local/include
    ${PROJECT_SOURCE_DIR}
    ${OPENGL_INCLUDE_DIR}
)

link_directories(
	/usr/local/lib
	/opt/local/lib
	${ANT_TWEAK_BAR_DIRS}
	${EIGEN_DIRS}
)

# Disable deprecated opengl code from libigl
add_definitions(-DIGL_OPENGL_4)

IF(CMAKE_BUILD_TYPE MATCHES RELEASE)
    add_definitions(-DNDEBUG)
ENDIF(CMAKE_BUILD_TYPE MATCHES RELEASE)

if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang" OR "${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU")
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-deprecated-register")
endif()

set(SHARED_SOURCES ${LIBIGL_SOURCES})

#set(SHARED_SOURCES ${SHARED_SOURCES} ${GLEW_SOURCES})
set(SHARED_LIBRARIES
		${LIBIGL_LIBRARIES}
		${OPENGL_LIBRARIES}
		${ANT_TWEAK_BAR_LIBRARY}
		${EIGEN_LIBRARIES}
		${GLEW_LIBRARIES}
		${GLFW_LIBRARIES}
)
find_package(LIM REQUIRED)

include_directories(${LIM_INCLUDE_DIR})

set(SOURCES
${PROJECT_SOURCE_DIR}/Main.cpp
)

#add_executable(${PROJECT_NAME}_bin ${SOURCES} ${SHARED_SOURCES} ${LIM_SOURCES} ${TRIANGLE_SOURCES})
add_executable(${PROJECT_NAME}_bin ${SOURCES} ${SHARED_SOURCES} ${TRIANGLE_SOURCES} ${LIM_SOURCES})
target_link_libraries(${PROJECT_NAME}_bin ${SHARED_LIBRARIES})
