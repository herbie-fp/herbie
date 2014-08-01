OBJECTS = main.o state.o
CXXFLAGS = -O2 -Wall
LDFLAGS = -lgsl -lgslcblas
DEPS = -I /usr/include/eigen3

infer : $(OBJECTS)
	$(CXX) $(CXXFLAGS) -o infer $(OBJECTS) $(LDFLAGS) $(DEPS)

main.o : state.hpp
