IDIR=include
ODIR=obj
SDIR=src
LDIR=lib

MAIN=main.cpp
BINARY=contact.out
LIBRARY=contact

LPATH=$(shell pwd)/$(LDIR)

CXX=g++
CXXFLAGS= --std=c++11 -Wall -g

DEPS=$(shell find $(IDIR) -name '*.h')
SOURCES=$(shell find $(SDIR) -name '*.cpp')
OBJECTS=$(addprefix $(ODIR)/,$(SOURCES:$(SDIR)/%.cpp=%.o))

$(ODIR)/%.o: $(SDIR)/%.cpp $(DEPS)
	mkdir -p $(ODIR)
	$(CXX) -c -o $@ $< $(CXXFLAGS)

static: $(OBJECTS)
	mkdir -p $(LDIR)
	ar rcs $(LDIR)/lib$(LIBRARY).a $(OBJECTS)
	$(CXX) $(MAIN) -L$(LDIR) -l$(LIBRARY) -o $(BINARY) $(CXXFLAGS) $(OPT)

shared: $(SOURCES)

	mkdir -p $(LDIR)
	$(CXX) -fPIC -shared $(SOURCES) -o $(LDIR)/lib$(LIBRARY).so $(CXXFLAGS)
	$(CXX) $(MAIN) -o $(BINARY) -L$(LDIR) -l$(LIBRARY) $(CXXFLAGS) $(OPT)

dynamic: $(SOURCES)
	mkdir -p $(LDIR)
	$(CXX) -fPIC -shared $(SOURCES) -o $(LDIR)/lib$(LIBRARY).so $(CXXFLAGS)
	$(CXX) $(MAIN) -o $(BINARY) -L$(LDIR) -l$(LIBRARY) -ldl $(CXXFLAGS) -D DYNAMIC $(OPT)

clean:
	rm -rf $(ODIR) $(LDIR) $(BINARY)

.PHONY: shared dynamic static clean
