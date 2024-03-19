# Define compiler
GHC=ghc

# Define source files
SOURCES=Main.hs

# Define the executable name
EXECUTABLE=interpreter

# Default target: build the project
all: $(EXECUTABLE)

# Rule to build the executable
$(EXECUTABLE): $(SOURCES)
	$(GHC) -o $@ $^

# Rule to clean up build artifacts
clean:
	rm -f $(EXECUTABLE) *.hi *.o