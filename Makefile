OUTPUT=mmult
BIN_PATH=bin
CC=gfortran

# just execute the project using some sample data after building the project
run: all
	./$(BIN_PATH)/$(OUTPUT)

# build all binaries
all: prepare mmult.f90
	$(CC) $(OUTPUT).f90 -o $(BIN_PATH)/$(OUTPUT)

# prepare the environment for project building
prepare: clean
	mkdir $(BIN_PATH)

# removes all object and binary files
clean:
	rm -Rf $(BIN_PATH)
