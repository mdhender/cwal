MODULE.NAME := sample_cpp
MODULE.OBJ := sample_cpp.o
MODULE.TESTS := test.s2
MODULE.LDFLAGS += -lstdc++

$(MODULE.sample_cpp.DIR)/sample_cpp.o: $(CWAL_CONVERT_GENERATED.HPP)
