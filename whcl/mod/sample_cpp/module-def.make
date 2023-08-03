module.name := sample_cpp
module.obj := sample_cpp.o
#module.tests := test.whcl
module.LDFLAGS += -lstdc++
module.builtin := 0

$(module.sample_cpp.DIR)/sample_cpp.o: $(CWAL_CONVERT_GENERATED.HPP)

ifeq (1,$(ENABLE_CXX))
module.enabled := 1
endif
