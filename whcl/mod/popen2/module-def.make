module.obj := mod_popen2.o
module.tests := test.whcl

ifeq (11,$(HAVE_POPEN)$(HAVE_FORK))
  module.enabled := 1
  module.builtin := 1
endif
