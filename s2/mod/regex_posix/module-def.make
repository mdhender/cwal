MODULE.NAME := regex_posix
MODULE.OBJ := mod_porex.o
MODULE.TESTS := test.s2

$(MODULE.regex_posix.DIR)/s2rx_common_static.c:
$(MODULE.regex_posix.DIR)/mod_porex.o $(MODULE.regex_posix.DIR)/static_module.o: \
  $(MODULE.regex_posix.DIR)/s2rx_common_static.c
$(MODULE.regex_posix.DIR)/mod_porex.o $(MODULE.regex_posix.DIR)/static_module.o: \
  CPPFLAGS+=-DREGEX_MAX_CAPTURES=$(S2_REGEX_CAPTURE_LIMIT)
