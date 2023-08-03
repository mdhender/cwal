MODULE.NAME := uuid
MODULE.OBJ := uuid.o whuuid.o
MODULE.TESTS := test.s2
#Grrrrrrrrr: gcc 7.3:
#/usr/include/features.h:184:3: error: #warning "_BSD_SOURCE and _SVID_SOURCE are deprecated, use _DEFAULT_SOURCE" [-Werror=cpp]
# # warning "_BSD_SOURCE and _SVID_SOURCE are deprecated, use _DEFAULT_SOURCE"
#   ^~~~~~~
MODULE.CFLAGS += -Wno-cpp
