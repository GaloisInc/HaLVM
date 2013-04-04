ifneq ($(V),1)
Q         := @
quiet     := quiet_
MAKEFLAGS += -s
else
Q         :=
quiet     :=
endif

echo-cmd = $(if $($(quiet)cmd_$1),echo '  $($(quiet)cmd_$1)';)
cmd      = @$(echo-cmd) $(cmd_$1)


