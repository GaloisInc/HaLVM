ifneq ($(V),1)
Q         := @
quiet     := quiet_
else
Q         :=
quiet     :=
endif

echo-cmd = $(if $($(quiet)cmd_$1),echo '  $($(quiet)cmd_$1)';)
cmd      = @$(echo-cmd) $(cmd_$1)

label    = $(if $(findstring $(V),1),,@echo '  $1';)


quiet_cmd_cp = CP       $@
      cmd_cp = cp $< $@
