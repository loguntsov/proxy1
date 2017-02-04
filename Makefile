APP:=proxy1
SRC_PATH:= -pa _build/default/lib/*/ebin
ERL_RUN_ARGS:= $(SRC_PATH) +pc unicode -name $(APP)@`hostname` -s $(APP)_app main


.PHONY: all
all: compile

.PHONY: compile
compile:
	./rebar3 compile

.PHONY: run
run: compile
	erl $(ERL_RUN_ARGS)

kill:
	kill `cat sendmail_server.pid` || true

.PHONY: daemon
daemon: kill
	run_erl -daemon /tmp/ log/ "erl $(ERL_RUN_ARGS)"

