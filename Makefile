LATEX_OUT = report/*.out report/*.aux report/*.log report/*.synctex.gz report/*.toc
ERL_OUT = apps/environment/ebin/*.beam apps/vehicle/ebin/*.beam test/ebin/*.beam

.PHONY: apps
apps: environment vehicle

.PHONY: all
all: apps
	@printf "\nCompile test suite:\n"
	@cd test/ && erl -make
	@printf "\nMake test scripts executable:\n"
	chmod +x test/start_environment.sh
	chmod +x test/start_vehicle.sh
	chmod +x test/start_generator.sh

.PHONY: environment
environment:
	@printf "\nCompile Environment:\n"
	@cd apps/environment/ && erl -make

.PHONY: vehicle
vehicle:
	@printf "\nCompile Vehicle:\n"
	@cd apps/vehicle/ && erl -make

.PHONY: demo
demo: all
	@printf "\nDEMO:"
	@printf "\n- vehicles generated: 42"
	@printf "\n- failure ratio: 20%%"
	@printf "\n- failures will be 50%% software and 50%% mechanical"
	@printf "\n- failures will be triggered before 20s from vehicle start"
	@printf "\n\nStop with ctrl+C and then (a)bort\n"
	@./test/start_generator.sh 42 0.2 0.5 20000

.PHONY: clean
clean:
	rm -rf $(ERL_OUT) *.dump log

.PHONY: distclean
distclean:
	rm -rf $(LATEX_OUT) $(ERL_OUT) *.dump log