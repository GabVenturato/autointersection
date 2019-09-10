LATEX_OUT = report/*.out report/*.aux report/*.log report/*.synctex.gz report/*.toc
ERL_OUT = apps/environment/ebin/*.beam apps/vehicle/ebin/*.beam test/ebin/*.beam

.PHONY: apps
apps: environment vehicle

.PHONY: all
all: apps
	@printf "\nCompile test suite:\n"
	@mkdir test/ebin/
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
	@printf "Logs availabe in \'log/\' directory\n"
	@./test/start_generator.sh 42 0.2 0.5 20000

.PHONY: docker
docker: all
	@printf "\nBuild docker:\n"
	docker build -t erlang-autointersection:1.0 .
	@printf "\nMake docker scripts executable:\n"
	chmod +x docker/start_docker.sh
	chmod +x docker/stop_docker.sh
	chmod +x docker/clean_docker.sh

.PHONY: dockerdemo
dockerdemo: docker
	@printf "\nDOCKER DEMO:"
	@printf "\n- vehicles generated: 10"
	@printf "\n- failure ratio: 20%%"
	@printf "\n- failures will be 50%% software and 50%% mechanical"
	@printf "\n- failures will be triggered before 20s from vehicle start"
	@printf "\n\nStop with ctrl+C and then (a)bort\n"
	@printf "Logs availabe in \'docker_log/\' directory\n\n"
	@./docker/start_docker.sh 10 0.2 0.5 20000

.PHONY: clean
clean:
	rm -rf $(ERL_OUT) *.dump log
	@chmod +x docker/stop_docker.sh
	@chmod +x docker/clean_docker.sh
	@printf "\nClean docker:\n"
	@./docker/clean_docker.sh

.PHONY: distclean
distclean:
	rm -rf $(LATEX_OUT) $(ERL_OUT) *.dump log
	@chmod +x docker/stop_docker.sh
	@chmod +x docker/clean_docker.sh
	@printf "\nClean docker:\n"
	@./docker/clean_docker.sh
