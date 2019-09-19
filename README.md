# autointersection
A distributed solution to let autonomous vehicles cross an intersection without 
relying on a central server.

## Requirements
1. OS: Ubuntu 18.04 LTS (or above), Fedora 30 (or above).
2. Erlang/OTP 21 or 22

Scripts won't run on a non Unix-like based OS while the Erlang code will.

## Intro
The idea is to solve the problem for a generic intersection. Autonomous vehicles can not rely on a central server, they have to cooperate with each other to cross the intersection by taking decisions which ensure a \emph{fair} and \emph{safe} policy. In particular there are two components: \emph{vehicles} and \emph{the environment}. The latter is necessary in this context in order to simulate sensors that are usually inside autonomous vehicles which allows them to interact with the environment (e.g. proximity sensors, GPS, cameras, etc\dots).
The system is \emph{fault tolerant}, but neither byzantine processes nor cybersecurity hazards are taken in consideration.

***A detailed description of the whole system with test cases and how to run them can be found in the report.pdf (report/repot.pdf)***

## Getting started

Clone (or download) the repository:
`git clone https://github.com/GabVenturato/autointersection.git`

Move to the repository location:
`cd path-to-repository`

Move into the repository root (all scripts must be run from the repo root!):
`cd autointersection`

Compile code and give execution permissions to scripts:
`make all`

Start the environment:
`./test/start_environment.sh`

Start a vehicle:
`./test/start_vehicle.sh 1 R_1 R_30`
The first parameter is the index of the vehicle, second and third are start and destination positions.
See figure 4.1 at page 20 in the report: report/report.pdf for more available positions.

## Automated vehicle generator

In order to spawn multiple vehicles at random locations run the following command (from the repository root, as always):
`./test/start_generator.sh <vehicle number> <fail ratio> [relaive sw fail ratio] [max fail timeout (ms)]`

Where:
1. *<vehicle number>* is mandatory and represents the number of vehicles that will be generated.
2. *<fail ratio>* is mandatory and represents the percentage (expressed as a number 0 <= x <= 1) of failures that will be generated.
3. *[relaive sw fail ratio]* is optional and represents the ratio of software failures caused by the generator. (e.g. if set to 0.5, half of the faults will be software and the other half will be mechanical).
4. *[max fail timeout (ms)]* represent the maximum time in milliseconds within which the fault will be caused, considering the moment when the vehicle starts. (e.g. if set to 20000, the vehicle can fail after a maximum of 20s from its start).

To stop the execution press **ctrl+C** and then **(a)bort**. The information is all printed in the same shell, so it is less clear as what is happening. The logs for each vehicle can be found in the **log/** directory.

Tests with Docker can also be performed, see Chapter 5 (Validation) in the report for more information.