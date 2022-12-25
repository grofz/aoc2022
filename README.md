# aoc2022
Solving Advent of Code 2022 in Fortran

01. Calorie Counting
    - making partial sums of items in the list
    - using `maxval` and `maxloc` functions 
02. Rock Paper Scissors
    - functions transforming moves to the game outcome
03. Rucksack Reorganization
    - search for a repeated letter in strings
    - function `scan` has been really useful here
04. Camp Cleanup
    - fully or partialy overlaping interval pairs 
05. Supply Stacks
    - stacks reorganization
    - using stack library modules `stack_mod` and `dll_mod`
06. Tuning Trouble
    - search for duplicities in the stream of characters
    - using trivial $O(N^2)$ comparison
07. No Space Left On Device
    - construct and query a file system
08. Treetop Tree House
    - 3D-arrays, test rays of visibility in a tree map
09. Rope Bridge
    - Simulate the movement of a rope
    - using parametrized derived type to differentiate between P1 and P2
10. Cathode-Ray Tube
    - simulate a graphic display
11. Monkey in the Middle
    - moving object among monkeys
12. Hill Climbing Algorithm
    - search path to the top of the hill
    - used Djikstra's search (not very original)
13. Distress Signal
    - parse and compare expressions [1,[2,3],[]]
    - structure with an array of pointers
14. Regolith Reservoir
    - simulate flow of sand particles
    - just a character(1) array to store the situation
15. Beacon Exclusion Zone
    - partition 4M x 4M by diamond tiles
    - class for subtracting squares
    - but a more simple, geometrical solution exists
16. Proboscidea Volcanium
    - optimize the order of closing valves
    - recursive search / greedy algorithm to pick three best moves
17. Pyroclastic Flow
    - part 1 is a tetris simulation
    - part 2 requires a pattern recognition
18. Boiling Boulders
    - calculate are in 3D-matrix of pores/rock/water
    - using Fortran matrices and functions
    - TODO: visualize the rock structure
19. Not Enough Minerals
    - optimize the order of spending resources on building robots
    - recursive search / must not consider useless choices 
20. Grove Positioning System
    - "mixing" (moving elements in a circular list
    - basic circular list implementation
21. Monkey Math
    - build tree of mathematical expressions
    - collection of recursive functions to evaluate expression (Part 1) and inverse operations (Part 2)
22. Monkey Map
    - travel on cube surface
    - fold cube from unfolded input
    - maping from 2d to 3d coordinates to move between cube sides
23. Unstable Diffusion
    - game of life algorithm with a rotating set of rules
    - visualization?
24. BLizzard Basin
    - search path in a maze (2D + time)
    - Djikstra saves the day
25. Full of Hot Air
    - conversion between pentary numbers