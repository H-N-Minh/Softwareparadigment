# SWP25 - AP Framework

Tested with JDK17 and SBT 1.10.10.

- Start SBT shell
sbt

While within the sbt shell:
- compile the whole project
compile

- To interpret a ÆXP program from the file test.exp
run file test.exp

- To evaluate individual expressions,
run evalexpr add(1, 2)

- run only the parser and display the generated syntax tree of a program
run parser test.exp

- To execute test cases
test


Some notes:
The directory src/test/scala/ contains some test cases for ÆXP and the code examples provided in this assignment
