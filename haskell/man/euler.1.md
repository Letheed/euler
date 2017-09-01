# EULER 1 "2016-12-30" "rgol 0.1.0.0" "User Commands"

## NAME
euler - run solutions to Project Euler problems in Haskell

## SYNOPSIS
`euler` [`exec`] [*OPTION*]... [*PROBLEMS*]...  
`euler` `text` [*PROBLEMS*]...

## DESCRIPTION

`exec`
  Run *PROBLEMS* and for each one print the execution time(s), the error status and the answer message. A summary is printed at the end with the number of problems solved, the summed execution time(s) and the conjunctive sum of error statuses.
  This is not a benchmark (no repeated runs). Problems are run in the order they are specified.

`test`
  Run *PROBLEMS* and check their error status. Print the number and proportion of passed problems (and failed problems if any). For problems that failed, print the right answer, the current result, and the message.
  Useful to verify that you did not introduce any regression.

## OPTIONS

`-t`, `--time`[=*TIMER*[,...]]
  Display the execution time using the specified *TIMER*(s). The default behavior for the program is `--time`=thread. If *TIMER* is not specified, “thread” is assumed. A special value “none” is accepted, where execution times are not displayed.

`-s`, `--sort`[=*TIMER*]
  Sort displayed problems by decreasing execution time. If *TIMER* is not specified, the first *TIMER* in `--time` is used, or “thread” if `--time`=none.

## SYNTAX

*PROBLEMS*
  Can be specified using any combination of the following number enumerations, separated by spaces: *X*, *X*..*Y* and *X*,*Y*..*Z*. If left unspecified, all available problems are selected by default.

*TIMER*
  Can take the following values:
  – “thread” for thread CPU time.
  – “proc” for process CPU time.

## PROBLEM ERROR STATUS
“Ok” or “Err” if the correct answer to a problem is known, “---” otherwise.

## EXAMPLES
Run problems 1, 5 and 7:

  euler 1 5 7

Test every other problem from 10 to 20:

  euler test 10,12..20

Run problems 1 through 60 and sort them by decreasing execution time:

  euler 1..60 --sort

Run problems 1, 5 and every even problem from 30 up to and including 60. Display the thread and process CPU execution times and sort the results by decreasing thread CPU time:

  euler 1 5 30,32..60 --time=thread,proc --sort

## AUTHOR
[Letheed][email]

## SOURCE
[Repository][repo]  
[Bug tracker][issues]

## COPYRIGHT
Copyright © 2016 Letheed. License BSD3.

[email]: mailto:letheed@outlook.com
[repo]: https://github.com/letheed/euler/tree/master/haskell
[issues]: https://github.com/letheed/euler/issues
