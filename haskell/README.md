:exclamation: This repository is under ongoing reorganization. Some things may be broken / messy.

------------------------------


# Project Euler in Haskell

Problems 1 to 120.

See the [problems](problems) folder for the complete set.

See the [lib](lib) folder, especially the `Math` and `LibCommon` modules and submodules for factorized code.


## Usage

`stack build && stack exec euler`

<pre>
SYNOPSIS
    euler [exec] [OPTION]... [PROBLEMS]...
    euler test [PROBLEMS]...

DESCRIPTION
    exec
        Run PROBLEMS and print for each one:
            - the execution time(s)
            - the error status
            - the answer message.
        A summary is printed at the end with:
            - the number of problems solved
            - the summed execution time(s)
            - the conjuctive sum of error statuses.
        This is not a benchmark (no repeated runs).
        Problems are run in the order they are specified.

    test
        Run PROBLEMS and check their error status.
        Print the number and proportion of passed / failed problems.
        For problems that failed, print the right answer, the current result, and the message.
        Useful to verify that you did not introduce any regression.

OPTIONS
    -t, --time[=TIMER[,...]]
        Display the execution time using the specified TIMER(s).
        The default behavior for the program is --time=thread.
        If TIMER is not specified, “thread” is assumed.
        A special value “none” is accepted, where execution times are not displayed.

    -s, --sort[=TIMER]
        Sort displayed problems by decreasing execution time.
        If TIMER is not specified, the first TIMER in --time is used, or “thread” if --time=none.

    TIMER
        TIMER can take the following values:
            - “thread” for thread CPU time
            - “proc” for process CPU time

PROBLEMS SYNTAX
    PROBLEMS can be specified using any combination of the following syntaxes, separated by spaces: X, X..Y and X,Y..Z.

PROBLEM ERROR STATUS
    “Ok” or “Err” if the right answer to a problem is known, “---” otherwise.

EXAMPLES
    Run problems 1, 5 and 7
        stack exec euler 1 5 7

    Test every other problem from 10 to 20
        stack exec euler test 10,12..20

    Run problems 1 through 60 and sort them by decreasing execution time
        stack exec euler 1..60 -- --sort
</pre>
