# Step by step tutorial

We go through the necessary steps to design and perform benchmarks.
If you want to try these steps out by yourself, a full [example project](https://github.com/ptal/kobe/tree/master/example) is provided.
Here are the steps explained in this section:

1. [Gather data sets](benchmarking-tutorial.html#a1-gather-data-sets)
2. [Create the benchmark configuration file](benchmarking-tutorial.html#a2-create-the-benchmark-configuration-file)
3. [Generate bench instances](benchmarking-tutorial.html#a3-generate-bench-instances)
4. [Perform the benchmark](benchmarking-tutorial.html#a4-perform-the-benchmark)
5. [View and analyse the results](benchmarking-tutorial.html#a5-view-and-analyse-the-results)

### 1. Gather data sets

Data sets must be contained in a directory with a specific structures, for example:

```
data
  rcpsp
    j30.sm
      optimum/optimum.csv
      j301_1.sm
      j301_2.sm
    patterson.rcp
      optimum/optimum.csv
      pat1.rcp
      pat2.rcp
  rcpsp-max
    sm_j10
      optimum/optimum.csv
      PSP1.SCH
      PSP2.SCH
      PSP3.SCH
```

In this _data directory_, we have two _families of problems_ namely `rcpsp/` and `rcpsp-max/`.
A _problem_ is a directory containing one or more data sets of the same underlying problem.
A _data set_ is a directory containing two things:

1. A file `optimum/optimum.csv` or `solution/solution.csv` listing the name of each data file along with their optimum values (in case of an optimization problem) or their satisfiability (in case of a satisfaction problem).
2. A list of _data instance_ files, for example `j301_1.sm`, `j301_2.sm`,...

Once we have these data, we can proceed to the description of our benchmarks.
Sample data are available in the directory [data](https://github.com/ptal/kobe/tree/master/example/data).

### 2. Create the benchmark configuration file

The benchmarks to perform are described in a single JSON configuration file.
For the sake of example, we will benchmark with AbSolute because it is already available to us (for GeCode and Chuffed, see [MiniZinc Benchmarking](minizinc-benchmarking.html)).
Let's have a look at the configuration file [example/scheduling-data.json](https://github.com/ptal/kobe/blob/master/example/scheduling-benchmark.json):

```json
{
  "bench_exec": "./exec.sh",
  "input_dir" : "data/",
  "output_dir" : "database/",
  "problem_sets" : [
    { "path": "rcpsp/patterson.rcp/",
      "timeout": 10 },
    { "path": "rcpsp-max/sm_j10/",
      "timeout": 10 }
  ],
  "solvers_kind": [
    <AbSolute: {
      "domains": [
        { "name": "Octagon", "strategies": ["MSLF_simple", "Max_min_LB", "Min_max_LB"] },
        { "name": "Box", "strategies": ["First_fail_LB", "MSLF_simple"] }
      ]
    }>],
  "csv" : {
    "fields" : [<ProblemName>, <Nodes>, <Solutions>, <Fails>, <Time : <Sec>>, <Optimum>]
  },
  "solvers_config": [
    { "name": "absolute",
      "version": "v0.4.0",
      "exec": "",
      "globals": "" }
  ]
}
```

*Warning*: be careful of trailing comma in lists such as at the end of `"MSLF_simple"] }`, it generates errors when reading the file.

Here a description of each entry:

* `bench_exec`: see [next section](benchmarking-tutorial.html#a3-generate-bench-instances)
* `input_dir`: Directory containing all the data sets for each family of problem.
* `output_dir`: Directory of the result database: where should the results be stored? This directory is created automatically if it does not yet exist.
* `problem_sets` is the list of every data set, where:
  * `path` is a path of the form `<family>/<data-sets>` where `family` must be supported by `kobe` (see [Sheet cheats](benchmarking.html#sheet-cheats)).
    The data sets are contained in `input_dir/path`.
  * `timeout` is the maximal number of _seconds_ for solving a data instance before it is stopped.
* `solvers_kind` contains the configuration of each solver that we must bench.
  * `AbSolute`: Every AbSolute configuration requires two things: a domain and its associated search strategy (see [Sheet cheats](benchmarking.html#sheet-cheats)).
* `csv` lists the entries that must appear in the resulting file, we explain these [below](benchmarking-tutorial.html#a4-perform-the-benchmark).
* `solvers_config` is the list of the solvers needed with several information.
  * `name` and `version` are used in the name of the produced files.
  * Note that kobe does not checked if the `version` of AbSolute matches the currently installed OPAM version.

You can adapt each entry depending on your needs, please see the section [Sheet cheats](benchmarking.html#sheet-cheats) for supported values.

### 3. Generate bench instances

Once we have the benchmarking configuration file ready, we prepare the benchmarking session by generating one file per benchmark instance.

```sh
$ kobegen scheduling-benchmark.json
10 bench files generated.
./exec.sh database/rcpsp/patterson.rcp/absolute-v0.4.0/Octagon-MSLF_simple.json database/rcpsp/patterson.rcp/absolute-v0.4.0/Octagon-MSLF_simple.csv
./exec.sh database/rcpsp/patterson.rcp/absolute-v0.4.0/Octagon-Max_min_LB.json database/rcpsp/patterson.rcp/absolute-v0.4.0/Octagon-Max_min_LB.csvbenchmarking-tutorial.html#a4-perform-the-benchmark
./exec.sh database/rcpsp/patterson.rcp/absolute-v0.4.0/Octagon-Min_max_LB.json database/rcpsp/patterson.rcp/absolute-v0.4.0/Octagon-Min_max_LB.csv
./exec.sh database/rcpsp/patterson.rcp/absolute-v0.4.0/Box-First_fail_LB.json database/rcpsp/patterson.rcp/absolute-v0.4.0/Box-First_fail_LB.csv
./exec.sh database/rcpsp/patterson.rcp/absolute-v0.4.0/Box-MSLF_simple.json database/rcpsp/patterson.rcp/absolute-v0.4.0/Box-MSLF_simple.csv
./exec.sh database/rcpsp-max/sm_j10/absolute-v0.4.0/Octagon-MSLF_simple.json database/rcpsp-max/sm_j10/absolute-v0.4.0/Octagon-MSLF_simple.csv
./exec.sh database/rcpsp-max/sm_j10/absolute-v0.4.0/Octagon-Max_min_LB.json database/rcpsp-max/sm_j10/absolute-v0.4.0/Octagon-Max_min_LB.csv
./exec.sh database/rcpsp-max/sm_j10/absolute-v0.4.0/Octagon-Min_max_LB.json database/rcpsp-max/sm_j10/absolute-v0.4.0/Octagon-Min_max_LB.csv
./exec.sh database/rcpsp-max/sm_j10/absolute-v0.4.0/Box-First_fail_LB.json database/rcpsp-max/sm_j10/absolute-v0.4.0/Box-First_fail_LB.csv
./exec.sh database/rcpsp-max/sm_j10/absolute-v0.4.0/Box-MSLF_simple.json database/rcpsp-max/sm_j10/absolute-v0.4.0/Box-MSLF_simple.csv
```

Basically, `kobegen` flattens the configuration file into many benchmark instance files, one per possible solver and data sets configuration.
These files are stored in the `output_dir` directory.
For conveniency, `kobegen` outputs a list of the benchmarking commands to execute next, for example:
```sh
./exec.sh database/rcpsp/patterson.rcp/absolute-v0.4.0/Octagon-MSLF_simple.json database/rcpsp/patterson.rcp/absolute-v0.4.0/Octagon-MSLF_simple.csv
```

`./exec.sh` is a user-defined script specified in the field `bench_exec` of the configuration file, and it takes two arguments:
  * `.../Octagon-MSLF_simple.json` which is the flatten configuration file.
  * `.../Octagon-MSLF_simple.csv` which is the name of the result (this file is not yet created since the benches have not yet been performed).
  * These two files are together in the database so we know exactly on what configuration a benchmark data set has been solved.

`kobegen` *does not execute* `exec.sh`, it just outputs the commands to execute.
It is particularly useful for launching benchmarks in parallel on a server, `exec.sh` can be used to launch a job.

Here `exec.sh` contains the single command `kobe $1 | tee $2` where `kobe` is the program explained next.

### 4. Perform the benchmark

Almost everything is settled, only the commands output by `kobegen` need to be run.
The results are stored in a CSV format which looks as follows:

```csv
problem, nodes, solutions, fails, time, optimum
[Warning] subdirectory data/rcpsp-max/sm_j10/optimum ignored.
PSP1.SCH, 85, 4, 39, 0.10s, 26
PSP2.SCH, 1, 0, 1, 0.01s, unsat
PSP3.SCH, 5107, 6, 2546, timeout, 45
```

Note that the warning is only output on the terminal and not in the CSV result file.

The field `csv` specified in the configuration file is directly reflected on the information printed here.
Here is some additional information on this output format:
  * The number of `Nodes`, `Solutions` and `Fails`.
  * The solving time in second or `timeout`.
  * `optimum` contains either the latest optimum value found, `none` if no optimum value was found before the timeout, or `unsat` if the problem was proven unsatisfiable.

Note that a combination of `timeout` with an optimum value means that the solver could not perform an optimality proof.

The benchmarking results can be shared by creating a pull request on the [kobe-database](https://github.com/ptal/kobe-database) repository.
You can also explore the results that are already there!

### 5. View and analyse the results

In the simple example of this tutorial, we already have 10 different files which can be cumbersome to examine by hand.
We provide `kobeview` to view and analyse the results stored in the database directory.

```sh
kobeview database > view_database.json
```

Then, you can go to the [kobe analysis page](kobe.html) or open the local `src/kobe-view/viewer/kobe.html` page, and load the file `view_database.json` into the website.
Note that all analysis are done locally in Javascript, so this website can be used offline.
You can click on "Time step" to have several graphs on our benchmarks ("instance inclusion" and "cactus plot" only work when there are several solvers, this should be fixed).

A limited number of analysis are available currently, but you can participate by contributing to kobeview, or create your own program since the database contains only CSV and JSON files.

## Supporting a new dataset

In case `kobe` does not support your problem, new contributions are welcomed!
We describe here the basic steps to add a new format and problem specification to kobe, which involves modifying its code.
To illustrate the necessary steps, we will pretend we add benchmarking support for the jobshop problem:

### Formatting the data set

Format the data instances of your problem (see also "Gather data sets" section above).
We have one github repository per problem's family (e.g. [scheduling](github.com/ptal/kobe-rcpsp) or [sat](github.com/ptal/kobe-sat)).
The data in these data sets are usually classified according to the origin of the instances (e.g. "Taillard instances") and the problem's kind (e.g. RCPSP or RCPSP/max).
Each data directory contains the set of instances, and an additional directory named either `solution` for satisfiability problems or `optimum` for optimization problems.

### Parsing the data

The first step is to parse the data instances into an OCaml structure specific to your problem.
To achieve that, in `kobe/parsers`, select the sub-directory according to the category of your problem or create one, and create `jobshop_jss.ml` and `jobshop_data.ml` where the first is the parser and the second the data structure parsed.
Usually, the format follows a similar pattern so you can have a look in existing files to get started.
Don't forget to also create the corresponding `.mli` and to add some comments in the code and in this manual (e.g. [Scheduling data sets](scheduling-data.html)).
Please cite your sources (papers and origin of data).

In the file `dispatch.ml` add a variant to `problem` (e.g. `JOBSHOP of jobshop`), and redirect the file to the right parser in the function `dispatch` according to the file extension.

### Create the model

Once we parsed our data into a suited structure, it is time to create the constraints model.
We create a file `models/jobshop.ml` with a single function `formula_of_jobshop: jobshop -> bab_qformula`.
`bab_qformula` is a representation of a formula where a value must be minimized or maximized.
It also works for satisfiability as well.
You can look to existing models for examples.
Similarly to `dispatch.ml` in the previous section, you must register your new function in `models.ml`.

If you want to support MiniZinc model, you need to convert the data parsed in the previous section to a DZN file that will be feed to your MiniZinc model.
To achieve that, create a file `generators/jobshop2dzn.ml` (replace `jobshop` by the name of your problem), and register this function in `mzn.ml`.

### Running the new model

That should be it!
You can now try and run your new model with various solvers (GeCode, Chuffed, AbSolute).
Check if the obtained results are right with our analyzer tool `kobeview`.

