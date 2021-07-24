Hi! 

8 full-time+ months of the creative vacation after almost 7 years of participating in weekly APIs releases of the active bank as 
Scala API developer made possible the initial commit to this repo on GitHub with the first working approximation of TMS & MFC ideas.

The further work is not for one person if ideas have the right to life.

So, welcome to discuss or continue the project if you are interested in its ideas and/or realisations, these or new similar one.

Please ask for help with any difficulties!

Some reasoning about participation follows.

#### Everyone's pleasure to participate (AKA Code Of Conduct)
Since we speak Scala with systems, let's follow the [Scala Code of Conduct](https://www.scala-lang.org/conduct/) 
while communicating with each other. The repository and posts may be moderated without warning. Only English is acceptable.

Just remember that this is the Home of the project and the common place for similar ideas and implementations. 
They (project & ideas) are not interested in our human existence and related comparisons in general. 
They want to be realised, to be bugs free, to be used & be helpful. :smile:

Actually, we can only enjoy by realizing their desires. Even just by asking unsure question. :smile:

#### Questions, ideas, suggestions, theory, bugs, discussions, etc.
All repo related moments (bugs, questionable functionality, code suggestions, doc corrections, etc.: anything that may lead 
to the clear task to be done) are in [Issues](https://github.com/SerhiyShamshetdinov/sugar-tms/issues). 

Questions and discussions of current & new ideas, ways to implement ones, restrictions and possible solutions, theory, etc. 
are in [Discussions](https://github.com/SerhiyShamshetdinov/sugar-tms/discussions).

First, please, try to find existing similar subject already reported or being discussed.   

For bugs reporting the good idea is to give the test code that fails with the "got" and "expected" results. 
It then may be added to project tests to see the fix is done. 

Debug output also may help and be easily done in the worksheet or REPL by passing "Debug" option to `*For` macro call variants.   

#### Compatibility and versioning
While the project is in research & growing state, it will be good if it follows the full (binary & semantic) backward 
versions compatibility for all 0.x.x versions: only extensions and bug fixes in new versions, no drops or deprecations at all. 

Such strategy simplifies lots of things for users, requires only one primary branch for developing & support and allows us
to focus on new ideas and implementations rather than on maintaining old versions code. This also allows us to implement 
different ideas at the same time reusing the common code base.

For all new versions the above means:
- the usage should not require runtime artifact dependency (do not embed to macro output the calls to external resources, 
  embed as a code when required)
- semantic behaviour of old macros should not be changed (for the same input code the code produced be the macro should 
  not change semantically: monadic manipulation order, order of side effects, etc.), but may to overcome past limitations 
  extending the functionality that was previously errors
- any defaults should not be changed
- new functionality should be realized by new macro or by new option of existing macro
- new option for existing macro should have the default disabled state to repeat the old behaviour without code usage changes
- semantic bugs (i.e. ones that when fixed will change the macros output) should be done as new macros if already published

This should be verified by existing or new tests (that only expands). 

So, up the version:
- `patch` when bug fix is done or helper functionality is added/changed, the output equivalent code changes are made
- `minor` when new macro is added or old macro features are semantically extended (resets the `patch` to 0)

#### Branches, commits, pull requests, etc.: workflow 
The current model of the repo participating for project is based on forks of the repo. Its basics & workflow are kindly 
described in [Working with forks](https://docs.github.com/en/github/collaborating-with-pull-requests/working-with-forks) 
GitHub article.

- [fork this project repo](https://docs.github.com/en/get-started/quickstart/fork-a-repo) - you will have the full 
  write access to the forked (copied) repo in you GitHub account while it still is linked to the upstream (original) repo 
- find the issue number you want to do, for instance, `#5`. Please, do not do the work without corresponding issue.
  Issues always contain more details to better understand what was done and why for the future
- start a new branch from the `main` one. If to name the branch like `main-#5` then GitHub (at WEB interface) will create 
  2 links at the same time: to the branch page and to the issue page. It is suitable. Hyphen `-` is required to get 2 links 
- increment project version in the `build.sbt`. You may add `-SNAPSHOT` suffix to publish artifacts locally
- do the changes and add tests that verify the changed behavior or fixed bug
- update the readme.md accordingly to your changes
- after the work done commit changes. Start commit message with the issue number like `#5: corrects misprint`.
  GitHub will create a useful link to the issue wherever the commit message is displayed at the WEB interface
- before pushing the final work result, do sync with upstream (original) repo and rebase you branch on the `main` branch with 
  commits squashing to one (if you have several commits done), verify the current project version number and increment it 
  once more when required (if somebody did it merged prior to you while you was working)
- push the branch to your forked repo and create the pull-request to the upstream (original) repo. See 
  [Creating a pull request from a fork](https://docs.github.com/en/github/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/creating-a-pull-request-from-a-fork)

#### Code formatting
There are no special code formatting requirements in the project. 
It is now as simple as Scala has "by default" and as the default IDEA Scala plugin formatting rulers are.  

Please, keep in mind that code is written once, but read many times by many people, 
so formatting should be simple and suitable for as many *readers* as possible.

In any case, do not format on commit, please.

#### Just after the pleasure is got
Please, do not forget to:
- test changes: add new tests for new or extended functionality, do regression
- update in-repo documentation: explain new features or correct the old descriptions to the new behavior
- rebase on the `main` branch of the original repo (upstream repository) squashing your commits & make the pull-request to upstream
- when something useful found while working on issue: comment on issue or open new required ones  

#### How to test
CI tests will run on pushing to the only `main` branch and on creating PR to it.

But keep in mind that you have the limit of free minutes for automation on all your private projects. 
Full `sugar-tms` CI tests on push to `main` consumes 700 minutes (mostly due to macOS runs are
counted x10 of real minutes, x2 for Windows, x1 for Ubuntu). Public repo has no such limits. 
Real CI tests start-to-finish time (all in parallel) is about an hour at GitHub (due to 2.11).

So, do not push to your forked repo `main` or cancel CI tests (if started) just after pushing to save your free minutes.

In your forked project you should be able to run manual tests from Actions page on GitHub. Run by default on Ubuntu. 
Single Scala 2.13 test takes and consumes about 30 minutes.

To test locally run `sugar-tms/test` to run tests for default Scala version or run `+sugar-tms/test` to test with all supported Scala versions.

`build.sbt` contains description of additional test options in its header comments. 

To test under different JDK see `tools` folder in the root of the project.

#### How to publish locally
To publish artifacts for all Scala versions locally do `+sugar-tms/publishLocal`.

#### Afterword
Even if you think that the above has too many letters (except the Code Of Conduct :smile: ) - just start your doing the way you know.
Everything above is under discussion and can be tweaked for your suitable actions.

Meet you in PR, issue or discussion!
