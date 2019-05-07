
# cs600-610-project3

This repository contains the starter for project three in Computer Science 600
Fall 2018. The same repository will be used in CMPSC 610 in Spring 2019. The main directory of this repository contains the LaTeX source code
for a project that is designed for use with [GitHub
Classroom](https://classroom.github.com/). To learn more about the course in
which these assignments were completed, please visit the [Computer Science Senior Thesis Fall 2018 GitHub
Organization](https://github.com/Allegheny-CS-SeniorThesis-2018).

The LaTeX file in this repository is automatically compiled with [Travis
CI](https://travis-ci.org/), thus ensuring that it compiles correctly and,
moreover, that a PDF of the project proposal is available in your GitHub
repository whenever a commit is tagged for a release. Additionally, you can use
a LaTeX compilation command like `pdflatex` or `latexmk` to compile the provided
LaTeX file on your local workstation.

## Introduction

This assignment requires a researcher to write a LaTeX document, stored in the
file `AllegThesis.tex`, that includes a complete outline for your senior thesis
and the final version of two chapters of your senior thesis. Please see the
course syllabus and your first reader for more details about the expected
content of these two chapters.

If both your LaTeX source code and your writing meet all of the established
requirements, then you will see a green &#x2714; in the listing of commits in
GitHub. If your submission does not meet the requirements, a red &#x2717; will
appear instead. Your course  instructor will reduce a researcher's grade for
this assignment if the red &#x2717; appears on the last commit in GitHub
immediately before the assignment's due date on December 12, 2017 at 12:00
midnight.

Yet, if the green &#x2714; appears on the last commit in your GitHub repository,
then you satisfied all of the main checks, thereby allowing the course
instructors to further evaluate other aspects of your LaTeX source code and
writing, as described in the remainder of this assignment sheet. Unless you
provide the course instructors with documentation of the extenuating
circumstances that you are facing, no late work will be considered towards your
grade for this project.

## Learning

If you have not done so already, please read all of the relevant [GitHub
Guides](https://guides.github.com/) that explain how to use many of the features
that GitHub provides. In particular, please make sure that you have read the
following GitHub guides: [Mastering
Markdown](https://guides.github.com/features/mastering-markdown/), [Hello
World](https://guides.github.com/activities/hello-world/), and [Documenting Your
Projects on GitHub](https://guides.github.com/features/wikis/). Each of these
guides will help you to understand how to use both [GitHub](http://github.com) and
[GitHub Classroom](https://classroom.github.com/).

## Travis

This assignment uses [Travis CI](https://travis-ci.com/) to automatically run
the checking programs every time you commit to your GitHub repository. The
checking will start as soon as you have accepted the assignment, thus creating
your own private repository. If
you are using Travis for the first time, you will need to authorize Travis CI to
access the private repositories that you created on GitHub. You will partially
complete this authorization by following intuitive steps in your web browser.
You will also need to type the command `travis login --pro` in your terminal
window when you are in the root of your GitHub repository.

## Security

In order for Travis to automatically upload a PDF, called `AllegThesis.pdf`, to
GitHub when you tag the commit, you need to create your encrypted access token.
To complete this task you must type the command `travis setup releases --com --force`
in your GitHub repository for this assignment. Then, when prompted, please type
your username and password for GitHub. When asked to give the filename, you can
type `_build/AllegThesis.pdf`. When asked if you want to deploy
from a specific repository, you can respond with the answer of "no". Finally,
when this tool asks if you want to use encryption, please answer with a "yes".

Now, you should have a `.travis.yml` file with a secure access token for your
GitHub repository for this assignment. Use a text editor to edit this file and
place the following lines of code at the bottom of it.

```
  skip_cleanup: true
  on:
    tags: true
```

## Tagging

Since this repository primarily contains LaTeX source code, the Travis CI
configuration for it will compile the source code and automatically release a
PDF of the main file whenever the last commit is associated with a [Git
Tag](https://git-scm.com/book/en/v2/Git-Basics-Tagging). As such, this will
cause a PDF file to become available in the listing of the "Releases" listing
for this repository. All release numbers for your writing in this repository
should adhere to the [Semantic Versioning](http://semver.org/) standard that
all GitHub projects are asked to adopt.

Please note that the faculty members who read the PDF that is generated from the
LaTeX source code will only do so by downloading the "tagged" release of the
file `AllegThesis.pdf` that has a version number greater than 1.0.0. That is, if
your commit is tagged with `AllegThesis-obonhamcarter-1.0.0`, then the file
`AllegThesis.pdf` should be available for download in the "Releases" tab in your
GitHub repository for this project under the name `AllegThesis-obonhamcarter-1.0.0`.

Once you have finished making a single small change to the `AllegThesis.tex`,
you should commit your file using a `git commit` command. Now, to create your
first tag for this repository you could type `git tag
AllegThesis-obonhamcarter-0.1.0`. Of course, you should substitute your user name for
`obonhamcarter` when you create the tag. At this point, you are ready to push your
changes with the appropriate tag by typing the command `git push -u origin
master --tags`. After waiting for a period of time, you should see that your
GitHub repository features a new release of the document that you must create
for this project.

When you make subsequent changes to your files and perform commits and you are
ready to release a new version of `AllegThesis.pdf`, then you should again tag
your work &mdash; before running a push &mdash; with a tag that adheres to the
[Semantic Versioning](http://semver.org/) standard. Each time that you correctly
execute this sequence of commands you will release a new version of your
document to GitHub that is easily accessible as a PDF to you and to your first
and second readers. If you are unable to create a tagged release using the
automated system that Travis provides you can manually create one by using
GitHub's web interface; to adopt the manual approach please click the "Draft a
new release" button in the Releases tab of your GitHub repository.

## Updates

If a course instructor updates the provided material for this assignment and you
would like to receive these updates, then you can type this command in the main
directory for this assignment:

```
git remote add download git@github.com:Allegheny-CS-SeniorThesis-2018/cs600-610-project3-starter.git
```

You should only need to type this command once; typing the command additional
times may yield an error message but will not negatively influence the state of
your repository. Now, you are ready to download the updates provided by the
course instructor by typing:

```
git pull download master
```

This second command can be run whenever a course instructor needs to provide you
with new source code for this assignment. However, please note that, if you have
edited the files that the course instructor updated, running the previous
command may lead to Git merge conflicts. If this happens, you may need to
manually resolve them with the help of the instructor or a teaching assistant.

## Problems

If you have found a problem with this assignment's provided source code, then
you can go to the [Computer Science 600/610 Project 3
Starter](https://github.com/Allegheny-CS-SeniorThesis-2018/cs600-610-project3-starter)
repository and create an issue by clicking the "Issues" tab and then clicking
the green "New Issue" button. To ensure that your issue is properly resolved,
please provide as many details as is possible about the problem that you
experienced.

Please note that these assignment sheets have been developed and tested on an
Ubuntu 16.04 workstation running a recent version of LaTeX that was manually
installed using the TeXLive installer. It is also worth noting that you can
compile the `AllegThesis.tex` file using LaTeX development tools
such as `latexmk` or `pdflatex`. If you are unable to compile this file with
your development tools and your execution environment, then please open a new
issue and we will attempt to resolve your concerns.


## Assistance

If you are having trouble completing any part of this project, then please talk
with your first reader. Alternatively, you may ask questions in the Slack
team for this course.
