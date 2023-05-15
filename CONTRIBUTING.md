### Fork seacas

*   If you have not already done so, create a fork of seacas on GitHub under your username.
  *   Sign on (via web) to https://github.com/sandialabs/seacas
  *   Make sure you are signed in to github
  *   Click on the 'Fork' button near the top right of the page.
*   Clone your fork of seacas with
  *   `git clone git@github.com:<username>/seacas`.
  *   Or: `git clone https://github.com/<username>/seacas`
*   Each time you clone your fork,
  *   `git remote add upstream git@github.com:sandialabs/seacas` to add the original seacas repository as the `upstream` remote.
  *   Or: `git remote add upstream https://github.com/sandialabs/seacas'

### Update the Main Development Branch

To keep your `master` branch up-to-date with `upstream`:

*   `git fetch --all`
*   `git checkout master`
*   `git merge upstream/master`
*   `git push origin master`

You want to do this before starting work on a new feature branch.

### Create a Feature Branch

Create a local branch off of `master` on which to make your changes:

*   `git checkout master`
*   `git checkout -b <branchName>`

`<branchName>` can be whatever you like, though we have some recommendations:
*   Make the branch name descriptive; that is, avoid `fixSomeStuff`, `performanceTweaks`, and generic names along those lines.
*   To indicate your branch is intended solely for your own use, preface the branch name with your username, as in `<username>/<restOfBranchName>`.

### Make Your Changes

Do whatever work is necessary to address the issue you're tackling,
breaking your work into logical, compilable commits.  Feel free to
commit small chunks early and often in your local repository and then
use `git rebase -i` to reorganize your commits before sharing.  Make
sure the commit messages you will be sharing reference the appropriate
GitHub issue numbers.

### Update Your Branch

While working on your feature in your local `<branchName>` branch,
other commits will likely make it into the real seacas `master`
branch.  There are a variety of ways to merge these changes into your
local feature branch.  One possibility is

*   `git checkout <branchName>`
*   `git fetch --all`
*   `git merge upstream/master`

though there are others that are equally valid.

### Create a Pull Request

When your changes are ready to be integrated into seacas' `master` branch:

*   Push your local feature branch up to your fork with `git push -u origin <branchName>`.

*   Navigate to your fork of seacas on GitHub and create a new pull request:

  *   Be sure you choose:
    *   base fork:  `sandialabs/seacas`
    *   base:  `master`
    *   head fork:  `<username>/seacas`
    *   compare:  `<branchName>`

  *   On the new pull request creation page, you'll notice the *Description* field will be pre-populated with some text.  Follow the instructions in that template to give us as much information as you can such that we can review and approve the issue as soon as is practicable.

### Feedback

At this point you'll enter into a stage where you and various seacas
developers will iterate back and forth until your changes are in an
acceptable state and can be merged in.  If you need to make changes to
your pull request, make additional commits on your `<branchName>`
branch and push them up to your fork.  Make sure you don't delete your
remote feature branch or your fork of seacas before your pull request
has been merged.

### Acknowledgement
Based on the `CONTRIBUTING.md` document from Trilinos.