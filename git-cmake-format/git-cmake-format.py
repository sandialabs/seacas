#!/usr/bin/python


import os
import subprocess
import sys


Git='git'
ClangFormat='clang-format'
Style='-style=file'
IgnoreList=[]


def getGitHead():
    RevParse = subprocess.Popen([Git, 'rev-parse', '--verify', 'HEAD'],
        stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    RevParse.communicate()
    if RevParse.returncode:
        return '4b825dc642cb6eb9a060e54bf8d69288fbee4904'
    else:
        return 'HEAD'

def getGitRoot():
    RevParse = subprocess.Popen([Git, 'rev-parse', '--show-toplevel'],
        stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    return RevParse.stdout.read().strip()

def getEditedFiles(InPlace):
    Head = getGitHead()
    GitArgs = [Git, 'diff-index']
    if not InPlace:
        GitArgs.append('--cached')
    GitArgs.extend(['--diff-filter=ACMR', '--name-only', Head])
    DiffIndex = subprocess.Popen(GitArgs, stdout=subprocess.PIPE)
    DiffIndexRet = DiffIndex.stdout.read().strip()
    DiffIndexRet = DiffIndexRet.decode()

    return DiffIndexRet.split('\n')

def isFormattable(File):
    for Dir in IgnoreList:
        if '' != Dir and '' != os.path.commonprefix([os.path.relpath(File), os.path.relpath(Dir)]):
            return False
    Extension = os.path.splitext(File)[1]
    for Ext in ['.h', '.cpp', '.hpp', '.c', '.cc', '.hh', '.cxx', '.hxx']:
        if Ext == Extension:
            return True
    return False


def formatFile(FileName, GitRoot):
    subprocess.Popen([ClangFormat, Style, '-i', os.path.join(GitRoot,FileName)])
    return


def requiresFormat(FileName):
    GitShowRet = subprocess.Popen([Git, "show", ":" + FileName],
            stdout=subprocess.PIPE)
    ClangFormatRet = subprocess.Popen(
            [ClangFormat, Style], stdin=GitShowRet.stdout, stdout=subprocess.PIPE)
    FormattedContent = ClangFormatRet.stdout.read()


    FileContent = subprocess.Popen([Git, "show", ":" + FileName],
            stdout=subprocess.PIPE).stdout.read()

    if FormattedContent == FileContent:
        return False
    return True


def printUsageAndExit():
    print("Usage: " + sys.argv[0] + " [--pre-commit|--cmake] " +
          "[<path/to/git>] [<path/to/clang-format]")
    sys.exit(1)


if __name__ == "__main__":
    if 2 > len(sys.argv):
        printUsageAndExit()

    if "--pre-commit" == sys.argv[1]:
        InPlace = False
    elif "--cmake" == sys.argv[1]:
        InPlace = True
    else:
        printUsageAndExit()

    for arg in sys.argv[2:]:
        if "git" in arg:
            Git = arg
        elif "clang-format" in arg:
            ClangFormat = arg
        elif "-style=" in arg:
            Style = arg
        elif "-ignore=" in arg:
            IgnoreList = arg.strip("-ignore=").split(";")
        else:
            printUsageAndExit()

    EditedFiles = getEditedFiles(InPlace)

    ReturnCode = 0

    if InPlace:
        GitRoot = getGitRoot()
        for FileName in EditedFiles:
            if isFormattable(FileName):
                formatFile(FileName,GitRoot)
        sys.exit(ReturnCode)

    for FileName in EditedFiles:
        if not isFormattable(FileName):
            continue
        if requiresFormat(FileName):
            print("'" + FileName +
                  "' must be formatted, run the cmake target 'format'")
            ReturnCode = 1

    if 1 == ReturnCode:
        subprocess.Popen([Git, "reset", "HEAD", "--", "."])

    sys.exit(ReturnCode)
