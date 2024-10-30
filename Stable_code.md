
# Radioss Stable Code

This section describes how to access the stable version of OpenRadioss.
Review the [How to Contribute](Contributing.md) section to create a clone of OpenRadioss.

## Stable Code in OpenRadioss

* The stable code in OpenRadioss is set with a **git tag**.
* The tag is in the main branch of OpenRadioss.
* It is named

      latest-YYYYMMDD

## To Obtain Stable Version of the Code

### Download from Release Area

Source code can be downloaded from the [Releases area on GitHub](https://github.com/OpenRadioss/OpenRadioss/releases)

![image](/doc/stable_release.png)

### Obtain the Stable Release from Clone of OpenRadioss Repository

Command line calls permit gathering the tags from OpenRadioss:

#### On Linux in the Clone

To gather all tags from OpenRadioss:

      git fetch --tags origin main

To work on the latest stable version:

      export latest_tag=`git tag --sort=-version:refname |head -1`
      git checkout $latest_tag

#### On Windows in the Clone with CMD Commands

To gather all tags from OpenRadioss:

      git fetch --tags origin main
      git tag --sort=-version:refname > tags.txt
      set /p latest-tag=<tags.txt
      git checkout %latest-tag%

### Obtain the Stable Release from Your Fork of OpenRadioss Repository

Command line calls permit gathering the tags from OpenRadioss when the clone is from a fork:

#### On Linux in the Fork Clone

To gather all tags from OpenRadioss:

      git fetch --tags upstream main

To work on the latest stable version:

      export latest_tag=`git tag --sort=-version:refname |head -1`
      git checkout $latest_tag

#### On Windows in the Fork Clone with CMD Command

To gather all tags from OpenRadioss:

      git fetch --tags upstream main

To work on the latest stable version:

      git tag --sort=-version:refname > tags.txt
      set /p latest-tag=<tags.txt
      git checkout %latest-tag%