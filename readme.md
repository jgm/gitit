How to build:
-------------

    git submodule init
    git submodule update
    qmake
    make
Depenedencies:
--------------
libgit2, which is a git external.

On windows, we are using a static build of libgit2.  automated building does not work yet.
zlib was packaged with libgit2.a for windows, because zlib is a PITA to get on windows.
